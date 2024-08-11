/*
 * Copyright (c) 2022 Caleb Etemesi <etemesicaleb@gmail.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*
 * Copyright 2019 - 2021, Osamu Watanabe
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdint.h>
#include <stdalign.h>
#include "libavutil/attributes.h"
#include "libavutil/common.h"
#include "libavutil/avassert.h"
#include "libavutil/mem.h"
#include "jpeg2000htdec.h"
#include "jpeg2000.h"
#include "jpeg2000dec.h"

#define J2K_Q1 0
#define J2K_Q2 1

#define HT_SHIFT_SIGMA 0
#define HT_SHIFT_SCAN 4
#define HT_SHIFT_REF 3
#define HT_SHIFT_REF_IND 2

/* See Rec. ITU-T T.800, Table 2 */
const static uint8_t mel_e[13] = { 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5 };

static const uint16_t dec_cxt_vlc_table1[1024];
static const uint16_t dec_cxt_vlc_table0[1024];
static const uint16_t uvlc_dec_0[256 + 64];
static const uint16_t uvlc_dec_1[256];


typedef struct StateVars {
    int32_t pos;
    uint32_t bits;
    uint32_t tmp;
    uint32_t last;
    uint8_t bits_left;
    uint64_t bit_buf;
} StateVars;

typedef struct Dec_state {
    uint8_t *buf;
    uint64_t Creg;
    int32_t bits;
    uint32_t unstuff;
    int32_t length;
    void (*read)(struct Dec_state *);
    uint32_t (*fetch)(struct Dec_state *);
    uint32_t (*advance)(struct Dec_state *, int32_t);
} Dec_state;

enum Quads {
    Q0,
    Q1
};

static void jpeg2000_init_zero(StateVars *s)
{
    s->bits_left = 0;
    s->bit_buf   = 0;
    s->tmp       = 0;
    s->bits      = 0;
    s->pos       = 0;
    s->last      = 0;
}

static void jpeg2000_init_mag_ref(StateVars *s, uint32_t Lref)
{
    s->pos       = Lref - 1;
    s->bits      = 0;
    s->last      = 0xFF;
    s->tmp       = 0;
    s->bits_left = 0;
    s->bit_buf   = 0;
}


/**
 * Refill the buffer backwards in little endian while skipping over stuffing
 * bits. Stuffing bits are those that appear in the position of any byte whose
 * LSBs are all 1's if the last consumed byte was larger than 0x8F.
 */
static int jpeg2000_bitbuf_refill_backwards(StateVars *buffer, const uint8_t *array)
{
    uint64_t tmp = 0;
    uint32_t new_bits = 32;

    buffer->last = array[buffer->pos + 1];

    if (buffer->bits_left >= 32)
        return 0; // enough data, no need to pull in more bits

    /**
     *  Unstuff bits. Load a temporary byte, which precedes the position we
     *  currently at, to ensure that we can also un-stuff if the stuffed bit is
     *  the bottom most bits.
     */

    if (buffer->pos >= 3) {  // Common case; we have at least 4 bytes available
         tmp = array[buffer->pos - 3];
         tmp = (tmp << 8) | array[buffer->pos - 2];
         tmp = (tmp << 8) | array[buffer->pos - 1];
         tmp = (tmp << 8) | array[buffer->pos];
         tmp = (tmp << 8) | buffer->last;  // For stuffing bit detection
         buffer->pos -= 4;
    } else {
        if (buffer->pos >= 2)
            tmp = array[buffer->pos - 2];
        if (buffer->pos >= 1)
            tmp = (tmp << 8) | array[buffer->pos - 1];
        if (buffer->pos >= 0)
            tmp = (tmp << 8) | array[buffer->pos];
        buffer->pos = 0;
        tmp = (tmp << 8) | buffer->last;  // For stuffing bit detection
    }
    // Now remove any stuffing bits, shifting things down as we go
    if ((tmp & 0x7FFF000000) > 0x7F8F000000) {
        tmp &= 0x7FFFFFFFFF;
        new_bits--;
    }
    if ((tmp & 0x007FFF0000) > 0x007F8F0000) {
        tmp = (tmp & 0x007FFFFFFF) + ((tmp & 0xFF00000000) >> 1);
        new_bits--;
    }
    if ((tmp & 0x00007FFF00) > 0x00007F8F00) {
        tmp = (tmp & 0x00007FFFFF) + ((tmp & 0xFFFF000000) >> 1);
        new_bits--;
    }
    if ((tmp & 0x0000007FFF) > 0x0000007F8F) {
        tmp = (tmp & 0x0000007FFF) + ((tmp & 0xFFFFFF0000) >> 1);
        new_bits--;
    }
    tmp >>= 8;  // Shifts away the extra byte we imported

    /* Add bits to the MSB of the bit buffer */
    buffer->bit_buf |= tmp << buffer->bits_left;
    buffer->bits_left += new_bits;
    return 0;
}


/**
 * Drops bits from lower bits in the bit buffer. buf contains the bit buffers.
 * nbits is the number of bits to remove.
 */
av_always_inline
static void jpeg2000_bitbuf_drop_bits_lsb(StateVars *buf, uint8_t nbits)
{
    av_assert2(buf->bits_left >= nbits); // cannot read more bits than available
    buf->bit_buf >>= nbits;
    buf->bits_left -= nbits;
}

/**
 * Get bits from the bit buffer reading them from the least significant bits
 * moving to the most significant bits. In case there are fewer bits, refill
 * from buf moving backwards.
 */
av_always_inline
static uint64_t jpeg2000_bitbuf_get_bits_lsb(StateVars *bit_stream, uint8_t nbits,
                                             const uint8_t *buf)
{
    uint64_t bits;
    uint64_t mask = (1ull << nbits) - 1;
    if (bit_stream->bits_left < nbits)
        jpeg2000_bitbuf_refill_backwards(bit_stream, buf);
    bits = bit_stream->bit_buf & mask;
    jpeg2000_bitbuf_drop_bits_lsb(bit_stream, nbits);
    return bits;
}

av_always_inline
static int jpeg2000_peek_bit(StateVars *stream, const uint8_t *array, uint32_t length)
{
    uint8_t bit;

    if (stream->bits == 0) {
        stream->bits = (stream->last == 0xFF) ? 7 : 8;
        if (stream->pos < length) {
            stream->tmp = array[stream->pos];
            stream->pos++;
        } else {
            stream->tmp = 0;
        }
        stream->last = stream->tmp;
    }
    bit = stream->tmp & 1;
    stream->tmp >>= 1;
    stream->bits--;
    return  bit;
}

/**
 * Magref decoding procedures.
 */
av_always_inline
static int jpeg2000_import_magref_bit(StateVars *stream, const uint8_t *array,
                                      uint32_t length)
{
    return jpeg2000_bitbuf_get_bits_lsb(stream, 1, array);
}

static void MEL_init(Dec_state *st, int32_t *MEL_k, int32_t *num_runs, uint64_t *runs, const uint8_t *Dcup, int32_t Lcup, int32_t Scup) {
    st->buf = Dcup + Lcup - Scup;
    st->Creg = 0;
    st->bits = 0;
    st->length = Scup - 1;  // length is the length of MEL+VLC-1
    st->unstuff = 0;
    *MEL_k = 0;
    *num_runs = 0;
    *runs = 0;
    int32_t num = 4 - (int32_t)((intptr_t)(st->buf) & 0x3);
    for (int32_t i = 0; i < num; ++i) {
        uint64_t d = (st->length > 0) ? *(st->buf) : 0xFF;  // if buffer is exhausted, set data to 0xFF
        if (st->length == 1) {
            d |= 0xF;  // if this is MEL+VLC+1, set LSBs to 0xF (see the spec)
        }
        st->buf += st->length-- > 0;  // increment if the end is not reached
        int32_t d_bits = 8 - st->unstuff;
        st->Creg = (st->Creg << d_bits) | d;
        st->bits += d_bits;
        st->unstuff = ((d & 0xFF) == 0xFF);
    }
    st->Creg <<= (64 - st->bits);
}

static void MEL_read(Dec_state *st) {
    if (st->bits > 32) {  // there are enough bits in tmp, return without any reading
        return;
    }

    uint32_t val = 0xFFFFFFFF;  // feed in 0xFF if buffer is exhausted
    if (st->length > 4) {           // if there is data in the MEL segment
        val = *(uint32_t *)(st->buf);  // read 32 bits from MEL data
        st->buf += 4;                                                         // advance pointer
        st->length -= 4;                                                      // reduce counter
    } else if (st->length > 0) {
        // 4 or less
        int i = 0;
        while (st->length > 1) {
            uint32_t v = *st->buf++;                // read one byte at a time
            uint32_t m = ~(0xFFU << i);         // mask of location
            val = (val & m) | (v << i);  // put one byte in its correct location
            --st->length;
            i += 8;
        }
        // length equal to 1
        uint32_t v = *st->buf++;  // the one before the last is different
        v |= 0xF;             // MEL and VLC segments may be overlapped
        uint32_t m = ~(0xFFU << i);
        val        = (val & m) | (v << i);
        --st->length;
    } else {
        // error
    }

    // next we unstuff them before adding them to the buffer
    int32_t bits_local = 32 - st->unstuff;  // number of bits in val, subtract 1 if the previously read byte requires unstuffing

    // data is unstuffed and accumulated in t
    // bits_local has the number of bits in t
    uint32_t t = val & 0xFF;
    int32_t unstuff_flag = ((val & 0xFF) == 0xFF);
    bits_local -= unstuff_flag;
    t = t << (8 - unstuff_flag);

    t |= (val >> 8) & 0xFF;
    unstuff_flag = (((val >> 8) & 0xFF) == 0xFF);
    bits_local -= unstuff_flag;
    t = t << (8 - unstuff_flag);

    t |= (val >> 16) & 0xFF;
    unstuff_flag = (((val >> 16) & 0xFF) == 0xFF);
    bits_local -= unstuff_flag;
    t = t << (8 - unstuff_flag);

    t |= (val >> 24) & 0xFF;
    st->unstuff = (((val >> 24) & 0xFF) == 0xFF);

    // move to tmp, and push the result all the way up, so we read from the MSB
    st->Creg |= ((uint64_t)(t)) << (64 - bits_local - st->bits);
    st->bits += bits_local;
}

static void MEL_decode(Dec_state *st, int32_t *MEL_k, int32_t *num_runs, uint64_t *runs) {
    if (st->bits < 6) {  // if there are less than 6 bits in tmp then read from the MEL bitstream 6 bits that is
        // the largest decodable MEL codeword.
        MEL_read(st);
    }
    // repeat so long that there is enough decodable bits in tmp, and the runs store is not full
    // (num_runs < 8)
    while (st->bits >= 6 && *num_runs < 8) {
        int32_t eval = mel_e[*MEL_k];
        int32_t run  = 0;
        // The next bit to decode (stored in MSB)
        if (st->Creg & (1ULL << 63)) {
            // "1" is found
            run = 1 << eval;
            run--;                                        // consecutive runs of 0 events - 1
            *MEL_k = ((*MEL_k + 1) < 12) ? *MEL_k + 1 : 12;  // increment, max is 12
            st->Creg <<= 1;                                    // consume one bit from tmp
            st->bits--;
            run <<= 1;  // a stretch of zeros not terminating in one
        } else {
            // "0" is found
            run   = (int32_t)(st->Creg >> (63 - eval)) & ((1 << eval) - 1);
            *MEL_k = ((*MEL_k - 1) > 0) ? *MEL_k - 1 : 0;  // decrement, min is 0
            st->Creg <<= eval + 1;                           // consume eval + 1 bits (max is 6)
            st->bits -= eval + 1;
            run = (run << 1) + 1;  // a stretch of zeros terminating with one
        }
        eval = *num_runs * 7;                             // 7 bits per run
        *runs &= ~((uint64_t)(0x3F) << eval);  // 6 bits are sufficient
        *runs |= ((uint64_t)(run)) << eval;    // store the value in runs
        (*num_runs)++;                                      // increment count
    }
}

static int32_t MEL_get_run(Dec_state *st, int32_t *MEL_k, int32_t *num_runs, uint64_t *runs) {
    if (*num_runs == 0) {  // if no runs, decode more bit from MEL segment
        MEL_decode(st, MEL_k, num_runs, runs);
    }
    int32_t t = (int32_t)(*runs & 0x7F);  // retrieve one run
    *runs >>= 7;                                     // remove the retrieved run
    (*num_runs)--;
    return t;  // return run
}

av_always_inline
static void VLC_read(Dec_state *st) {
    // process 4 bytes at a time
    if (st->bits > 32) {
        // if there are already more than 32 bits, do nothing to prevent overflow of Creg
        return;
    }
    uint32_t val = 0;
    if (st->length > 3) {  // Common case; we have at least 4 bytes available
        val = *(uint32_t *)(st->buf - 3);
        st->buf -= 4;
        st->length -= 4;
    } else if (st->length > 0) {  // we have less than 4 bytes
        int i = 24;
        while (st->length > 0) {
            uint32_t v = *st->buf--;
            val |= (v << i);
            --st->length;
            i -= 8;
        }
    } else {
        // error
    }

    // accumulate in tmp, number of bits in tmp are stored in bits
    uint32_t tmp = val >> 24;  // start with the MSB byte
    int32_t bits_local;

    // test unstuff (previous byte is >0x8F), and this byte is 0x7F
    bits_local        = 8 - ((st->unstuff && (((val >> 24) & 0x7F) == 0x7F)) ? 1 : 0);
    int32_t unstuff_flag = (val >> 24) > 0x8F;  // this is for the next byte

    tmp |= ((val >> 16) & 0xFF) << bits_local;  // process the next byte
    bits_local += 8 - ((unstuff_flag && (((val >> 16) & 0x7F) == 0x7F)) ? 1 : 0);
    unstuff_flag = ((val >> 16) & 0xFF) > 0x8F;

    tmp |= ((val >> 8) & 0xFF) << bits_local;
    bits_local += 8 - ((unstuff_flag && (((val >> 8) & 0x7F) == 0x7F)) ? 1 : 0);
    unstuff_flag = ((val >> 8) & 0xFF) > 0x8F;

    tmp |= (val & 0xFF) << bits_local;
    bits_local += 8 - ((unstuff_flag && ((val & 0x7F) == 0x7F)) ? 1 : 0);
    unstuff_flag = (val & 0xFF) > 0x8F;

    // now move the read and unstuffed bits into this->Creg
    st->Creg |= (uint64_t)(tmp) << st->bits;
    st->bits += bits_local;
    st->unstuff = unstuff_flag;  // this for the next read
}

static uint32_t VLC_fetch(Dec_state *st) {
    if (st->bits < 32) {
        st->read(st);
        if (st->bits < 32) {
            st->read(st);
        }
    }
    return (uint32_t)st->Creg;
}

static uint32_t VLC_advance(Dec_state *st, int32_t num_bits) {
    if (num_bits > st->bits) {
        printf("ERROR: VLC require %d bits but there are %d bits left\n", num_bits, st->bits);
    }
    st->Creg >>= num_bits;
    st->bits -= num_bits;
    return (uint32_t)st->Creg;
}

static void VLC_init(Dec_state *st, uint8_t *Dcup, int32_t Lcup, int32_t Scup) {
    st->buf = Dcup + Lcup - 2;
    st->Creg = 0;
    st->bits = 0;
    st->length = Scup - 2;
    st->unstuff = 0;
    st->read = VLC_read;
    st->fetch = VLC_fetch;
    st->advance = VLC_advance;

    uint32_t d = *st->buf--;  // read a byte (only use it's half byte)
    st->Creg       = d >> 4;
    st->bits       = 4 - ((st->Creg & 0x07) == 0x07);
    st->unstuff    = (d | 0x0F) > 0x8f;

    int32_t p = (int32_t)((intptr_t)(st->buf) & 0x03);
//    p &= 0x03;
    int32_t num  = 1 + p;
    int32_t tnum = (num < st->length) ? num : st->length;
    for (int i = 0; i < tnum; ++i) {
        uint64_t d64;
        d64             = *st->buf--;
        uint32_t d_bits = 8 - ((st->unstuff && ((d64 & 0x7F) == 0x7F)) ? 1 : 0);
        st->Creg |= d64 << st->bits;
        st->bits += d_bits;
        st->unstuff = d64 > 0x8F;
    }
    st->length -= tnum;
    st->read(st);
}

av_always_inline
static void MagSgn_read(Dec_state *st) {
    if (st->bits > 32) {
        printf("ERROR: in MagSgn reading\n");
    }

    uint32_t val = 0;
    if (st->length > 3) {
        val = *(uint32_t *)(st->buf);
        st->buf += 4;
        st->length -= 4;
    } else if (st->length > 0) {
        int i = 0;
        val   = 0xFFFFFFFFU; // (X != 0) ? 0xFFFFFFFFU : 0;
        while (st->length > 0) {
            uint32_t v =  *(st->buf++);
            uint32_t m = ~(0xFFU << i);
            val        = (val & m) | (v << i);  // put one byte in its correct location
            --st->length;
            i += 8;
        }
    } else {
        val = 0xFFFFFFFFU; // (X != 0) ? 0xFFFFFFFFU : 0;
    }

    // we accumulate in t and keep a count of the number of bits_local in bits_local
    int32_t bits_local   = 8 - st->unstuff;
    uint32_t t            = val & 0xFF;
    int32_t unstuff_flag = ((val & 0xFF) == 0xFF);  // Do we need unstuffing next?

    t |= ((val >> 8) & 0xFF) << bits_local;
    bits_local += 8 - unstuff_flag;
    unstuff_flag = (((val >> 8) & 0xFF) == 0xFF);

    t |= ((val >> 16) & 0xFF) << bits_local;
    bits_local += 8 - unstuff_flag;
    unstuff_flag = (((val >> 16) & 0xFF) == 0xFF);

    t |= ((val >> 24) & 0xFF) << bits_local;
    bits_local += 8 - unstuff_flag;
    st->unstuff = (((val >> 24) & 0xFF) == 0xFF);  // for next byte

    st->Creg |= ((uint64_t)t) << st->bits;  // move data to this->tmp
    st->bits += bits_local;
}

av_always_inline
static uint32_t MagSgn_fetch(Dec_state *st) {
    if (st->bits < 32) {
        st->read(st);
        if (st->bits < 32)  // need to test
            st->read(st);
    }
    return (uint32_t)st->Creg;
}
av_always_inline
static uint32_t MagSgn_advance(Dec_state *st, int32_t n) {
    if (n > st->bits) {
        printf("ERROR: illegal attempt to advance %d bits but there are %d bits left in MagSgn advance\n", n,
               st->bits);
        return -1;
    }
    st->Creg >>= n;  // consume n bits
    st->bits -= n;
    return 0;
}


static void MagSgn_init(Dec_state *st, uint8_t *Dcup, int32_t Pcup) {
    st->buf = Dcup;
    st->Creg = 0;
    st->bits = 0;
    st->unstuff = 0;
    st->length = Pcup;

    st->read = MagSgn_read;
    st->fetch = MagSgn_fetch;
    st->advance = MagSgn_advance;
    // for alignment
    intptr_t p = (intptr_t)st->buf;
    p &= 0x03;
    int32_t num = 4 - p;
    for (int32_t i = 0; i < num; ++i) {
        uint64_t d;
        if (st->length-- > 0) {
            d = *st->buf++;
        } else {
            d = (uint64_t)0xFF; // X (0 or FF)
        }
        st->Creg |= (d << st->bits);
        st->bits += 8 - st->unstuff;
        st->unstuff = ((d & 0xFF) == 0xFF);  // bit-unstuffing for next byte
    }
    st->read(st);
}




static int jpeg2000_decode_ht_cleanup_segment(Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, const uint8_t *Dcup,
                                              uint32_t Lcup, uint32_t Pcup, uint8_t pLSB,
                                              const int stride,
                                              uint8_t *block_states)
{
    Dec_state mag_dec, mel_dec, vlc_dec;
    int32_t MEL_k, num_runs;
    uint64_t runs;
    const int32_t width = cblk->coord[0][1] - cblk->coord[0][0];
    const int32_t height = cblk->coord[1][1] - cblk->coord[1][0];
    const uint16_t QW = ff_jpeg2000_ceildiv(width, 2);
    const uint16_t QH = ff_jpeg2000_ceildiv(height, 2);
    int32_t sstr = (int32_t)(((width + 2) + 7u) & ~7u);  // multiples of 8
    uint16_t *sp;
    int32_t qx;

    MagSgn_init(&mag_dec, Dcup, Pcup);
    MEL_init(&mel_dec, &MEL_k, &num_runs, &runs, Dcup, Lcup, Lcup - Pcup);
    VLC_init(&vlc_dec, Dcup, Lcup, Lcup - Pcup);


    /*******************************************************************************************************************/
    // VLC, UVLC and MEL decoding
    /*******************************************************************************************************************/

    uint8_t *sp0 = block_states + 1 + stride;
    uint8_t *sp1 = block_states + 1 + 2 * stride;
    uint32_t u_off0, u_off1;
    uint32_t u0, u1;
    uint32_t context = 0;
    uint32_t vlcval;

    const uint16_t *dec_table;
    // Initial line-pair
    dec_table       = dec_cxt_vlc_table0;
    sp              = t1->flags;// scratch;
    int32_t mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
    for (qx = QW; qx > 0; qx -= 2, sp += 4) {
        // Decoding of significance and EMB patterns and unsigned residual offsets
        vlcval       = vlc_dec.fetch(&vlc_dec);
        uint16_t tv0 = dec_table[(vlcval & 0x7F) + context];
        if (context == 0) {
            mel_run -= 2;
            tv0 = (mel_run == -1) ? tv0 : 0;
            if (mel_run < 0) {
                mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
            }
        }
        sp[0] = tv0;

        // calculate context for the next quad, Eq. (1) in the spec
        context = ((tv0 & 0xE0U) << 2) | ((tv0 & 0x10U) << 3);  // = context << 7

        // Decoding of significance and EMB patterns and unsigned residual offsets
        vlcval       = vlc_dec.advance(&vlc_dec,(tv0 & 0x000F) >> 1);
        uint16_t tv1 = dec_table[(vlcval & 0x7F) + context];
        if (context == 0 && qx > 1) {
            mel_run -= 2;
            tv1 = (mel_run == -1) ? tv1 : 0;
            if (mel_run < 0) {
                mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
            }
        }
        tv1   = (qx > 1) ? tv1 : 0;
        sp[2] = tv1;

        // store sigma
        *sp0++ = ((tv0 >> 4) >> 0) & 1;
        *sp0++ = ((tv0 >> 4) >> 2) & 1;
        *sp0++ = ((tv1 >> 4) >> 0) & 1;
        *sp0++ = ((tv1 >> 4) >> 2) & 1;
        *sp1++ = ((tv0 >> 4) >> 1) & 1;
        *sp1++ = ((tv0 >> 4) >> 3) & 1;
        *sp1++ = ((tv1 >> 4) >> 1) & 1;
        *sp1++ = ((tv1 >> 4) >> 3) & 1;

        // calculate context for the next quad, Eq. (1) in the spec
        context = ((tv1 & 0xE0U) << 2) | ((tv1 & 0x10U) << 3);  // = context << 7

        vlcval = vlc_dec.advance(&vlc_dec,(tv1 & 0x000F) >> 1);
        u_off0 = tv0 & 1;
        u_off1 = tv1 & 1;

        uint32_t mel_offset = 0;
        if (u_off0 == 1 && u_off1 == 1) {
            mel_run -= 2;
            mel_offset = (mel_run == -1) ? 0x40 : 0;
            if (mel_run < 0) {
                mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
            }
        }

        // UVLC decoding
        uint32_t idx         = (vlcval & 0x3F) + (u_off0 << 6U) + (u_off1 << 7U) + mel_offset;
        uint32_t uvlc_result = uvlc_dec_0[idx];
        // remove total prefix length
        vlcval = vlc_dec.advance(&vlc_dec,uvlc_result & 0x7);
        uvlc_result >>= 3;
        // extract suffixes for quad 0 and 1
        uint32_t len = uvlc_result & 0xF;  // suffix length for 2 quads (up to 10 = 5 + 5)
        //  ((1U << len) - 1U) can be replaced with _bzhi_u32(UINT32_MAX, len); not fast
        uint32_t tmp = vlcval & ((1U << len) - 1U);  // suffix value for 2 quads
        vlcval       = vlc_dec.advance(&vlc_dec,len);
        uvlc_result >>= 4;
        // quad 0 length
        len = uvlc_result & 0x7;  // quad 0 suffix length
        uvlc_result >>= 3;
        // U = 1+ u
        u0 = 1 + (uvlc_result & 7) + (tmp & ~(0xFFU << len));  // always kappa = 1 in initial line pair
        u1 = 1 + (uvlc_result >> 3) + (tmp >> len);            // always kappa = 1 in initial line pair

        sp[1] = (uint16_t)u0;
        sp[3] = (uint16_t)u1;
    }
    // sp[0] = sp[1] = 0;

    // Non-initial line-pair
    dec_table = dec_cxt_vlc_table1;
    for (uint16_t row = 1; row < QH; row++) {
        sp0 = block_states + (row * 2U + 1U) * stride + 1U;
        sp1 = sp0 + stride;

        sp = t1->flags + row * sstr;// scratch + row * sstr;
        // calculate context for the next quad: w, sw, nw are always 0 at the head of a row
        context = ((sp[0 - sstr] & 0xA0U) << 2) | ((sp[2 - sstr] & 0x20U) << 4);
        for (qx = QW; qx > 0; qx -= 2, sp += 4) {
            // Decoding of significance and EMB patterns and unsigned residual offsets
            vlcval       = vlc_dec.fetch(&vlc_dec);
            uint16_t tv0 = dec_table[(vlcval & 0x7F) + context];
            if (context == 0) {
                mel_run -= 2;
                tv0 = (mel_run == -1) ? tv0 : 0;
                if (mel_run < 0) {
                    mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
                }
            }
            // calculate context for the next quad, Eq. (2) in the spec
            context = ((tv0 & 0x40U) << 2) | ((tv0 & 0x80U) << 1);              // (w | sw) << 8
            context |= (sp[0 - sstr] & 0x80U) | ((sp[2 - sstr] & 0xA0U) << 2);  // ((nw | n) << 7) | (ne << 9)
            context |= (sp[4 - sstr] & 0x20U) << 4;                             // ( nf) << 9

            sp[0] = tv0;

            vlcval = vlc_dec.advance(&vlc_dec,(tv0 & 0x000F) >> 1);

            // Decoding of significance and EMB patterns and unsigned residual offsets
            uint16_t tv1 = dec_table[(vlcval & 0x7F) + context];
            if (context == 0 && qx > 1) {
                mel_run -= 2;
                tv1 = (mel_run == -1) ? tv1 : 0;
                if (mel_run < 0) {
                    mel_run = MEL_get_run(&mel_dec, &MEL_k, &num_runs, &runs);
                }
            }
            tv1 = (qx > 1) ? tv1 : 0;
            // calculate context for the next quad, Eq. (2) in the spec
            context = ((tv1 & 0x40U) << 2) | ((tv1 & 0x80U) << 1);              // (w | sw) << 8
            context |= (sp[2 - sstr] & 0x80U) | ((sp[4 - sstr] & 0xA0U) << 2);  // ((nw | n) << 7) | (ne << 9)
            context |= (sp[6 - sstr] & 0x20U) << 4;                             // ( nf) << 9

            sp[2] = tv1;

            // store sigma
            *sp0++ = ((tv0 >> 4) >> 0) & 1;
            *sp0++ = ((tv0 >> 4) >> 2) & 1;
            *sp0++ = ((tv1 >> 4) >> 0) & 1;
            *sp0++ = ((tv1 >> 4) >> 2) & 1;
            *sp1++ = ((tv0 >> 4) >> 1) & 1;
            *sp1++ = ((tv0 >> 4) >> 3) & 1;
            *sp1++ = ((tv1 >> 4) >> 1) & 1;
            *sp1++ = ((tv1 >> 4) >> 3) & 1;

            vlcval = vlc_dec.advance(&vlc_dec,(tv1 & 0x000F) >> 1);

            // UVLC decoding
            u_off0       = tv0 & 1;
            u_off1       = tv1 & 1;
            uint32_t idx = (vlcval & 0x3F) + (u_off0 << 6U) + (u_off1 << 7U);

            uint32_t uvlc_result = uvlc_dec_1[idx];
            // remove total prefix length
            vlcval = vlc_dec.advance(&vlc_dec,uvlc_result & 0x7);
            uvlc_result >>= 3;
            // extract suffixes for quad 0 and 1
            uint32_t len = uvlc_result & 0xF;  // suffix length for 2 quads (up to 10 = 5 + 5)
            //  ((1U << len) - 1U) can be replaced with _bzhi_u32(UINT32_MAX, len); not fast
            uint32_t tmp = vlcval & ((1U << len) - 1U);  // suffix value for 2 quads
            vlcval       = vlc_dec.advance(&vlc_dec,len);
            uvlc_result >>= 4;
            // quad 0 length
            len = uvlc_result & 0x7;  // quad 0 suffix length
            uvlc_result >>= 3;
            u0 = (uvlc_result & 7) + (tmp & ~(0xFFU << len));
            u1 = (uvlc_result >> 3) + (tmp >> len);

            sp[1] = (uint16_t)u0;
            sp[3] = (uint16_t)u1;
        }
        // sp[0] = sp[1] = 0;
    }

    /*******************************************************************************************************************/
    // MagSgn decoding
    /*******************************************************************************************************************/
    {
        // We allocate a scratch row for storing v_n values.
        // We have 512 quads horizontally.
        // We need an extra entry to handle the case of vp[1]
        // when vp is at the last column.
        // Here, we allocate 4 instead of 1 to make the buffer size
        // a multipled of 16 bytes.
        const int v_n_size             = 512 + 4;
        uint32_t v_n_scratch[512 + 4] = { 0 };  // 2+ kB

        uint32_t prev_v_n = 0;
        uint32_t *vp = v_n_scratch;
        int32_t *dp  = t1->data;
        sp = t1->flags; // scratch;

        for (uint32_t x = 0; x < width; sp += 2, ++vp) {
            uint32_t v_n;
            uint32_t val = 0;
            uint32_t bit = 0;
            uint32_t inf = sp[0];
            uint32_t U_q = sp[1];
            if (U_q > ((30 - pLSB) + 2)) {
                av_log(NULL, AV_LOG_ERROR, "U_q is too large.\n");
                return -1;
            }

            if (inf & (1 << (4 + bit))) {
                // get 32 bits of magsgn data
                uint32_t ms_val = mag_dec.fetch(&mag_dec);
                uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                val = ms_val << 31;                      // get sign bit
                v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                v_n |= 1;                                // add center of bin
                // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                val |= (v_n + 2) << (pLSB - 1);
            }
            dp[0] = val;

            v_n = 0;
            val = 0;
            bit = 1;
            if (inf & (1 << (4 + bit))) {
                // get 32 bits of magsgn data
                uint32_t ms_val = mag_dec.fetch(&mag_dec);
                uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                val = ms_val << 31;                      // get sign bit
                v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                v_n |= 1;                                // add center of bin
                // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                val |= (v_n + 2) << (pLSB - 1);
            }
            dp[t1->stride] = val;
            vp[0]                      = prev_v_n | v_n;
            prev_v_n                   = 0;
            ++dp;
            if (++x >= width) {
                ++vp;
                break;
            }

            val = 0;
            bit = 2;
            if (inf & (1 << (4 + bit))) {
                // get 32 bits of magsgn data
                uint32_t ms_val = mag_dec.fetch(&mag_dec);
                uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                val = ms_val << 31;                      // get sign bit
                v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                v_n |= 1;                                // add center of bin
                // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                val |= (v_n + 2) << (pLSB - 1);
            }
            dp[0] = val;

            v_n = 0;
            val = 0;
            bit = 3;
            if (inf & (1 << (4 + bit))) {
                // get 32 bits of magsgn data
                uint32_t ms_val = mag_dec.fetch(&mag_dec);
                uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                val = ms_val << 31;                      // get sign bit
                v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                v_n |= 1;                                // add center of bin
                // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                val |= (v_n + 2) << (pLSB - 1);
            }
            dp[t1->stride] = val;
            prev_v_n                   = v_n;
            ++dp;
            ++x;
        }
        vp[0] = prev_v_n;

        for (uint32_t y = 2; y < height; y += 2) {
            sp = t1->flags + (y >> 1) * sstr; //scratch + (y >> 1) * sstr;
            vp = v_n_scratch;
            dp  = t1->data + y * t1->stride;

            prev_v_n = 0;
            for (uint32_t x = 0; x < width; sp += 2, ++vp) {
                uint32_t inf = sp[0];
                uint32_t u_q = sp[1];
                uint32_t emax, kappa, U_q;
                uint32_t gamma = inf & 0xF0;
                uint32_t v_n;
                uint32_t val = 0;
                uint32_t bit = 0;

                gamma &= gamma - 0x10;  // is gamma_q 1?
                emax  = vp[0] | vp[1];
                emax = 31 - ff_clz(emax | 2);  // emax - 1
                kappa = gamma ? emax : 1;

                U_q = u_q + kappa;
                if (U_q > ((30 - pLSB) + 2)) {
                    av_log(NULL, AV_LOG_ERROR, "U_q is too large.\n");
                    return -1;
                }

                if (inf & (1 << (4 + bit))) {
                    // get 32 bits of magsgn data
                    uint32_t ms_val = mag_dec.fetch(&mag_dec);
                    uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                    mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                    val = ms_val << 31;                      // get sign bit
                    v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                    v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                    v_n |= 1;                                // add center of bin
                    // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                    // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                    val |= (v_n + 2) << (pLSB - 1);
                }
                dp[0] = val;

                v_n = 0;
                val = 0;
                bit = 1;
                if (inf & (1 << (4 + bit))) {
                    // get 32 bits of magsgn data
                    uint32_t ms_val = mag_dec.fetch(&mag_dec);
                    uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                    mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                    val = ms_val << 31;                      // get sign bit
                    v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                    v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                    v_n |= 1;                                // add center of bin
                    // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                    // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                    val |= (v_n + 2) << (pLSB - 1);
                }
                dp[t1->stride] = val;
                vp[0]                      = prev_v_n | v_n;
                prev_v_n                   = 0;
                ++dp;
                if (++x >= width) {
                    ++vp;
                    break;
                }

                val = 0;
                bit = 2;
                if (inf & (1 << (4 + bit))) {
                    // get 32 bits of magsgn data
                    uint32_t ms_val = mag_dec.fetch(&mag_dec);
                    uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                    mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                    val = ms_val << 31;                      // get sign bit
                    v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                    v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                    v_n |= 1;                                // add center of bin
                    // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                    // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                    val |= (v_n + 2) << (pLSB - 1);
                }
                dp[0] = val;

                v_n = 0;
                val = 0;
                bit = 3;
                if (inf & (1 << (4 + bit))) {
                    // get 32 bits of magsgn data
                    uint32_t ms_val = mag_dec.fetch(&mag_dec);
                    uint32_t m_n    = U_q - ((inf >> (12 + bit)) & 1);  // remove e_k
                    mag_dec.advance(&mag_dec, m_n);                                // consume m_n

                    val = ms_val << 31;                      // get sign bit
                    v_n = ms_val & ((1 << m_n) - 1);         // keep only m_n bits
                    v_n |= ((inf >> (8 + bit)) & 1) << m_n;  // add EMB e_1 as MSB
                    v_n |= 1;                                // add center of bin
                    // v_n now has 2 * (\mu - 1) + 0.5 with correct sign bit
                    // add 2 to make it 2*\mu+0.5, shift it up to missing MSBs
                    val |= (v_n + 2) << (pLSB - 1);
                }
                dp[t1->stride] = val;
                prev_v_n                   = v_n;
                ++dp;
                ++x;
            }
            vp[0] = prev_v_n;
        }
    }
    return 1;
}

static void jpeg2000_calc_mbr(uint8_t *mbr, const uint16_t i, const uint16_t j,
                              const uint32_t mbr_info, uint8_t causal_cond,
                              uint8_t *block_states, int stride)
{
    uint8_t *state_p0 = block_states + i * stride + j;
    uint8_t *state_p1 = block_states + (i + 1) * stride + j;
    uint8_t *state_p2 = block_states + (i + 2) * stride + j;

    uint8_t mbr0 = state_p0[0] | state_p0[1] | state_p0[2];
    uint8_t mbr1 = state_p1[0] | state_p1[2];
    uint8_t mbr2 = state_p2[0] | state_p2[1] | state_p2[2];
    *mbr  = mbr0 | mbr1 | (mbr2 & causal_cond);
    *mbr |= (mbr0 >> HT_SHIFT_REF) & (mbr0 >> HT_SHIFT_SCAN);
    *mbr |= (mbr1 >> HT_SHIFT_REF) & (mbr1 >> HT_SHIFT_SCAN);
    *mbr |= (mbr2 >> HT_SHIFT_REF) & (mbr2 >> HT_SHIFT_SCAN) & causal_cond;
    *mbr &= 1;
}

static void jpeg2000_process_stripes_block(Jpeg2000T1Context *t1, StateVars *sig_prop, int i_s, int j_s,
                                           int width, int height, int stride, int pLSB,
                                           uint8_t *block_states,
                                           uint8_t *magref_segment, uint32_t magref_length,
                                           uint8_t is_causal)
{
    for (int j = j_s; j < j_s + width; j++) {
        uint32_t  mbr_info = 0;
        for (int i = i_s; i < i_s + height; i++) {
            uint8_t bit;
            uint8_t causal_cond = (is_causal == 0) || (i != (i_s + height - 1));
            int32_t *sp = t1->data + j + i * t1->stride;
            uint8_t mbr = 0;
            uint8_t *state_p = block_states + (i + 1) * stride + (j + 1);

            if ((state_p[0] >> HT_SHIFT_SIGMA & 1) == 0)
                jpeg2000_calc_mbr(&mbr, i, j, mbr_info & 0x1EF, causal_cond, block_states, stride);
            mbr_info >>= 3;

            state_p[0] |= 1 << HT_SHIFT_SCAN;
            if (mbr != 0) {
                state_p[0] |= 1 << HT_SHIFT_REF_IND;
                bit = jpeg2000_peek_bit(sig_prop, magref_segment, magref_length);
                state_p[0] |= bit << HT_SHIFT_REF;
                *sp |= bit << pLSB;
                *sp |= bit << (pLSB - 1); // Add 0.5 (reconstruction parameter = 1/2)
            }
        }
    }
    // decode sign
    for (int j = j_s; j < j_s + width; j++) {
        for (int i = i_s; i < i_s + height; i++) {
            uint8_t bit;
            int32_t *sp = t1->data + j + i * t1->stride;
            uint8_t *state_p = block_states + (i + 1) * stride + (j + 1);
            if ((state_p[0] >> HT_SHIFT_REF) & 1) {
                bit = jpeg2000_peek_bit(sig_prop, magref_segment, magref_length);
                *sp |= (int32_t)bit << 31;
            }
        }
    }
}

/**
 * See procedure decodeSigPropMag at Rec. ITU-T T.814, 7.4.
*/
av_noinline
static void jpeg2000_decode_sigprop_segment(Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk,
                                            const int stride, uint8_t *magref_segment,
                                            uint32_t magref_length, uint8_t pLSB,
                                            uint8_t *block_states)
{
    StateVars sp_dec;
    const uint16_t width = cblk->coord[0][1] - cblk->coord[0][0];
    const uint16_t height = cblk->coord[1][1] - cblk->coord[1][0];

    const uint16_t num_v_stripe = height / 4;
    const uint16_t num_h_stripe = width / 4;
    int b_width                 = 4;
    int b_height                = 4;

    int last_width;
    uint16_t i = 0, j = 0;
    uint8_t is_causal = cblk->modes & JPEG2000_CBLK_VSC;

    jpeg2000_init_zero(&sp_dec);

    for (int n1 = 0; n1 < num_v_stripe; n1++) {
        j = 0;
        for (int n2 = 0; n2 < num_h_stripe; n2++) {
            jpeg2000_process_stripes_block(t1, &sp_dec, i, j, b_width, b_height, stride,
                                           pLSB, block_states, magref_segment,
                                           magref_length, is_causal);
            j += 4;
        }
        last_width = width % 4;
        if (last_width)
            jpeg2000_process_stripes_block(t1, &sp_dec, i, j, last_width, b_height, stride,
                                           pLSB, block_states, magref_segment,
                                           magref_length, is_causal);
        i += 4;
    }

    /* Decode remaining height stripes */
    b_height = height % 4;
    j = 0;
    for (int n2 = 0; n2 < num_h_stripe; n2++) {
        jpeg2000_process_stripes_block(t1, &sp_dec, i, j, b_width, b_height, stride,
                                       pLSB, block_states, magref_segment,
                                       magref_length, is_causal);
        j += 4;
    }
    last_width = width % 4;
    if (last_width)
        jpeg2000_process_stripes_block(t1, &sp_dec, i, j, last_width, b_height, stride,
                                       pLSB, block_states, magref_segment,
                                       magref_length, is_causal);
}

/**
 * See procedure decodeSigPropMag at Rec. ITU-T T.814, 7.5.
*/
static void
jpeg2000_decode_magref_segment( Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, const int stride,
                                uint8_t *magref_segment, uint32_t magref_length,
                                uint8_t pLSB, uint8_t *block_states)
{

    StateVars mag_ref           = { 0 };
    const uint16_t width = cblk->coord[0][1] - cblk->coord[0][0];
    const uint16_t block_height = cblk->coord[1][1] - cblk->coord[1][0];
    const uint16_t num_v_stripe = block_height / 4;
    uint16_t height             = 4;
    uint16_t i_start            = 0;
    int32_t *sp;
    int32_t bit;
    int32_t tmp;
    jpeg2000_init_mag_ref(&mag_ref, magref_length);

    for (int n1 = 0; n1 < num_v_stripe; n1++) {
        for (int j = 0; j < width; j++) {
            for (int i = i_start; i < i_start + height; i++) {
                /**
                 *  We move column wise, going from one quad to another. See
                 *  Rec. ITU-T T.814, Figure 7.
                 */
                uint8_t *state_p = block_states + (i + 1) * stride + (j + 1);
                sp = t1->data + j + i * t1->stride;
                if ((state_p[0] >> HT_SHIFT_SIGMA & 1) != 0) {
                    state_p[0] |= 1 << HT_SHIFT_REF_IND;
                    bit = jpeg2000_import_magref_bit(&mag_ref, magref_segment, magref_length);
                    tmp = 0xFFFFFFFE | (uint32_t)bit;
                    tmp <<= pLSB;
                    sp[0] &= tmp;
                    sp[0] |= 1 << (pLSB - 1); // Add 0.5 (reconstruction parameter = 1/2)
                }
            }
        }
        i_start += 4;
    }
    height = block_height % 4;
    for (int j = 0; j < width; j++) {
        for (int i = i_start; i < i_start + height; i++) {
            uint8_t *state_p = block_states + (i + 1) * stride + (j + 1);
            sp = t1->data + j + i * t1->stride;
            if ((state_p[0] >> HT_SHIFT_SIGMA & 1) != 0) {
                state_p[0] |= 1 << HT_SHIFT_REF_IND;
                bit = jpeg2000_import_magref_bit(&mag_ref, magref_segment, magref_length);
                tmp = 0xFFFFFFFE | (uint32_t)bit;
                tmp <<= pLSB;
                sp[0] &= tmp;
                sp[0] |= 1 << (pLSB - 1); // Add 0.5 (reconstruction parameter = 1/2)
            }
        }
    }
}


int
ff_jpeg2000_decode_htj2k(const Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk,
                         int width, int height, int M_b, uint8_t roi_shift)
{
    uint8_t p0 = 0;             // 3 * p0 = Number of placeholder passes
    uint32_t Lcup;              // Length of HT cleanup segment
    uint32_t Lref;              // Length of Refinement segment
    uint32_t Scup;              // HT cleanup segment suffix length
    uint32_t Pcup;              // HT cleanup segment prefix length

    uint8_t S_blk;              // Number of skipped magnitude bitplanes
    uint8_t pLSB;

    uint8_t *Dcup;              // Byte of an HT cleanup segment
    uint8_t *Dref;              // Byte of an HT refinement segment

    int z_blk;                  // Number of ht coding pass

    uint8_t num_plhd_passes;    // Number of placeholder passes

    StateVars sig_prop;         // Significance propagation


    int ret;

    /* Temporary buffer */
    uint8_t *block_states = NULL;

    const uint32_t mask  = UINT32_MAX >> (M_b + 1); // bit mask for ROI detection
    uint8_t num_rempass;

    const int quad_buf_width = width + 4;
    const int quad_buf_height = height + 4;

    /* codeblock size as constrained by Rec. ITU-T T.800, Table A.18 */
    av_assert0(width <= 1024U && height <= 1024U);
    av_assert0(width * height <= 4096);
    av_assert0(width * height > 0);

    memset(t1->data, 0, t1->stride * height * sizeof(*t1->data));
    memset(t1->flags, 0, t1->stride * (height + 2) * sizeof(*t1->flags));

    if (cblk->npasses == 0)
        return 0;

    num_rempass = cblk->npasses % 3;  // Number of remainder passes
    num_plhd_passes = num_rempass ? cblk->npasses - num_rempass : cblk->npasses - 3;
    av_assert0(num_plhd_passes % 3 == 0);
    p0 = num_plhd_passes / 3;
    z_blk = cblk->npasses - num_plhd_passes;

    if (z_blk <= 0)
        return 0; // No passes within this set, continue

    Lcup = cblk->pass_lengths[0];
    Lref = cblk->pass_lengths[1];

    if (Lcup < 2) {
        av_log(s->avctx, AV_LOG_ERROR,
               "Cleanup pass length must be at least 2 bytes in length\n");
        return AVERROR_INVALIDDATA;
    }
    Dcup = cblk->data;
    Dref  = cblk->data + Lcup; // Dref comes after the refinement segment

    cblk->data[cblk->length] = 0xFF; // an extra byte for refinement segment (buffer->last)

    S_blk = p0 + cblk->zbp;
    cblk->zbp = S_blk - 1;
    pLSB  = 30 - S_blk;

    Scup = (Dcup[Lcup - 1] << 4) + (Dcup[Lcup - 2] & 0x0F);

    if (Scup < 2 || Scup > Lcup || Scup > 4079) {
        av_log(s->avctx, AV_LOG_ERROR, "Cleanup pass suffix length is invalid %d\n",
               Scup);
        ret = AVERROR_INVALIDDATA;
        goto free;
    }
    Pcup = Lcup - Scup;

    /* modDcup shall be done before the creation of vlc instance. */
    Dcup[Lcup - 1] = 0xFF;
    Dcup[Lcup - 2] |= 0x0F;

    block_states = av_calloc(quad_buf_width * quad_buf_height, sizeof(uint8_t));

    if (!block_states) {
        ret = AVERROR(ENOMEM);
        goto free;
    }
    if ((ret = jpeg2000_decode_ht_cleanup_segment(t1, cblk, Dcup, Lcup, Pcup, pLSB, quad_buf_width, block_states)) < 0) {
        av_log(s->avctx, AV_LOG_ERROR, "Bad HT cleanup segment\n");
        goto free;
    }

    if (z_blk > 1)
        jpeg2000_decode_sigprop_segment(t1, cblk, quad_buf_width, Dref, Lref, pLSB - 1, block_states);

    if (z_blk > 2)
        jpeg2000_decode_magref_segment(t1, cblk, quad_buf_width, Dref, Lref, pLSB - 1, block_states);

    pLSB = 31 - M_b;

    /* Reconstruct the sample values */
    for (int y = 0; y < height; y++) {
        int32_t *val = t1->data + y * t1->stride;
        int32_t len = width;
        for (; len > 0; --len) {
            int32_t sign = val[0] & INT32_MIN;
            val[0] &= INT32_MAX;

            /* ROI shift, if necessary */
            if (roi_shift && (((uint32_t)val[0] & ~mask) == 0))
                val[0] <<= roi_shift;
            /* Shift down to 1 bit upper from decimal point for reconstruction value (= 0.5) */
            val[0] >>= pLSB - 1;
            /* Convert sign-magnitude to two's complement. */
            if (sign)
                val[0] = -(val[0] & INT32_MAX);
            val++;
        }
    }
free:
    av_freep(&block_states);
    return ret;
}

/**
 * CtxVLC tables (see Rec. ITU-T T.800, Annex C) as found at
 * https://github.com/osamu620/OpenHTJ2K (author: Osamu Watanabe)
 */
static const alignas(32) uint16_t dec_cxt_vlc_table1[1024] = {
        0x0016, 0x006A, 0x0046, 0x00DD, 0x0086, 0x888B, 0x0026, 0x444D, 0x0016, 0x00AA, 0x0046, 0x88AD, 0x0086,
        0x003A, 0x0026, 0x00DE, 0x0016, 0x00CA, 0x0046, 0x009D, 0x0086, 0x005A, 0x0026, 0x222D, 0x0016, 0x009A,
        0x0046, 0x007D, 0x0086, 0x10FD, 0x0026, 0x007E, 0x0016, 0x006A, 0x0046, 0x88CD, 0x0086, 0x888B, 0x0026,
        0x111D, 0x0016, 0x00AA, 0x0046, 0x005D, 0x0086, 0x003A, 0x0026, 0x00EE, 0x0016, 0x00CA, 0x0046, 0x00BD,
        0x0086, 0x005A, 0x0026, 0x11FF, 0x0016, 0x009A, 0x0046, 0x003D, 0x0086, 0x40ED, 0x0026, 0xA2AF, 0x0016,
        0x006A, 0x0046, 0x00DD, 0x0086, 0x888B, 0x0026, 0x444D, 0x0016, 0x00AA, 0x0046, 0x88AD, 0x0086, 0x003A,
        0x0026, 0x44EF, 0x0016, 0x00CA, 0x0046, 0x009D, 0x0086, 0x005A, 0x0026, 0x222D, 0x0016, 0x009A, 0x0046,
        0x007D, 0x0086, 0x10FD, 0x0026, 0x00BE, 0x0016, 0x006A, 0x0046, 0x88CD, 0x0086, 0x888B, 0x0026, 0x111D,
        0x0016, 0x00AA, 0x0046, 0x005D, 0x0086, 0x003A, 0x0026, 0xC4CF, 0x0016, 0x00CA, 0x0046, 0x00BD, 0x0086,
        0x005A, 0x0026, 0x00FE, 0x0016, 0x009A, 0x0046, 0x003D, 0x0086, 0x40ED, 0x0026, 0x006F, 0x0002, 0x0088,
        0x0002, 0x005C, 0x0002, 0x0018, 0x0002, 0x00DE, 0x0002, 0x0028, 0x0002, 0x009C, 0x0002, 0x004A, 0x0002,
        0x007E, 0x0002, 0x0088, 0x0002, 0x00CC, 0x0002, 0x0018, 0x0002, 0x888F, 0x0002, 0x0028, 0x0002, 0x00FE,
        0x0002, 0x003A, 0x0002, 0x222F, 0x0002, 0x0088, 0x0002, 0x40FD, 0x0002, 0x0018, 0x0002, 0x00BE, 0x0002,
        0x0028, 0x0002, 0x00BF, 0x0002, 0x004A, 0x0002, 0x006E, 0x0002, 0x0088, 0x0002, 0x00AC, 0x0002, 0x0018,
        0x0002, 0x444F, 0x0002, 0x0028, 0x0002, 0x00EE, 0x0002, 0x003A, 0x0002, 0x113F, 0x0002, 0x0088, 0x0002,
        0x005C, 0x0002, 0x0018, 0x0002, 0x00CF, 0x0002, 0x0028, 0x0002, 0x009C, 0x0002, 0x004A, 0x0002, 0x006F,
        0x0002, 0x0088, 0x0002, 0x00CC, 0x0002, 0x0018, 0x0002, 0x009F, 0x0002, 0x0028, 0x0002, 0x00EF, 0x0002,
        0x003A, 0x0002, 0x323F, 0x0002, 0x0088, 0x0002, 0x40FD, 0x0002, 0x0018, 0x0002, 0x00AF, 0x0002, 0x0028,
        0x0002, 0x44FF, 0x0002, 0x004A, 0x0002, 0x005F, 0x0002, 0x0088, 0x0002, 0x00AC, 0x0002, 0x0018, 0x0002,
        0x007F, 0x0002, 0x0028, 0x0002, 0x00DF, 0x0002, 0x003A, 0x0002, 0x111F, 0x0002, 0x0028, 0x0002, 0x005C,
        0x0002, 0x008A, 0x0002, 0x00BF, 0x0002, 0x0018, 0x0002, 0x00FE, 0x0002, 0x00CC, 0x0002, 0x007E, 0x0002,
        0x0028, 0x0002, 0xF8FF, 0x0002, 0x004A, 0x0002, 0x007F, 0x0002, 0x0018, 0x0002, 0x00DF, 0x0002, 0x00AC,
        0x0002, 0x313F, 0x0002, 0x0028, 0x0002, 0x222D, 0x0002, 0x008A, 0x0002, 0x00BE, 0x0002, 0x0018, 0x0002,
        0x44EF, 0x0002, 0xA2AD, 0x0002, 0x006E, 0x0002, 0x0028, 0x0002, 0x51FF, 0x0002, 0x004A, 0x0002, 0x009E,
        0x0002, 0x0018, 0x0002, 0x00CF, 0x0002, 0x003C, 0x0002, 0x223F, 0x0002, 0x0028, 0x0002, 0x005C, 0x0002,
        0x008A, 0x0002, 0xB2BF, 0x0002, 0x0018, 0x0002, 0x40EF, 0x0002, 0x00CC, 0x0002, 0x006F, 0x0002, 0x0028,
        0x0002, 0x72FF, 0x0002, 0x004A, 0x0002, 0x009F, 0x0002, 0x0018, 0x0002, 0x00DE, 0x0002, 0x00AC, 0x0002,
        0x444F, 0x0002, 0x0028, 0x0002, 0x222D, 0x0002, 0x008A, 0x0002, 0xA8AF, 0x0002, 0x0018, 0x0002, 0x00EE,
        0x0002, 0xA2AD, 0x0002, 0x005F, 0x0002, 0x0028, 0x0002, 0x44FF, 0x0002, 0x004A, 0x0002, 0x888F, 0x0002,
        0x0018, 0x0002, 0xAAAF, 0x0002, 0x003C, 0x0002, 0x111F, 0x0004, 0xF8FD, 0x0028, 0x005C, 0x0004, 0x00BC,
        0x008A, 0x66FF, 0x0004, 0x00CD, 0x0018, 0x111D, 0x0004, 0x009C, 0x003A, 0xA8AF, 0x0004, 0x00FC, 0x0028,
        0x313D, 0x0004, 0x00AC, 0x004A, 0xB3BF, 0x0004, 0xB2BD, 0x0018, 0xF5FF, 0x0004, 0x006C, 0x517D, 0x545F,
        0x0004, 0xF2FD, 0x0028, 0x222D, 0x0004, 0x22AD, 0x008A, 0x44EF, 0x0004, 0x00CC, 0x0018, 0xF4FF, 0x0004,
        0x007C, 0x003A, 0x447F, 0x0004, 0x40DD, 0x0028, 0x323D, 0x0004, 0x009D, 0x004A, 0x00DE, 0x0004, 0x88BD,
        0x0018, 0xFAFF, 0x0004, 0x115D, 0xF1FD, 0x444F, 0x0004, 0xF8FD, 0x0028, 0x005C, 0x0004, 0x00BC, 0x008A,
        0xC8EF, 0x0004, 0x00CD, 0x0018, 0x111D, 0x0004, 0x009C, 0x003A, 0x888F, 0x0004, 0x00FC, 0x0028, 0x313D,
        0x0004, 0x00AC, 0x004A, 0x44DF, 0x0004, 0xB2BD, 0x0018, 0xA8FF, 0x0004, 0x006C, 0x517D, 0x006F, 0x0004,
        0xF2FD, 0x0028, 0x222D, 0x0004, 0x22AD, 0x008A, 0x00EE, 0x0004, 0x00CC, 0x0018, 0xE2EF, 0x0004, 0x007C,
        0x003A, 0x727F, 0x0004, 0x40DD, 0x0028, 0x323D, 0x0004, 0x009D, 0x004A, 0xB1BF, 0x0004, 0x88BD, 0x0018,
        0x73FF, 0x0004, 0x115D, 0xF1FD, 0x333F, 0x0002, 0x0088, 0x0002, 0x20ED, 0x0002, 0x00CA, 0x0002, 0xC4CF,
        0x0002, 0x0048, 0x0002, 0x32FF, 0x0002, 0x001A, 0x0002, 0x888F, 0x0002, 0x0088, 0x0002, 0x006C, 0x0002,
        0x002A, 0x0002, 0x00AF, 0x0002, 0x0048, 0x0002, 0x22EF, 0x0002, 0x00AC, 0x0002, 0x005F, 0x0002, 0x0088,
        0x0002, 0x444D, 0x0002, 0x00CA, 0x0002, 0xCCCF, 0x0002, 0x0048, 0x0002, 0x00FE, 0x0002, 0x001A, 0x0002,
        0x006F, 0x0002, 0x0088, 0x0002, 0x005C, 0x0002, 0x002A, 0x0002, 0x009F, 0x0002, 0x0048, 0x0002, 0x00DF,
        0x0002, 0x30FD, 0x0002, 0x222F, 0x0002, 0x0088, 0x0002, 0x20ED, 0x0002, 0x00CA, 0x0002, 0xC8CF, 0x0002,
        0x0048, 0x0002, 0x11FF, 0x0002, 0x001A, 0x0002, 0x007E, 0x0002, 0x0088, 0x0002, 0x006C, 0x0002, 0x002A,
        0x0002, 0x007F, 0x0002, 0x0048, 0x0002, 0x00EE, 0x0002, 0x00AC, 0x0002, 0x003E, 0x0002, 0x0088, 0x0002,
        0x444D, 0x0002, 0x00CA, 0x0002, 0x00BE, 0x0002, 0x0048, 0x0002, 0x00BF, 0x0002, 0x001A, 0x0002, 0x003F,
        0x0002, 0x0088, 0x0002, 0x005C, 0x0002, 0x002A, 0x0002, 0x009E, 0x0002, 0x0048, 0x0002, 0x00DE, 0x0002,
        0x30FD, 0x0002, 0x111F, 0x0004, 0xA8ED, 0x0048, 0x888D, 0x0004, 0x00DC, 0x00CA, 0xF3FF, 0x0004, 0xFCFD,
        0x002A, 0x003D, 0x0004, 0x00BC, 0x005A, 0xD8DF, 0x0004, 0xF8FD, 0x0048, 0x006C, 0x0004, 0x207D, 0x008A,
        0x99FF, 0x0004, 0x00EC, 0x00FA, 0x003C, 0x0004, 0x00AC, 0x001A, 0x009F, 0x0004, 0xF2FD, 0x0048, 0x007C,
        0x0004, 0x44CD, 0x00CA, 0x76FF, 0x0004, 0xF1FD, 0x002A, 0x444D, 0x0004, 0x00AD, 0x005A, 0xC8CF, 0x0004,
        0xF4FD, 0x0048, 0x445D, 0x0004, 0x10BD, 0x008A, 0xE4EF, 0x0004, 0x54DD, 0x00FA, 0x111D, 0x0004, 0x009C,
        0x001A, 0x222F, 0x0004, 0xA8ED, 0x0048, 0x888D, 0x0004, 0x00DC, 0x00CA, 0xFAFF, 0x0004, 0xFCFD, 0x002A,
        0x003D, 0x0004, 0x00BC, 0x005A, 0x11BF, 0x0004, 0xF8FD, 0x0048, 0x006C, 0x0004, 0x207D, 0x008A, 0x22EF,
        0x0004, 0x00EC, 0x00FA, 0x003C, 0x0004, 0x00AC, 0x001A, 0x227F, 0x0004, 0xF2FD, 0x0048, 0x007C, 0x0004,
        0x44CD, 0x00CA, 0xD5FF, 0x0004, 0xF1FD, 0x002A, 0x444D, 0x0004, 0x00AD, 0x005A, 0x006F, 0x0004, 0xF4FD,
        0x0048, 0x445D, 0x0004, 0x10BD, 0x008A, 0x11DF, 0x0004, 0x54DD, 0x00FA, 0x111D, 0x0004, 0x009C, 0x001A,
        0x515F, 0x0006, 0x00FC, 0x0018, 0x111D, 0x0048, 0x888D, 0x00AA, 0xD4DF, 0x0006, 0xA2AD, 0x005A, 0x76FF,
        0x0028, 0x223D, 0x00BC, 0xAAAF, 0x0006, 0x00EC, 0x0018, 0xF5FF, 0x0048, 0x006C, 0x008A, 0xCCCF, 0x0006,
        0x009D, 0x00CA, 0x44EF, 0x0028, 0x003C, 0xF8FD, 0x317F, 0x0006, 0xE8ED, 0x0018, 0xF1FF, 0x0048, 0x007C,
        0x00AA, 0xC4CF, 0x0006, 0x227D, 0x005A, 0xD1DF, 0x0028, 0x444D, 0xF4FD, 0x515F, 0x0006, 0x00DC, 0x0018,
        0xE2EF, 0x0048, 0x445D, 0x008A, 0x22BF, 0x0006, 0x009C, 0x00CA, 0xC8DF, 0x0028, 0x222D, 0xF2FD, 0x226F,
        0x0006, 0x00FC, 0x0018, 0x111D, 0x0048, 0x888D, 0x00AA, 0xB1BF, 0x0006, 0xA2AD, 0x005A, 0x33FF, 0x0028,
        0x223D, 0x00BC, 0xA8AF, 0x0006, 0x00EC, 0x0018, 0xB9FF, 0x0048, 0x006C, 0x008A, 0xA8BF, 0x0006, 0x009D,
        0x00CA, 0xE4EF, 0x0028, 0x003C, 0xF8FD, 0x646F, 0x0006, 0xE8ED, 0x0018, 0xFCFF, 0x0048, 0x007C, 0x00AA,
        0xC8CF, 0x0006, 0x227D, 0x005A, 0xEAEF, 0x0028, 0x444D, 0xF4FD, 0x747F, 0x0006, 0x00DC, 0x0018, 0xFAFF,
        0x0048, 0x445D, 0x008A, 0xB2BF, 0x0006, 0x009C, 0x00CA, 0x44DF, 0x0028, 0x222D, 0xF2FD, 0x313F, 0x00F6,
        0xFAFD, 0xF1FB, 0x003C, 0x0008, 0x32BD, 0x007A, 0x11DF, 0x00F6, 0x54DD, 0xF2FB, 0xE4EF, 0x00DA, 0x717D,
        0xFCFD, 0x737F, 0x00F6, 0xF3FD, 0xF8FB, 0x111D, 0x0008, 0x009C, 0x005A, 0xB1BF, 0x00F6, 0x00CD, 0x00BA,
        0xD8DF, 0xF4FB, 0x006C, 0xB9FD, 0x545F, 0x00F6, 0x76FD, 0xF1FB, 0x002C, 0x0008, 0x00AC, 0x007A, 0x009F,
        0x00F6, 0x00AD, 0xF2FB, 0xF7FF, 0x00DA, 0x004C, 0xF5FD, 0x747F, 0x00F6, 0x00EC, 0xF8FB, 0x001C, 0x0008,
        0x008C, 0x005A, 0x888F, 0x00F6, 0x00CC, 0x00BA, 0xE2EF, 0xF4FB, 0x115D, 0xA8ED, 0x113F, 0x00F6, 0xFAFD,
        0xF1FB, 0x003C, 0x0008, 0x32BD, 0x007A, 0xD1DF, 0x00F6, 0x54DD, 0xF2FB, 0xFBFF, 0x00DA, 0x717D, 0xFCFD,
        0x447F, 0x00F6, 0xF3FD, 0xF8FB, 0x111D, 0x0008, 0x009C, 0x005A, 0x727F, 0x00F6, 0x00CD, 0x00BA, 0x22EF,
        0xF4FB, 0x006C, 0xB9FD, 0x444F, 0x00F6, 0x76FD, 0xF1FB, 0x002C, 0x0008, 0x00AC, 0x007A, 0x11BF, 0x00F6,
        0x00AD, 0xF2FB, 0xFFFF, 0x00DA, 0x004C, 0xF5FD, 0x323F, 0x00F6, 0x00EC, 0xF8FB, 0x001C, 0x0008, 0x008C,
        0x005A, 0x006F, 0x00F6, 0x00CC, 0x00BA, 0xB8BF, 0xF4FB, 0x115D, 0xA8ED, 0x222F};

static const alignas(32) uint16_t dec_cxt_vlc_table0[1024] = {
        0x0026, 0x00AA, 0x0046, 0x006C, 0x0086, 0xA8ED, 0x0018, 0xD8DF, 0x0026, 0x10BD, 0x0046, 0xF5FF, 0x0086,
        0x207D, 0x005A, 0x515F, 0x0026, 0x003A, 0x0046, 0x444D, 0x0086, 0xC4CD, 0x0018, 0xCCCF, 0x0026, 0xE2FD,
        0x0046, 0x99FF, 0x0086, 0x009C, 0x00CA, 0x313F, 0x0026, 0x00AA, 0x0046, 0x445D, 0x0086, 0xC8CD, 0x0018,
        0x11DF, 0x0026, 0xF4FD, 0x0046, 0xFCFF, 0x0086, 0x009D, 0x005A, 0x007E, 0x0026, 0x003A, 0x0046, 0xF1FF,
        0x0086, 0x88AD, 0x0018, 0x00BE, 0x0026, 0xF8FD, 0x0046, 0xE4EF, 0x0086, 0x888D, 0x00CA, 0x111F, 0x0026,
        0x00AA, 0x0046, 0x006C, 0x0086, 0xA8ED, 0x0018, 0x54DF, 0x0026, 0x10BD, 0x0046, 0x22EF, 0x0086, 0x207D,
        0x005A, 0x227F, 0x0026, 0x003A, 0x0046, 0x444D, 0x0086, 0xC4CD, 0x0018, 0x11BF, 0x0026, 0xE2FD, 0x0046,
        0x00FE, 0x0086, 0x009C, 0x00CA, 0x223F, 0x0026, 0x00AA, 0x0046, 0x445D, 0x0086, 0xC8CD, 0x0018, 0x00DE,
        0x0026, 0xF4FD, 0x0046, 0xBAFF, 0x0086, 0x009D, 0x005A, 0x006F, 0x0026, 0x003A, 0x0046, 0xE6FF, 0x0086,
        0x88AD, 0x0018, 0xA2AF, 0x0026, 0xF8FD, 0x0046, 0x00EE, 0x0086, 0x888D, 0x00CA, 0x222F, 0x0004, 0x00CA,
        0x0088, 0x207D, 0x0004, 0xC4CD, 0x0028, 0x00FE, 0x0004, 0xA2FD, 0x0048, 0x005C, 0x0004, 0x009D, 0x0018,
        0x00DE, 0x0004, 0x10BD, 0x0088, 0x006C, 0x0004, 0x88AD, 0x0028, 0x11DF, 0x0004, 0xA8ED, 0x0048, 0x003C,
        0x0004, 0x888D, 0x0018, 0x111F, 0x0004, 0x00CA, 0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0x88FF, 0x0004,
        0xB8FD, 0x0048, 0x444D, 0x0004, 0x009C, 0x0018, 0x00BE, 0x0004, 0xE4FD, 0x0088, 0x445D, 0x0004, 0x00AC,
        0x0028, 0x00EE, 0x0004, 0x54DD, 0x0048, 0x222D, 0x0004, 0x003D, 0x0018, 0x007E, 0x0004, 0x00CA, 0x0088,
        0x207D, 0x0004, 0xC4CD, 0x0028, 0xF1FF, 0x0004, 0xA2FD, 0x0048, 0x005C, 0x0004, 0x009D, 0x0018, 0x11BF,
        0x0004, 0x10BD, 0x0088, 0x006C, 0x0004, 0x88AD, 0x0028, 0x22EF, 0x0004, 0xA8ED, 0x0048, 0x003C, 0x0004,
        0x888D, 0x0018, 0x227F, 0x0004, 0x00CA, 0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0xE4EF, 0x0004, 0xB8FD,
        0x0048, 0x444D, 0x0004, 0x009C, 0x0018, 0xA2AF, 0x0004, 0xE4FD, 0x0088, 0x445D, 0x0004, 0x00AC, 0x0028,
        0xD8DF, 0x0004, 0x54DD, 0x0048, 0x222D, 0x0004, 0x003D, 0x0018, 0x515F, 0x0004, 0x005A, 0x0088, 0x006C,
        0x0004, 0x88DD, 0x0028, 0x32FF, 0x0004, 0x11FD, 0x0048, 0x444D, 0x0004, 0x00AD, 0x0018, 0x00BE, 0x0004,
        0x317D, 0x0088, 0x515D, 0x0004, 0x00CC, 0x0028, 0x00DE, 0x0004, 0x20ED, 0x0048, 0x111D, 0x0004, 0x009D,
        0x0018, 0x007E, 0x0004, 0x005A, 0x0088, 0x545D, 0x0004, 0x44CD, 0x0028, 0x00EE, 0x0004, 0xF1FD, 0x0048,
        0x003C, 0x0004, 0x00AC, 0x0018, 0x555F, 0x0004, 0x74FD, 0x0088, 0x113D, 0x0004, 0x20BD, 0x0028, 0x747F,
        0x0004, 0xC4DD, 0x0048, 0xF8FF, 0x0004, 0x009C, 0x0018, 0x222F, 0x0004, 0x005A, 0x0088, 0x006C, 0x0004,
        0x88DD, 0x0028, 0x00FE, 0x0004, 0x11FD, 0x0048, 0x444D, 0x0004, 0x00AD, 0x0018, 0x888F, 0x0004, 0x317D,
        0x0088, 0x515D, 0x0004, 0x00CC, 0x0028, 0xC8CF, 0x0004, 0x20ED, 0x0048, 0x111D, 0x0004, 0x009D, 0x0018,
        0x006F, 0x0004, 0x005A, 0x0088, 0x545D, 0x0004, 0x44CD, 0x0028, 0xD1DF, 0x0004, 0xF1FD, 0x0048, 0x003C,
        0x0004, 0x00AC, 0x0018, 0x227F, 0x0004, 0x74FD, 0x0088, 0x113D, 0x0004, 0x20BD, 0x0028, 0x22BF, 0x0004,
        0xC4DD, 0x0048, 0x22EF, 0x0004, 0x009C, 0x0018, 0x323F, 0x0006, 0xD4DD, 0xF4FB, 0xFCFF, 0x0018, 0x113D,
        0x005A, 0x888F, 0x0006, 0x32BD, 0x008A, 0x00EE, 0x002A, 0x515D, 0xAAFD, 0x727F, 0x0006, 0x44CD, 0xF8FB,
        0x44EF, 0x0018, 0x647D, 0x004A, 0xA2AF, 0x0006, 0x00AC, 0x555B, 0x99DF, 0xF1FB, 0x003C, 0xF5FD, 0x626F,
        0x0006, 0xD1DD, 0xF4FB, 0xE6FF, 0x0018, 0x717D, 0x005A, 0xB1BF, 0x0006, 0x88AD, 0x008A, 0xD5DF, 0x002A,
        0x444D, 0xF2FD, 0x667F, 0x0006, 0x00CC, 0xF8FB, 0xE2EF, 0x0018, 0x545D, 0x004A, 0x119F, 0x0006, 0x009C,
        0x555B, 0xC8CF, 0xF1FB, 0x111D, 0xC8ED, 0x006E, 0x0006, 0xD4DD, 0xF4FB, 0xF3FF, 0x0018, 0x113D, 0x005A,
        0x11BF, 0x0006, 0x32BD, 0x008A, 0xD8DF, 0x002A, 0x515D, 0xAAFD, 0x222F, 0x0006, 0x44CD, 0xF8FB, 0x00FE,
        0x0018, 0x647D, 0x004A, 0x989F, 0x0006, 0x00AC, 0x555B, 0x00DE, 0xF1FB, 0x003C, 0xF5FD, 0x446F, 0x0006,
        0xD1DD, 0xF4FB, 0xB9FF, 0x0018, 0x717D, 0x005A, 0x00BE, 0x0006, 0x88AD, 0x008A, 0xDCDF, 0x002A, 0x444D,
        0xF2FD, 0x007E, 0x0006, 0x00CC, 0xF8FB, 0xE4EF, 0x0018, 0x545D, 0x004A, 0x737F, 0x0006, 0x009C, 0x555B,
        0xB8BF, 0xF1FB, 0x111D, 0xC8ED, 0x323F, 0x0004, 0x00AA, 0x0088, 0x407D, 0x0004, 0x10DD, 0x0028, 0x11DF,
        0x0004, 0x72FD, 0x0048, 0x005C, 0x0004, 0xA8AD, 0x0018, 0xB2BF, 0x0004, 0x009C, 0x0088, 0x006C, 0x0004,
        0x00CC, 0x0028, 0x00EE, 0x0004, 0xC8ED, 0x0048, 0x222D, 0x0004, 0x888D, 0x0018, 0x007E, 0x0004, 0x00AA,
        0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0x00FE, 0x0004, 0x91FD, 0x0048, 0x003C, 0x0004, 0xA2AD, 0x0018,
        0xAAAF, 0x0004, 0xB8FD, 0x0088, 0x005D, 0x0004, 0x00BD, 0x0028, 0xC4CF, 0x0004, 0x44ED, 0x0048, 0xF4FF,
        0x0004, 0x223D, 0x0018, 0x111F, 0x0004, 0x00AA, 0x0088, 0x407D, 0x0004, 0x10DD, 0x0028, 0x99FF, 0x0004,
        0x72FD, 0x0048, 0x005C, 0x0004, 0xA8AD, 0x0018, 0x00BE, 0x0004, 0x009C, 0x0088, 0x006C, 0x0004, 0x00CC,
        0x0028, 0x00DE, 0x0004, 0xC8ED, 0x0048, 0x222D, 0x0004, 0x888D, 0x0018, 0x444F, 0x0004, 0x00AA, 0x0088,
        0x006D, 0x0004, 0x88CD, 0x0028, 0xE2EF, 0x0004, 0x91FD, 0x0048, 0x003C, 0x0004, 0xA2AD, 0x0018, 0x447F,
        0x0004, 0xB8FD, 0x0088, 0x005D, 0x0004, 0x00BD, 0x0028, 0x009F, 0x0004, 0x44ED, 0x0048, 0x76FF, 0x0004,
        0x223D, 0x0018, 0x313F, 0x0006, 0x00CC, 0x008A, 0xD9FF, 0xF2FB, 0x647D, 0xF1FD, 0x99BF, 0x0006, 0xA2AD,
        0x002A, 0x66EF, 0xF4FB, 0x005C, 0xE2ED, 0x737F, 0x0006, 0x98BD, 0x004A, 0x00FE, 0xF8FB, 0x006C, 0x76FD,
        0x889F, 0x0006, 0x888D, 0x001A, 0xD5DF, 0x00AA, 0x222D, 0x98DD, 0x444F, 0x0006, 0xB2BD, 0x008A, 0xFCFF,
        0xF2FB, 0x226D, 0x009C, 0x00BE, 0x0006, 0xAAAD, 0x002A, 0xD1DF, 0xF4FB, 0x003C, 0xD4DD, 0x646F, 0x0006,
        0xA8AD, 0x004A, 0xEAEF, 0xF8FB, 0x445D, 0xE8ED, 0x717F, 0x0006, 0x323D, 0x001A, 0xC4CF, 0x00AA, 0xFAFF,
        0x88CD, 0x313F, 0x0006, 0x00CC, 0x008A, 0x77FF, 0xF2FB, 0x647D, 0xF1FD, 0xB3BF, 0x0006, 0xA2AD, 0x002A,
        0x00EE, 0xF4FB, 0x005C, 0xE2ED, 0x007E, 0x0006, 0x98BD, 0x004A, 0xE4EF, 0xF8FB, 0x006C, 0x76FD, 0x667F,
        0x0006, 0x888D, 0x001A, 0x00DE, 0x00AA, 0x222D, 0x98DD, 0x333F, 0x0006, 0xB2BD, 0x008A, 0x75FF, 0xF2FB,
        0x226D, 0x009C, 0x919F, 0x0006, 0xAAAD, 0x002A, 0x99DF, 0xF4FB, 0x003C, 0xD4DD, 0x515F, 0x0006, 0xA8AD,
        0x004A, 0xECEF, 0xF8FB, 0x445D, 0xE8ED, 0x727F, 0x0006, 0x323D, 0x001A, 0xB1BF, 0x00AA, 0xF3FF, 0x88CD,
        0x111F, 0x0006, 0x54DD, 0xF2FB, 0x111D, 0x0018, 0x647D, 0xF8FD, 0xCCCF, 0x0006, 0x91BD, 0x004A, 0x22EF,
        0x002A, 0x222D, 0xF3FD, 0x888F, 0x0006, 0x00CC, 0x008A, 0x00FE, 0x0018, 0x115D, 0xFCFD, 0xA8AF, 0x0006,
        0x00AC, 0x003A, 0xC8DF, 0xF1FB, 0x313D, 0x66FD, 0x646F, 0x0006, 0xC8CD, 0xF2FB, 0xF5FF, 0x0018, 0x006C,
        0xF4FD, 0xBABF, 0x0006, 0x22AD, 0x004A, 0x00EE, 0x002A, 0x323D, 0xEAFD, 0x737F, 0x0006, 0xB2BD, 0x008A,
        0x55DF, 0x0018, 0x005C, 0x717D, 0x119F, 0x0006, 0x009C, 0x003A, 0xC4CF, 0xF1FB, 0x333D, 0xE8ED, 0x444F,
        0x0006, 0x54DD, 0xF2FB, 0x111D, 0x0018, 0x647D, 0xF8FD, 0x99BF, 0x0006, 0x91BD, 0x004A, 0xE2EF, 0x002A,
        0x222D, 0xF3FD, 0x667F, 0x0006, 0x00CC, 0x008A, 0xE4EF, 0x0018, 0x115D, 0xFCFD, 0x989F, 0x0006, 0x00AC,
        0x003A, 0x00DE, 0xF1FB, 0x313D, 0x66FD, 0x226F, 0x0006, 0xC8CD, 0xF2FB, 0xB9FF, 0x0018, 0x006C, 0xF4FD,
        0x00BE, 0x0006, 0x22AD, 0x004A, 0xD1DF, 0x002A, 0x323D, 0xEAFD, 0x007E, 0x0006, 0xB2BD, 0x008A, 0xECEF,
        0x0018, 0x005C, 0x717D, 0x727F, 0x0006, 0x009C, 0x003A, 0xB8BF, 0xF1FB, 0x333D, 0xE8ED, 0x545F, 0xF1F9,
        0xD1DD, 0xFAFB, 0x00DE, 0xF8F9, 0x001C, 0xFFFB, 0x747F, 0xF4F9, 0x717D, 0xF3FB, 0xB3BF, 0xF2F9, 0xEAEF,
        0xE8ED, 0x444F, 0xF1F9, 0x22AD, 0x000A, 0xB8BF, 0xF8F9, 0x00FE, 0xFCFD, 0x007E, 0xF4F9, 0x115D, 0xF5FB,
        0x757F, 0xF2F9, 0xD8DF, 0xE2ED, 0x333F, 0xF1F9, 0xB2BD, 0xFAFB, 0x88CF, 0xF8F9, 0xFBFF, 0xFFFB, 0x737F,
        0xF4F9, 0x006D, 0xF3FB, 0x00BE, 0xF2F9, 0x66EF, 0xF9FD, 0x313F, 0xF1F9, 0x009D, 0x000A, 0xBABF, 0xF8F9,
        0xFDFF, 0xF6FD, 0x006E, 0xF4F9, 0x002C, 0xF5FB, 0x888F, 0xF2F9, 0xDCDF, 0xD4DD, 0x222F, 0xF1F9, 0xD1DD,
        0xFAFB, 0xC4CF, 0xF8F9, 0x001C, 0xFFFB, 0x727F, 0xF4F9, 0x717D, 0xF3FB, 0x99BF, 0xF2F9, 0xECEF, 0xE8ED,
        0x004E, 0xF1F9, 0x22AD, 0x000A, 0x00AE, 0xF8F9, 0xF7FF, 0xFCFD, 0x005E, 0xF4F9, 0x115D, 0xF5FB, 0x009E,
        0xF2F9, 0xD5DF, 0xE2ED, 0x003E, 0xF1F9, 0xB2BD, 0xFAFB, 0x00CE, 0xF8F9, 0xFEFF, 0xFFFB, 0x667F, 0xF4F9,
        0x006D, 0xF3FB, 0xA8AF, 0xF2F9, 0x00EE, 0xF9FD, 0x323F, 0xF1F9, 0x009D, 0x000A, 0xB1BF, 0xF8F9, 0xE4EF,
        0xF6FD, 0x545F, 0xF4F9, 0x002C, 0xF5FB, 0x008E, 0xF2F9, 0x99DF, 0xD4DD, 0x111F};

// LUT for UVLC decoding in initial line-pair
//   index (8bits) : [bit   7] u_off_1 (1bit)
//                   [bit   6] u_off_0 (1bit)
//                   [bit 5-0] LSB bits from VLC codeword
//   the index is incremented by 64 when both u_off_0 and u_off_1 are 0
//
//   output        : [bit 0-2] length of prefix (l_p) for quads 0 and 1
//                 : [bit 3-6] length of suffix (l_s) for quads 0 and 1
//                 : [bit 7-9] ength of suffix (l_s) for quads 0
static const alignas(32) uint16_t uvlc_dec_0[256 + 64] = {
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x16ab,
        0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401,
        0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802,
        0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401,
        0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b,
        0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0xa02b, 0x2001,
        0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002,
        0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001,
        0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b,
        0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001,
        0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0x36ac, 0xa42c, 0xa82d,
        0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402, 0x56ac, 0x640c, 0x4804, 0x2402, 0x4c8c, 0x4403, 0x2803, 0x2402,
        0x36ac, 0xa42c, 0x680d, 0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402, 0x56ac, 0x640c, 0x4804, 0x2402, 0x4c8c,
        0x4403, 0x2803, 0x2402, 0x36ac, 0xa42c, 0xa82d, 0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402, 0x56ac, 0x640c,
        0x4804, 0x2402, 0x4c8c, 0x4403, 0x2803, 0x2402, 0x36ac, 0xa42c, 0x680d, 0x2402, 0x2c8c, 0x4403, 0x2803,
        0x2402, 0x56ac, 0x640c, 0x4804, 0x2402, 0x4c8c, 0x4403, 0x2803, 0x2402, 0xfed6, 0xec2c, 0xf02d, 0x6c02,
        0xf4b6, 0x8c03, 0x7003, 0x6c02, 0x7eac, 0xac0c, 0x9004, 0x6c02, 0x748c, 0x8c03, 0x7003, 0x6c02, 0x9ead,
        0xec2c, 0xb00d, 0x6c02, 0x948d, 0x8c03, 0x7003, 0x6c02, 0x7eac, 0xac0c, 0x9004, 0x6c02, 0x748c, 0x8c03,
        0x7003, 0x6c02, 0xbeb6, 0xec2c, 0xf02d, 0x6c02, 0xb496, 0x8c03, 0x7003, 0x6c02, 0x7eac, 0xac0c, 0x9004,
        0x6c02, 0x748c, 0x8c03, 0x7003, 0x6c02, 0x9ead, 0xec2c, 0xb00d, 0x6c02, 0x948d, 0x8c03, 0x7003, 0x6c02,
        0x7eac, 0xac0c, 0x9004, 0x6c02, 0x748c, 0x8c03, 0x7003, 0x6c02};

// LUT for UVLC decoding in non-initial line-pair
//   index (8bits) : [bit   7] u_off_1 (1bit)
//                   [bit   6] u_off_0 (1bit)
//                   [bit 5-0] LSB bits from VLC codeword
//
//   output        : [bit 0-2] length of prefix (l_p) for quads 0 and 1
//                 : [bit 3-6] length of suffix (l_s) for quads 0 and 1
//                 : [bit 7-9] ength of suffix (l_s) for quads 0
static const alignas(32) uint16_t uvlc_dec_1[256] = {
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
        0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x16ab,
        0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401,
        0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802,
        0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401,
        0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b,
        0x0401, 0x0802, 0x0401, 0x16ab, 0x0401, 0x0802, 0x0401, 0x0c8b, 0x0401, 0x0802, 0x0401, 0xa02b, 0x2001,
        0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002,
        0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001,
        0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b,
        0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001,
        0x4002, 0x2001, 0xa02b, 0x2001, 0x4002, 0x2001, 0x600b, 0x2001, 0x4002, 0x2001, 0xb6d6, 0xa42c, 0xa82d,
        0x2402, 0xacb6, 0x4403, 0x2803, 0x2402, 0x36ac, 0x640c, 0x4804, 0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402,
        0x56ad, 0xa42c, 0x680d, 0x2402, 0x4c8d, 0x4403, 0x2803, 0x2402, 0x36ac, 0x640c, 0x4804, 0x2402, 0x2c8c,
        0x4403, 0x2803, 0x2402, 0x76b6, 0xa42c, 0xa82d, 0x2402, 0x6c96, 0x4403, 0x2803, 0x2402, 0x36ac, 0x640c,
        0x4804, 0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402, 0x56ad, 0xa42c, 0x680d, 0x2402, 0x4c8d, 0x4403, 0x2803,
        0x2402, 0x36ac, 0x640c, 0x4804, 0x2402, 0x2c8c, 0x4403, 0x2803, 0x2402};

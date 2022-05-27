/*
 * JPEG2000 High Throughput block decoder
 * Copyright (c) 2022 Caleb Etemesi<etemesicaleb@gmail.com>
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

/**
 * @file
 * JPEG2000 High Throughtput block decoder
 */

#include "jpeg2000_htj2k.h"
#include "bytestream.h"
#include <libavutil/common.h>
#include <libavutil/log.h>
#include <libavutil/mem.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/* Initialize State variables to zero */
static void jpeg2000_init_zero(StateVars *s)
{
    s->bits_left = 0;
    s->bit_buf = 0;
    s->tmp = 0;
    s->bits = 0;
    s->tmp = 0;
    s->last = 0;
}
/*Initialize MEL bit stream*/
static void jpeg2000_init_mel(StateVars *s, uint32_t Pcup)
{
    jpeg2000_init_zero(s);
    s->pos = Pcup;
}

static void jpeg2000_init_vlc(StateVars *s, uint32_t Lcup, uint32_t Pcup, const uint8_t *Dcup)
{
    s->bits_left = 0;
    s->bit_buf = 0;
    s->pos = Lcup - 2 - Pcup;
    s->last = Dcup[Lcup - 2];
    s->tmp = (s->last) >> 4;
    s->bits = ((s->tmp & 7) < 7) ? 4 : 3;
}

static void jpeg2000_init_mag_ref(StateVars *s, uint32_t Lref)
{
    s->pos = Lref - 1;
    s->bits = 0;
    s->last = 0xFF;
    s->tmp = 0;
}

void jpeg2000_init_mel_decoder(MelDecoderState *mel_state)
{
    mel_state->k = 0;
    mel_state->run = 0;
    mel_state->one = 0;
}

int jpeg2000_bitbuf_refill_backwards(StateVars *buffer, const uint8_t *array)
{
    uint64_t tmp = 0;
    int32_t position = buffer->pos;
    int new_bits = 32;

    // TODO: (cae), confirm if we need to swap in BE systems.
    if (buffer->bits_left > 32)
        return 0; // enough data, no need to pull in more bits

    if (position >= 3) {
        position -= 4;
        memcpy(&tmp, array + position + 1, 4);
        tmp = (uint64_t)av_bswap32((uint32_t)tmp);
    } else if (position == 2) {
        position = 0;
        memcpy(&tmp, array + position + 1, 3);
        tmp = (uint64_t)av_bswap32((uint32_t)tmp);
        new_bits -= 8;
    } else if (position == 1) {
        position = 0;
        memcpy(&tmp, array + position + 1, 2);
        tmp = (uint64_t)av_bswap16((uint16_t)tmp);
        buffer->bits_left += 16;
        new_bits -= 16;
    } else if (position == 0) {
        position = -1;
        memcpy(&tmp, array + position + 1, 1);
        buffer->bits_left += 8;
        new_bits -= 24;
    } else {
        printf("No bytes left");
        return 1;
    }
    // check for stuff bytes (0xff)
    if (has_byte(tmp, 0xff)) {
        // borrowed from open_htj2k ht_block_decoding.cpp

        // TODO(cae): confirm this is working

        // Load the next byte to check for stuffing.
        tmp <<= 8;
        tmp |= (uint64_t) * (array + position);
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
        // remove temporary byte loaded.
        tmp >>= 8;
    }
    // Add bits to the MSB of the bit buffer
    buffer->bit_buf |= tmp << buffer->bits_left;
    buffer->bits_left += new_bits;
    buffer->pos = position;
    return 0;
}
void jpeg2000_bitbuf_drop_bits(StateVars *buf, uint8_t nbits)
{
    av_assert0(buf->bits_left >= nbits);
    buf->bit_buf >>= nbits;
    buf->bits_left -= nbits;
}


uint64_t jpeg2000_bitbuf_get_bits(StateVars *bit_stream, uint8_t nbits, const uint8_t *buf)
{

    uint64_t bits;
    uint64_t mask = (1 << nbits) - 1;
    if (bit_stream->bits_left < nbits)
        // TODO: (cae) this may fail I  guess if there are no more bits,
        // add a check for it.
        jpeg2000_bitbuf_refill_backwards(bit_stream, buf);

    bits = bit_stream->bit_buf & mask;

    jpeg2000_bitbuf_drop_bits(bit_stream, nbits);
    return bits;
};

uint64_t jpeg2000_bitbuf_peek_bits(StateVars *stream, uint8_t nbits)
{
    uint64_t mask = (1 << nbits) - 1;

    return stream->bit_buf & mask;
}

int jpeg2000_decode_mel_sym(MelDecoderState *mel_state, StateVars *mel_stream, const uint8_t *Dcup, uint32_t Lcup)
{

    if (mel_state->run == 0 && mel_state->one == 0) {
        uint8_t eval;
        uint8_t bit;

        eval = MEL_E[mel_state->k];
        bit = jpeg2000_import_mel_bit(mel_stream, Dcup, Lcup);
        if (bit == 1) {
            mel_state->run = 1 << eval;
            mel_state->k = MIN(12, mel_state->k + 1);
        } else {
            mel_state->run = 0;
            while (eval > 0) {
                bit = jpeg2000_import_mel_bit(mel_stream, Dcup, Lcup);
                mel_state->run = (2 * (mel_state->run)) + bit;
                eval -= 1;
            }
            mel_state->k = MAX(0, mel_state->k - 1);
            mel_state->one = 1;
        }
    }
    if (mel_state->run > 0) {
        mel_state->run -= 1;
        return 0;
    } else {
        mel_state->one = 0;
        return 1;
    }
}

int jpeg2000_import_mel_bit(StateVars *mel_stream, const uint8_t *Dcup, uint32_t Lcup)
{
    if (mel_stream->bits == 0) {
        mel_stream->bits = (mel_stream->tmp == 0xFF) ? 7 : 8;
        if (mel_stream->pos < Lcup) {
            mel_stream->tmp = Dcup[mel_stream->pos];
            mel_stream->pos += 1;
        } else
            mel_stream->tmp = 0xFF;
    }
    mel_stream->bits -= 1;

    return (mel_stream->tmp >> mel_stream->bits) & 1;
}

int jpeg2000_decode_ctx_vlc(Jpeg2000DecoderContext *s, StateVars *vlc_stream, const uint16_t *table, const uint8_t *Dcup, uint8_t *sig_pat, uint8_t *res_off, uint8_t *emb_pat_k, uint8_t *emb_pat_1, uint8_t pos, uint32_t Pcup, uint16_t context)
{
    uint32_t value;
    uint8_t len;
    int index;

    jpeg2000_bitbuf_refill_backwards(vlc_stream, Dcup + Pcup);

    int code_word = vlc_stream->bit_buf & 0x7f;
    index = code_word + (context << 7);

    // decode table has 1024 entries so ensure array access is in bounds
    av_assert0(index < 1024);

    value = table[index];
    len = (value & 0x000F) >> 1;

    res_off[pos] = (uint8_t)(value & 1);
    sig_pat[pos] = (uint8_t)((value & 0x00F0) >> 4);
    emb_pat_k[pos] = (uint8_t)((value & 0x0F00) >> 8);
    emb_pat_1[pos] = (uint8_t)((value & 0xF000) >> 12);

    jpeg2000_bitbuf_drop_bits(vlc_stream, len);

    return 0;
}
 uint8_t vlc_decode_u_prefix(StateVars *vlc_stream, const uint8_t *refill_array)
{
    uint8_t bits = jpeg2000_bitbuf_peek_bits(vlc_stream, 3);

    if (bits & 0b1) {
        jpeg2000_bitbuf_drop_bits(vlc_stream, 1);
        return 1;
    }
    if (bits & 0b10) {
        jpeg2000_bitbuf_drop_bits(vlc_stream, 2);
        return 2;
    }
    jpeg2000_bitbuf_drop_bits(vlc_stream, 3);

    if (bits & 0b100)
        return 3;
    else
        return 5;
}

uint8_t vlc_decode_u_suffix(StateVars *vlc_stream, uint8_t prefix, const uint8_t *refill_array)
{
    uint8_t bits;
    if (prefix < 3)
        return 0;

    if (vlc_stream->bits_left < 5)
        jpeg2000_bitbuf_refill_backwards(vlc_stream, refill_array);

    bits = jpeg2000_bitbuf_peek_bits(vlc_stream, 5);

    if (prefix == 3) {
        jpeg2000_bitbuf_drop_bits(vlc_stream, 1);
        return bits & 1;
    }
    jpeg2000_bitbuf_drop_bits(vlc_stream, 5);

    return bits;
}



int jpeg2000_decode_sig_emb(Jpeg2000DecoderContext *s, MelDecoderState *mel_state, StateVars *mel_stream, StateVars *vlc_stream, const uint16_t *vlc_table, const uint8_t *Dcup, uint8_t *sig_pat, uint8_t *res_off, uint8_t *emb_pat_k, uint8_t *emb_pat_1, uint8_t pos, uint16_t q, uint16_t context, uint32_t Lcup, uint32_t Pcup)
{
    if (context == 0) {
        uint8_t sym;
        sym = jpeg2000_decode_mel_sym(mel_state, mel_stream, Dcup, Lcup);
        if (sym == 0) {
            sig_pat[pos] = 0;
            res_off[pos] = 0;
            emb_pat_k[pos] = 0;
            emb_pat_1[pos] = 0;
            return 0;
        }
    }
    return jpeg2000_decode_ctx_vlc(s, vlc_stream, vlc_table, Dcup, sig_pat, res_off, emb_pat_k, emb_pat_1, pos, Pcup, context);
}

int jpeg2000_decode_ht_cleanup(Jpeg2000DecoderContext *s, Jpeg2000Cblk *cblk, MelDecoderState *mel_state, StateVars *mel_stream, StateVars *vlc_stream, const uint8_t *Dcup, uint32_t Lcup, uint32_t Pcup, int width, int height)
{

    uint16_t q = 0; // Represents current quad position.
    uint16_t q1, q2;

    uint8_t sig_pat[2];   // significance pattern
    uint8_t res_off[2];   // residual offset
    uint8_t emb_pat_k[2]; // Exponent Max Bound pattern K
    uint8_t emb_pat_1[2]; // Exponent Max Bound pattern 1.

    uint8_t u_prefix[2];
    uint8_t u_suffix[2];

    const uint16_t *vlc_table;

    uint16_t context = 0;

    const uint16_t quad_width = ff_jpeg2000_ceildivpow2(width, 1);
    const uint16_t quad_height = ff_jpeg2000_ceildivpow2(height, 1);

    size_t buf_size = 4 * quad_width * quad_height;

    uint8_t *sigma_n = av_calloc(buf_size, sizeof(uint8_t));

    if (!sigma_n) {
        av_log(s->avctx, AV_LOG_ERROR, "Could not allocate %zu bytes for sigma_n buffer", buf_size);
        goto error;
    }
    while (q < quad_width - 1) {
        q1 = q;
        q2 = q1 + 1;
        // if (q < QW) -> table=CtxVLC_table_0
        vlc_table = dec_CxtVLC_table0;
        if (jpeg2000_decode_sig_emb(s, mel_state, mel_stream, vlc_stream, vlc_table, Dcup, sig_pat, res_off, emb_pat_k, emb_pat_1, J2K_FIRST_QUAD, q, context, Lcup, Pcup) == -1)
            goto error;

        for (int i = 0; i < 4; i++)
            sigma_n[4 * q1 + i] = (sig_pat[J2K_FIRST_QUAD] >> i) & 1;

        // calculate context
        context = sigma_n[4 * q1];           // f
        context |= sigma_n[4 * q1 + 1];      // sf
        context += sigma_n[4 * q1 + 2] << 1; // w << 1
        context += sigma_n[4 * q1 + 3] << 2;

        if (jpeg2000_decode_sig_emb(s, mel_state, mel_stream, vlc_stream, vlc_table, Dcup, sig_pat, res_off, emb_pat_k, emb_pat_1, J2K_SECOND_QUAD, q, context, Lcup, Pcup) == -1)
            goto error;

        for (int i = 0; i < 4; i++)
            sigma_n[4 * q2 + i] = (sig_pat[J2K_SECOND_QUAD] >> i) & 1;

        // calculate context for the next quad
        context = sigma_n[4 * q2];           // f
        context |= sigma_n[4 * q2 + 1];      // sf
        context += sigma_n[4 * q2 + 2] << 1; // w << 1
        context += sigma_n[4 * q2 + 3] << 2; // sw << 2

        if (res_off[J2K_FIRST_QUAD] == 1 && res_off[J2K_SECOND_QUAD] == 1) {
            if (jpeg2000_decode_mel_sym(mel_state, mel_stream, Dcup, Lcup) == 1) {
                printf("MEL DECODER HERE\n");
                // max bits needed before refilling
                // 6 -> 3 bits per each vlc_decode_u_prefix() ( 2 instances)
                // 10 -> 5 bits per each vlc_decode_u_suffix() ( 2 instances)
                // 8  -> 4 bits per each vlc_decode_u_extension ( 2 instances ) (might be 3 confirm)
                //
                // TODO :(cae) might optimize this to remove refill checks inside vlc_decode_u_prefix/suffix if need be.
                //
                if (vlc_stream->bits_left < 26)
                    jpeg2000_bitbuf_refill_backwards(vlc_stream, Dcup + Pcup);

                u_prefix[J2K_FIRST_QUAD] = vlc_decode_u_prefix(vlc_stream, Dcup + Pcup);
                u_prefix[J2K_SECOND_QUAD] = vlc_decode_u_prefix(vlc_stream, Dcup + Pcup);

                printf("u_prefix 1:%d u_prefix 2: %d\n", u_prefix[0], u_prefix[1]);

                u_suffix[J2K_FIRST_QUAD] = vlc_decode_u_suffix(vlc_stream, u_prefix[J2K_FIRST_QUAD], Dcup + Pcup);
                u_suffix[J2K_SECOND_QUAD] = vlc_decode_u_suffix(vlc_stream, u_prefix[J2K_SECOND_QUAD], Dcup + Pcup);

                printf("u_prefix 1:%d u_prefix 2: %d\n", u_suffix[0], u_suffix[1]);
            }
        }

        exit(1);
        // prepare context for the next quad

        // move to the next quad pair
        q += 2;
    }

    av_freep(sigma_n);
    return 1;

error:
    av_freep(sigma_n);
    return 0;
}

int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift)
{
    /* (cae) : Notes in no order about the code structure
     *
     * cblk -> Values mainly filled in  jpeg2000dec.c/jpeg2000_decode_packet.
     *
     * */
    uint8_t p0 = 0;    // Number of placeholder passes.
    uint32_t Lcup;     // Length of HT cleanup segment.
    uint32_t Lref = 0; // Length of Refinement segment.
    uint32_t Scup;     // HT cleanup segment suffix length.
    uint32_t Pcup;     // HT cleanup segment prefix length.

    uint8_t *Dcup; // Byte of an HT cleanup segment.
    uint8_t *Dref; // Byte of an HT refinement segment.

    int z_blk; // Number of ht coding pass
    uint8_t empty_passes;

    StateVars mag_sgn;  // Magnitude and Sign
    StateVars mel;      // Adaptive run-length coding
    StateVars vlc;      // Variable Length coding
    StateVars sig_prop; // Significance propagation
    StateVars mag_ref;  // Magnitude and refinement.

    MelDecoderState mel_state;

    int ret;

    if (cblk->npasses == 0) {
        return 0;
    }

    if (cblk->npasses > 3)
        // TODO:(cae) Add correct support for this
        // Currently use this as a dummy but should be fixed soon
        p0 = 0;
    else if (cblk->length == 0)
        p0 = 1;

    empty_passes = p0 * 3;
    z_blk = cblk->npasses - empty_passes;

    if (z_blk <= 0)
        // no passes within this set, continue
        return 0;

    Lcup = cblk->length;
    if (Lcup < 2) {
        av_log(s->avctx, AV_LOG_ERROR, "Cleanup pass length must be at least 2 bytes in length");
        return AVERROR_INVALIDDATA;
    }
    Dcup = cblk->data;
    // Dref comes after the refinement segment.
    Dref = cblk->data + Lcup;

    Scup = (Dcup[Lcup - 1] << 4) | (Dcup[Lcup - 2] & 0x0F);

    if (Scup < 2 || Scup > Lcup || Scup > 4079) {
        av_log(s->avctx, AV_LOG_ERROR, "Cleanup pass suffix length is invalid %d", Scup);
        return AVERROR_INVALIDDATA;
    }

    Pcup = Lcup - Scup;
    // TODO (cae): Correctly decode the Lref value.

    // modDcup (shall be done before the creation of state_VLC instance)
    Dcup[Lcup - 1] = 0xFF;
    Dcup[Lcup - 2] |= 0x0F;

    jpeg2000_init_zero(&mag_sgn);
    jpeg2000_init_zero(&sig_prop);

    jpeg2000_init_mel(&mel, Pcup);
    jpeg2000_init_vlc(&vlc, Lcup, Pcup, Dcup);

    jpeg2000_bitbuf_refill_backwards(&vlc, Dcup + Pcup);
    jpeg2000_bitbuf_drop_bits(&vlc, 4);

    jpeg2000_init_mag_ref(&mag_ref, Lref);

    jpeg2000_init_mel_decoder(&mel_state);

    ret = jpeg2000_decode_ht_cleanup(s, cblk, &mel_state, &mel, &vlc, Dcup, Lcup, Pcup, width, height);

    return ret;
}
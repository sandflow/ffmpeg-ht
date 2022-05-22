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
#include <libavutil/avassert.h>
#include <libavutil/log.h>
#include <libavutil/mem.h>
#include <stddef.h>
#include <stdint.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/* Initialize State variables to zero */
static void init_zero(StateVars *s)
{
    s->tmp = 0;
    s->bits = 0;
    s->tmp = 0;
    s->last = 0;
}
/*Initialize MEL bit stream*/
static void init_mel(StateVars *s, uint32_t Pcup)
{
    init_zero(s);
    s->pos = Pcup;
}

static void init_vlc(StateVars *s, uint32_t Lcup, uint8_t *Dcup)
{
    s->tmp = Lcup - 3;
    // Vlc_Last = modDcup(Dcup, Lcup-2)
    // which becomes  Dcup[pos] | 0x0F
    s->last = Dcup[Lcup - 2] | 0x0F;
    s->tmp = (s->last) >> 4;
    s->bits = ((s->tmp & 7) < 7) ? 4 : 3;
}

static void init_mag_ref(StateVars *s, uint32_t Lref)
{
    s->pos = Lref - 1;
    s->bits = 0;
    s->last = 0xFF;
    s->tmp = 0;
}

static void jpeg2000_init_byte_buf(Jpeg2000ByteBuffer *buffer, GetByteContext *b)
{
    buffer->bit_buf = 0;
    buffer->bits_left = 0;
    buffer->src = b;
}

static void jpeg2000_init_mel_decoder(MelDecoderState *mel_state)
{
    mel_state->k = 0;
    mel_state->run = 0;
    mel_state->one = 0;
}

static int jpeg2000_refill_and_unsfuff(Jpeg2000DecoderContext *s, Jpeg2000ByteBuffer *buffer)
{
    uint64_t tmp; // temporary storage for bytes
    size_t bytes_left;

    if (buffer->bits_left > 32) {
        return 0; // enough data, no need to pull in more bits
    }

    bytes_left = bytestream2_get_bytes_left(buffer->src);

    if (bytes_left > 4) {
        tmp = bytestream2_get_be32u(buffer->src);
        buffer->bits_left += 32;
    } else if (bytes_left == 3) {
        tmp = bytestream2_get_be24u(buffer->src);
        buffer->bits_left += 24;
    } else if (bytes_left == 2) {
        tmp = bytestream2_get_be16u(buffer->src);
        buffer->bits_left += 16;
    } else if (bytes_left == 1) {
        tmp = bytestream2_get_byteu(buffer->src);
        buffer->bits_left += 8;
    } else {
        av_log(s->avctx, AV_LOG_ERROR, "No more bytes left\n");
        return 1;
    }

    // check for stuff bytes (0xff)
    if (has_byte(tmp, 0xff)) {
        // 0xff exists, check for stuffing.

        // borrowed from open_htj2k ht_block_decoding.cpp
        // thanks Osamu Watanabe.

        // Load the next byte to check for stuffing.
        tmp <<= 8;
        tmp |= bytestream2_peek_byte(buffer->src);

        if ((tmp & 0x7FFF000000) > 0x7F8F000000) {
            tmp &= 0x7FFFFFFFFF;
            buffer->bits_left--;
        }
        if ((tmp & 0x007FFF0000) > 0x007F8F0000) {
            tmp = (tmp & 0x007FFFFFFF) + ((tmp & 0xFF00000000) >> 1);
            buffer->bits_left--;
        }
        if ((tmp & 0x00007FFF00) > 0x00007F8F00) {
            tmp = (tmp & 0x00007FFFFF) + ((tmp & 0xFFFF000000) >> 1);
            buffer->bits_left--;
        }
        if ((tmp & 0x0000007FFF) > 0x0000007F8F) {
            tmp = (tmp & 0x0000007FFF) + ((tmp & 0xFFFFFF0000) >> 1);
            buffer->bits_left--;
        }
        // remove temporary byte loaded.
        tmp >>= 8;
    }
    // Add bits to the MSB of the bit buffer
    buffer->bit_buf |= (uint64_t)tmp << (64 - buffer->bits_left);
    return 0;
}

static int jpeg2000_decode_mel_sym(MelDecoderState *mel_state, StateVars *mel_stream, const uint8_t *Dcup, uint32_t Lcup)
{

    if (mel_state->run == 0 && mel_state->one == 0) {
        uint8_t eval;
        uint8_t bit;

        eval = MEL_E[mel_state->k];
        bit = jpeg2000_import_mel_bit(mel_stream, Dcup, Lcup);
        if (bit == 1) {
            mel_state->run = 1 << eval;
            mel_state->k = MIN(12,mel_state->k + 1);
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

static int jpeg2000_import_mel_bit(StateVars *mel_stream, const uint8_t *Dcup, uint32_t Lcup)
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

static int jpeg2000_decode_sig_emb(MelDecoderState *mel_state, StateVars *mel_stream, uint8_t *Dcup, uint16_t q, uint16_t context, uint32_t Lcup)
{
    uint8_t sym;
    if (context == 0) {
        sym = jpeg2000_decode_mel_sym(mel_state, mel_stream, Dcup, Lcup);
        if (sym == 0) {
            //
        }
    }
    return 0;
}

static int jpeg2000_decode_ht_cleanup(Jpeg2000DecoderContext *s, Jpeg2000Cblk *cblk, uint8_t *Dcup, uint32_t Lcup, uint32_t Scup, int width, int height)
{

    uint16_t q = 0; // Represents one quad.
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
    }

    av_freep(sigma_n);
    return 0;

error:
    av_freep(sigma_n);
    return 1;
}

int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift)
{
    /* (cae) : Notes in no order about the code structure
     *
     * cblk -> Values mainly filled in  jpeg2000dec.c/jpeg2000_decode_packet.
     *
     * */
    uint8_t p0 = 0;    // Number of placeholder passes.
    uint32_t Lcup = 0; // Length of HT cleanup segment.
    uint32_t Lref = 0; // Length of Refinement segment.
    uint32_t Scup = 0; // HT cleanup segment suffix length.
    uint32_t Pcup = 0; // HT cleanup segment prefix length.

    uint8_t *Dcup; // Byte of an HT cleanup segment.
    uint8_t *Dref; // Byte of an HT refinement segment.

    int z_blk; // Number of ht coding pass
    uint8_t empty_passes;

    StateVars mag_sgn;  // Magnitude and Sign
    StateVars mel;      // Adaptive run-length coding
    StateVars vlc;      // Variable Length coding
    StateVars sig_prop; // Significance propagation
    StateVars mag_ref;  // Magnitude and refinement.

    if (cblk->npasses == 0) {
        return 0;
    }
    av_log(s->avctx, AV_LOG_TRACE, "Initializing HTJ2K decoder\n");

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

    init_zero(&mag_sgn);
    init_zero(&sig_prop);

    init_mel(&mel, Pcup);
    init_vlc(&vlc, Lcup, Dcup);

    init_mag_ref(&mag_ref, Lref);

    jpeg2000_decode_ht_cleanup(s, cblk, Dcup, Lcup, Scup, width, height);

    return 1;
}
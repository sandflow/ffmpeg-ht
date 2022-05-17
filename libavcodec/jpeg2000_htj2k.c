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

static int jpeg2000_init_byte_buf(Jpeg2000ByteBuffer *buffer, GetByteContext *b)
{
    buffer->bit_buf = 0;
    buffer->bits_left = 0;
    buffer->src = b;
    return 0;
}

static int jpeg2000_refill_and_unsfuff(Jpeg2000DecoderContext *s,Jpeg2000ByteBuffer *buffer)
{
    uint64_t tmp; // temporary storage for bytes
    size_t bytes_left;
    uint8_t initial_bits;

    if (buffer->bits_left > 32) {
        return 0; // enough data, no need to pull in more bits
    }

    bytes_left = buffer->src->buffer_end - buffer->src->buffer_start;

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
        tmp = bytestream2_peek_byte(buffer->src);

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

int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift)
{
    /* (cae) : Notes in no order about the code structure
     *
     * cblk -> Values mainly filled in  jpeg2000dec.c/jpeg2000_decode_packet.
     *
     * */
    uint8_t p0 = 0;          // Number of placeholder passes.
    uint32_t Lcup = 0;       // Length of HT cleanup segment.
    uint32_t Lref = 0;       // Length of Refinement segment.
    const uint8_t Sskip = 0; // Number of HT Sets preceding the given set.
    int z_blk;               // Number of ht coding pass
    uint8_t empty_passes;

    if (cblk->npasses==0){
        return 0;
    }
    av_log(s->avctx, AV_LOG_TRACE, "Initializing HTJ2K decoder\n");

    if (cblk->npasses > 3)
        // TODO:(cae) Add correct support for this
        // Currently use this as a dummy but should be fixed soon
        p0 = 0;
    else if (cblk->length == 0 && cblk->npasses != 0)
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


    return 1;
}
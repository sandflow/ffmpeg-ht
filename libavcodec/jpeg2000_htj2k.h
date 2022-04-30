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
#include <stdint.h>

#include "bytestream.h"


/**
 * Determine if a word has a zero byte
 *
 * @param dword value to check if it contains  zeroe bytes
 *
 * @returns a number whose high bit is set if a byte at a position  is equal to zero
 * */
static uint32_t has_zero(uint32_t dword){
    // Borrowed from the famous stanford bithacks page
    // see https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
    return ~((((dword & 0x7F7F7F7F) + 0x7F7F7F7F) | dword) | 0x7F7F7F7F);
}
/**
 * Determine if a word has a byte equal to `byte`
 *
 * @param dword value to check if it contains some bytes
 * @param bytes the needle we are looking for in the haystack
 * */
static uint32_t has_byte(uint32_t dword,uint8_t byte)
{
    return has_zero(dword  ^ (~0UL/255 * (byte)));
}



typedef  struct Jpeg2000ByteBuffer{
    uint8_t bits_left;   //number of bits remaining in the byte buffer
    uint64_t  buf;      // actual byte buffer
} Jpeg2000ByteBuffer;

static int jpeg2000_init_byte_buf(Jpeg2000ByteBuffer *buffer){
    buffer->bits_left=0;
    buffer->buf = 0;
    return 0;
}

/**
 * Refill the bit buffer with new bytes unstuffing bits if needed
 *
 * @param buffer THe current bit-buffer where we are adding new bits
 * @param b The source where we are pulling bits
 * */
static int jpeg2000_refill_and_unsfuff(Jpeg2000ByteBuffer *buffer, GetByteContext *b){
    uint64_t  tmp;  // temporary storage for bytes
    size_t    bytes_left;
    uint8_t   initial_bits;

    if (buffer->bits_left>32){
        return 0; // enough data, no need to pull in more bits
    }
    bytes_left = b->buffer_end - b->buffer_start;

    if (bytes_left > 4){
        tmp = bytestream2_get_be32u(b);
        buffer->bits_left+=32;
    }
    else if (bytes_left==3){
        tmp = bytestream2_get_be24u(b);
        buffer->bits_left+=24;
    }
    else if (bytes_left==2){
        tmp = bytestream2_get_be16u(b);
        buffer->bits_left+=16;
    }
    else{
        tmp = bytestream2_get_byteu(b);
        buffer->bits_left+=8;
    }

    // check for stuff bytes (0xff)
    if (has_byte(tmp, 0xff)){
        // 0xff exists, check for stuffing.

        // borrowed from open_htj2k ht_block_decoding.cpp
        // thanks Osamu Watanabe.

        // Load the next byte to check for stuffing.
        tmp <<= 8;
        tmp = bytestream2_peek_byte(b);

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
        tmp>>=8;
    }
    // Add bits to the MSB of the bit buffer
    buffer->buf |= (uint64_t)tmp << (64- buffer->bits_left);
    return 1;
}

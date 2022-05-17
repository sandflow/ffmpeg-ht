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
#include "jpeg2000dec.h"
#include <stdint.h>

#include "bytestream.h"

/**
 * Determine if a word has a zero byte
 *
 * @param dword value to check if it contains  zeroe bytes
 *
 * @returns a number whose high bit is set if a byte at a position  is equal to zero
 * */
static uint32_t has_zero(uint32_t dword)
{
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
static uint32_t has_byte(uint32_t dword, uint8_t byte)
{
    return has_zero(dword ^ (~0UL / 255 * (byte)));
}

typedef struct Jpeg2000ByteBuffer {
    uint8_t bits_left; // number of bits remaining in the byte buffer
    uint64_t bit_buf;  // actual byte buffer
    GetByteContext *src; // source from which we pull our data from
} Jpeg2000ByteBuffer;

/* Initialize the byte buffer for HTJ2K decoding */
static int jpeg2000_init_byte_buf(Jpeg2000ByteBuffer *buffer, GetByteContext *b);

/**
 * Refill the bit buffer with new bytes unstuffing bits if needed
 *
 * @param buffer THe current bit-buffer where we are adding new bits
 * @param b The source where we are pulling bits
 * */
static int jpeg2000_refill_and_unsfuff( Jpeg2000DecoderContext *s,Jpeg2000ByteBuffer *buffer);
/**
 * Decode a jpeg2000 High throughtput bitstream
 *
 * @returns 0 on success, all other values are errors.
 * */
int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift);
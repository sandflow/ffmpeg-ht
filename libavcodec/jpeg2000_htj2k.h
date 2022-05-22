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

typedef struct Jpeg2000ByteBuffer {
    uint8_t bits_left;   // number of bits remaining in the byte buffer
    uint64_t bit_buf;    // actual byte buffer
    GetByteContext *src; // source from which we pull our data from
} Jpeg2000ByteBuffer;

/**
 * @brief State Machine variables for block decoding
 * 
 */
typedef struct StateVars {
    int32_t pos;
    uint32_t bits;
    uint32_t tmp;
    uint32_t last;

} StateVars;
/**
 * @brief Adaptive run length decoding algorithm
 * 
 */
typedef struct MelDecoderState{
    uint8_t k;
    uint8_t run;
    uint8_t one;

} MelDecoderState;
/**
 * @brief Table 2 in clause 7.3.3
 * */
const static uint8_t MEL_E[13] = { 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5};


/**
 * Determine if a word has a zero byte
 *
 * @param dword value to check if it contains  zeroe bytes
 *
 * @returns 0 if the dword has no zero 1 otherwise
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
 * 
 * @returns zero if the dword doesn't contain `bytes`, 1 otherwise.      
 * */
static uint32_t has_byte(uint32_t dword, uint8_t byte)
{
    return has_zero(dword ^ (~0UL / 255 * (byte)));
}

/**
 * @brief Initialize BitStream decoder
 * 
 * @param buffer A struct containing BitStream variables
 * @param b The byte buffer we will be extracting bits from.
  */
static void jpeg2000_init_byte_buf(Jpeg2000ByteBuffer *buffer, GetByteContext *b);

/**
 * Refill the bit buffer with new bytes unstuffing bits if needed
 *
 * @param s. A jpeg2000 decoder context
 * @param buffer THe current bit-buffer where we are adding new bits
 *
 * */
static int jpeg2000_refill_and_unsfuff(Jpeg2000DecoderContext *s, Jpeg2000ByteBuffer *buffer);

/**
 * Entry point for Cleanup segment decoding
 *
 *
 * @param s         JPeg20000 decoder context
 * @param cblk      Code block for this packet.
 * @param Dcup      The bytes of a HT cleanup segment
 * @param Lcup      Length in bytes of the HT cleanup segment
 * @param Pcup      Prefix length of the HT cleanup segment.
 * @param width     Width of the code block
 * @param height    Height of the code block
 *
 * */
static  int jpeg2000_decode_ht_cleanup(Jpeg2000DecoderContext *s,Jpeg2000Cblk *cblk,uint8_t *Dcup, uint32_t Lcup,uint32_t Pcup,int width,int heigth);

/**
 * @brief Decode significance and EMB patterns
 * 
 *
 * Described in Clause 7.3.5. 
 *
 * @param mel_state     Adaptive run length state machine variables
 * @param mel_stream    Adaptive run length bit stream variables.
 * @param q             Quad index
 * @param context       Significane of a set of neighbouring samples
 * @param Dcup          Bytes of the HT cleanup segment
 * @param Lcup          Length of the HT cleanup segment
 *
 * 
 */
static int jpeg2000_decode_sig_emb(MelDecoderState *mel_state, StateVars *mel_stream,uint8_t *Dcup,uint16_t q,uint16_t context,uint32_t Lcup);

/**
 * @brief Initialize the mel decoder by zeroing all its variables
 * 
 *  Described in Clause 7.1.3
 *
 * @param mel_state  An allocated but uninitialized MEL decoder
 */
static void jpeg2000_init_mel_decoder(MelDecoderState *mel_state);

/**
 * @brief Decode an adaptive run length symbol
 * 
 * @param mel_state Variables for MEL state machine
 * @param mel       MEL bit stream struct 
 * @param Dcup      Bytes of the HT cleanup segment
 * @param Lcup      Length of the HT cleanup segment
 * @return int 
 */
static int jpeg2000_decode_mel_sym(MelDecoderState *mel_state, StateVars *mel,const uint8_t *Dcup, uint32_t Lcup);

/**
 * @brief Recover Adaptive run length bits from the byte stream
 * 
 * @param mel_stream    The MEL byte stream state variables
 * @param Dcup          Bytes of the HT segment
 * @param Lcup          Length of the HT cleanup segment. 
 *         
 * @return int          THe next MEL bit      
 */
static int jpeg2000_import_mel_bit(StateVars *mel_stream,const uint8_t *Dcup,uint32_t Lcup);

/**
 * Decode a jpeg2000 High throughtput bitstream
 *
 * @returns 0 on success, all other values are errors.
 * */
int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift);
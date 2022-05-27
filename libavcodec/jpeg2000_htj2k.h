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
#include <libavutil/log.h>
#include <stdint.h>

#include "bytestream.h"

#define J2K_FIRST_QUAD 0

#define J2K_SECOND_QUAD 1
/**
 * @brief State Machine variables for block decoding
 *
 */
typedef struct StateVars {
    int32_t pos;
    uint32_t bits;
    uint32_t tmp;
    uint32_t last;

    uint8_t bits_left; // number of bits remaining in the byte buffer
    uint64_t bit_buf;

} StateVars;
/**
 * @brief Adaptive run length decoding algorithm
 *
 */
typedef struct MelDecoderState {
    uint8_t k;
    uint8_t run;
    uint8_t one;

} MelDecoderState;
/**
 * @brief Table 2 in clause 7.3.3
 * */
const static uint8_t MEL_E[13] = {0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5};

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
 * Refill the bit buffer with new bytes unstuffing bits if needed
 *
 * @param s. A jpeg2000 decoder context
 * @param buffer THe current bit-buffer where we are adding new bits
 *
 * */
int jpeg2000_bitbuf_refill_backwards(StateVars *buffer, const uint8_t *array);
/**
 * @brief Drops bits from the bit buffer
 *
 * @param buf: Struct containing bit buffers
 * @param nbits: Number of bits to remove.
 * */
void jpeg2000_bitbuf_drop_bits(StateVars *buf, uint8_t nbits);

/**
 * @brief  Get a variable number of bits from the stream
 *
 * @param  bit_stream   The struct containing the bit buffer and the number of bits left.
 * @param  nbits        Number of bits to retrieve from the stream
 * @param  buf          The array where we will be pulling in more bytes from in case it's empty
 *
 *@returns Lower `nbits` bits of the bit buffer
 *
 * This routine will refill bytes in case it can't satisfy the request to get `nbits`
 * */
uint64_t jpeg2000_bitbuf_get_bits(StateVars *bit_stream, uint8_t nbits, const uint8_t *buf);

/**
 * @brief Retrieve `nbits` from the bitbuffer but don't discard them
 *
 * @param stream The bit buffer
 * @param nbits  The number of bits to to peek ahead
 * */
uint64_t jpeg2000_bitbuf_peek_bits(StateVars *stream, uint8_t nbits);

void jpeg2000_init_mel_decoder(MelDecoderState *mel_state);
/**
 * Entry point for Cleanup segment decoding
 *
 *
 * @param s             JPeg20000 decoder context
 * @param cblk          Code block for this packet.
 * @param mel_state     Adaptive Run Length State variables
 * @param mel_stream    Adaptive Run Length BitStream variables.
 * @param Dcup          The bytes of a HT cleanup segment
 * @param Lcup          Length in bytes of the HT cleanup segment
 * @param Pcup          Prefix length of the HT cleanup segment.
 * @param width         Width of the code block
 * @param height        Height of the code block
 *
 * */
int jpeg2000_decode_ht_cleanup(Jpeg2000DecoderContext *s, Jpeg2000Cblk *cblk, MelDecoderState *mel_state, StateVars *mel_stream, StateVars *vlc_stream, const uint8_t *Dcup, uint32_t Lcup, uint32_t Pcup, int width, int height);

/**
 * @brief Decode significance and EMB patterns
 *
 *
 * Described in Clause 7.3.5.
 *
 * @param s             A jpeg2000 decoder context.
 * @param mel_state     Adaptive run length state machine variables
 * @param mel_stream    Adaptive run length bit stream variables.
 * @param vlc_stream    Variable Length coding bit stream variables.
 * @param vlc_table     A Variable length Context table described in Annex C.
 * @param q             Quad index
 * @param context       Significance of a set of neighbouring samples
 * @param Dcup          Bytes of the HT cleanup segment
 * @param sig_pat       Significance pattern  ρq
 * @param res_off       Unsigned residual offset  uqoff
 * @param emb_pat_k     Exponent Magnitude Pattern.
 * @param emb_pat_1     Exponent Magnitude Pattern.
 * @param pos           Position to write values in the above 4 parameters
 * @param Lcup          Length of the HT cleanup segment.
 * @param Pcup          HT cleanup segment prefix length.
 *
 * @returns  -1 on error.
 */
int jpeg2000_decode_sig_emb(Jpeg2000DecoderContext *s, MelDecoderState *mel_state, StateVars *mel_stream, StateVars *vlc_stream, const uint16_t *vlc_table, const uint8_t *Dcup, uint8_t *sig_pat, uint8_t *res_off, uint8_t *emb_pat_k, uint8_t *emb_pat_1, uint8_t pos, uint16_t q, uint16_t context, uint32_t Lcup, uint32_t Pcup);

/**
 * @brief Decode an adaptive run length symbol
 *
 * @param mel_state Variables for MEL state machine
 * @param mel       MEL bit stream struct
 * @param Dcup      Bytes of the HT cleanup segment
 * @param Lcup      Length of the HT cleanup segment
 *
 * @return int
 */
int jpeg2000_decode_mel_sym(MelDecoderState *mel_state, StateVars *mel, const uint8_t *Dcup, uint32_t Lcup);

/**
 * @brief Recover Adaptive run length bits from the byte stream
 *
 * @param mel_stream    The MEL byte stream state variables
 * @param Dcup          Bytes of the HT segment
 * @param Lcup          Length of the HT cleanup segment.
 *
 * @return int          The next MEL bit
 */
int jpeg2000_import_mel_bit(StateVars *mel_stream, const uint8_t *Dcup, uint32_t Lcup);

/**
 * @brief Decode Variable Suffix Length
 *
 * @param vlc_stream    VLC bit-stream
 * @param prefix
 * @param refill_array  The array to pull more bytes in case the bit-stream doesn't have enough bits.
 **/
uint8_t vlc_decode_u_suffix(StateVars *vlc_stream, uint8_t suffix, const uint8_t *refill_array);
/**
 * @brief Decode Variable Prefix Length
 * @param vlc_stream    VLC bit-stream
 * @param refill_array  The array to pull more bytes in case the bit-stream doesn't have enough bits.
 **/
uint8_t vlc_decode_u_prefix(StateVars *vlc_stream, const uint8_t *refill_array);


uint8_t vlc_decode_u_extension(StateVars *vlc_stream, uint8_t suffix, const uint8_t *refill_array);

/**
 * @brief Retrieve VLC bits from the byte-stream
 *
 * @param s             A Jpeg2000 Decoder context
 * @param vlc_stream    VLC Byte stream state variables
 * @param Dcup          HT Cleanup Segment bytes
 * @param Pcup          HT cleanup segment prefix length.
 *
 * @return int          -1 on error, (0 or 1 on success).
 */
int jpeg2000_import_vlc_bit(Jpeg2000DecoderContext *s, StateVars *vlc_stream, const uint8_t *Dcup, uint32_t Pcup);
/**
 * @brief Decode Context for Variable Length Coding
 *
 * @param s             A jpeg Decoder context
 * @param vlc_stream    State variables for VLC bit-stream
 * @param table         CtxVlC decoder tables described in Annex C
 * @param Dcup          Bytes of the HT cleanup segment
 * @param sig_pat       Significance pattern  ρq
 * @param res_off       Unsigned residual offset  uqoff
 * @param emb_pat_k     Exponent Magnitude Pattern.
 * @param emb_pat_1     Exponent Magnitude Pattern.
 * @param pos           Position to write values in the above 4 parameters
 * @param Pcup          Length of Prefix Segment
 * @param context       Significance of a set of neighbouring samples
 * */
int jpeg2000_decode_ctx_vlc(Jpeg2000DecoderContext *s, StateVars *vlc_stream, const uint16_t *table, const uint8_t *Dcup, uint8_t *sig_pat, uint8_t *res_off, uint8_t *emb_pat_k, uint8_t *emb_pat_1, uint8_t pos, uint32_t Pcup, uint16_t context);

/**
 * Decode a jpeg2000 High throughput bitstream
 *
 * @returns 0 on success, all other values are errors.
 * */
int decode_htj2k(Jpeg2000DecoderContext *s, Jpeg2000CodingStyle *codsty, Jpeg2000T1Context *t1, Jpeg2000Cblk *cblk, int width, int height, int bandpos, uint8_t roi_shift);

/**
 * @brief  CtxVLC tables, borrowed from openhtj2k (https://github.com/osamu620/OpenHTJ2K) (credits to Osamu Watanabe)
 *
 */
static const uint16_t dec_CxtVLC_table1[1024] = {
    0x0016, 0x006A, 0x0046, 0x00DD, 0x0086, 0x888B, 0x0026, 0x444D, 0x0016, 0x00AA, 0x0046, 0x88AD, 0x0086,
    0x003A, 0x0026, 0x00DE, 0x0016, 0x00CA, 0x0046, 0x009D, 0x0086, 0x005A, 0x0026, 0x222D, 0x0016, 0x009A,
    0x0046, 0x007D, 0x0086, 0x01FD, 0x0026, 0x007E, 0x0016, 0x006A, 0x0046, 0x88CD, 0x0086, 0x888B, 0x0026,
    0x111D, 0x0016, 0x00AA, 0x0046, 0x005D, 0x0086, 0x003A, 0x0026, 0x00EE, 0x0016, 0x00CA, 0x0046, 0x00BD,
    0x0086, 0x005A, 0x0026, 0x11FF, 0x0016, 0x009A, 0x0046, 0x003D, 0x0086, 0x04ED, 0x0026, 0x2AAF, 0x0016,
    0x006A, 0x0046, 0x00DD, 0x0086, 0x888B, 0x0026, 0x444D, 0x0016, 0x00AA, 0x0046, 0x88AD, 0x0086, 0x003A,
    0x0026, 0x44EF, 0x0016, 0x00CA, 0x0046, 0x009D, 0x0086, 0x005A, 0x0026, 0x222D, 0x0016, 0x009A, 0x0046,
    0x007D, 0x0086, 0x01FD, 0x0026, 0x00BE, 0x0016, 0x006A, 0x0046, 0x88CD, 0x0086, 0x888B, 0x0026, 0x111D,
    0x0016, 0x00AA, 0x0046, 0x005D, 0x0086, 0x003A, 0x0026, 0x4CCF, 0x0016, 0x00CA, 0x0046, 0x00BD, 0x0086,
    0x005A, 0x0026, 0x00FE, 0x0016, 0x009A, 0x0046, 0x003D, 0x0086, 0x04ED, 0x0026, 0x006F, 0x0002, 0x0088,
    0x0002, 0x005C, 0x0002, 0x0018, 0x0002, 0x00DE, 0x0002, 0x0028, 0x0002, 0x009C, 0x0002, 0x004A, 0x0002,
    0x007E, 0x0002, 0x0088, 0x0002, 0x00CC, 0x0002, 0x0018, 0x0002, 0x888F, 0x0002, 0x0028, 0x0002, 0x00FE,
    0x0002, 0x003A, 0x0002, 0x222F, 0x0002, 0x0088, 0x0002, 0x04FD, 0x0002, 0x0018, 0x0002, 0x00BE, 0x0002,
    0x0028, 0x0002, 0x00BF, 0x0002, 0x004A, 0x0002, 0x006E, 0x0002, 0x0088, 0x0002, 0x00AC, 0x0002, 0x0018,
    0x0002, 0x444F, 0x0002, 0x0028, 0x0002, 0x00EE, 0x0002, 0x003A, 0x0002, 0x113F, 0x0002, 0x0088, 0x0002,
    0x005C, 0x0002, 0x0018, 0x0002, 0x00CF, 0x0002, 0x0028, 0x0002, 0x009C, 0x0002, 0x004A, 0x0002, 0x006F,
    0x0002, 0x0088, 0x0002, 0x00CC, 0x0002, 0x0018, 0x0002, 0x009F, 0x0002, 0x0028, 0x0002, 0x00EF, 0x0002,
    0x003A, 0x0002, 0x233F, 0x0002, 0x0088, 0x0002, 0x04FD, 0x0002, 0x0018, 0x0002, 0x00AF, 0x0002, 0x0028,
    0x0002, 0x44FF, 0x0002, 0x004A, 0x0002, 0x005F, 0x0002, 0x0088, 0x0002, 0x00AC, 0x0002, 0x0018, 0x0002,
    0x007F, 0x0002, 0x0028, 0x0002, 0x00DF, 0x0002, 0x003A, 0x0002, 0x111F, 0x0002, 0x0028, 0x0002, 0x005C,
    0x0002, 0x008A, 0x0002, 0x00BF, 0x0002, 0x0018, 0x0002, 0x00FE, 0x0002, 0x00CC, 0x0002, 0x007E, 0x0002,
    0x0028, 0x0002, 0x8FFF, 0x0002, 0x004A, 0x0002, 0x007F, 0x0002, 0x0018, 0x0002, 0x00DF, 0x0002, 0x00AC,
    0x0002, 0x133F, 0x0002, 0x0028, 0x0002, 0x222D, 0x0002, 0x008A, 0x0002, 0x00BE, 0x0002, 0x0018, 0x0002,
    0x44EF, 0x0002, 0x2AAD, 0x0002, 0x006E, 0x0002, 0x0028, 0x0002, 0x15FF, 0x0002, 0x004A, 0x0002, 0x009E,
    0x0002, 0x0018, 0x0002, 0x00CF, 0x0002, 0x003C, 0x0002, 0x223F, 0x0002, 0x0028, 0x0002, 0x005C, 0x0002,
    0x008A, 0x0002, 0x2BBF, 0x0002, 0x0018, 0x0002, 0x04EF, 0x0002, 0x00CC, 0x0002, 0x006F, 0x0002, 0x0028,
    0x0002, 0x27FF, 0x0002, 0x004A, 0x0002, 0x009F, 0x0002, 0x0018, 0x0002, 0x00DE, 0x0002, 0x00AC, 0x0002,
    0x444F, 0x0002, 0x0028, 0x0002, 0x222D, 0x0002, 0x008A, 0x0002, 0x8AAF, 0x0002, 0x0018, 0x0002, 0x00EE,
    0x0002, 0x2AAD, 0x0002, 0x005F, 0x0002, 0x0028, 0x0002, 0x44FF, 0x0002, 0x004A, 0x0002, 0x888F, 0x0002,
    0x0018, 0x0002, 0xAAAF, 0x0002, 0x003C, 0x0002, 0x111F, 0x0004, 0x8FFD, 0x0028, 0x005C, 0x0004, 0x00BC,
    0x008A, 0x66FF, 0x0004, 0x00CD, 0x0018, 0x111D, 0x0004, 0x009C, 0x003A, 0x8AAF, 0x0004, 0x00FC, 0x0028,
    0x133D, 0x0004, 0x00AC, 0x004A, 0x3BBF, 0x0004, 0x2BBD, 0x0018, 0x5FFF, 0x0004, 0x006C, 0x157D, 0x455F,
    0x0004, 0x2FFD, 0x0028, 0x222D, 0x0004, 0x22AD, 0x008A, 0x44EF, 0x0004, 0x00CC, 0x0018, 0x4FFF, 0x0004,
    0x007C, 0x003A, 0x447F, 0x0004, 0x04DD, 0x0028, 0x233D, 0x0004, 0x009D, 0x004A, 0x00DE, 0x0004, 0x88BD,
    0x0018, 0xAFFF, 0x0004, 0x115D, 0x1FFD, 0x444F, 0x0004, 0x8FFD, 0x0028, 0x005C, 0x0004, 0x00BC, 0x008A,
    0x8CEF, 0x0004, 0x00CD, 0x0018, 0x111D, 0x0004, 0x009C, 0x003A, 0x888F, 0x0004, 0x00FC, 0x0028, 0x133D,
    0x0004, 0x00AC, 0x004A, 0x44DF, 0x0004, 0x2BBD, 0x0018, 0x8AFF, 0x0004, 0x006C, 0x157D, 0x006F, 0x0004,
    0x2FFD, 0x0028, 0x222D, 0x0004, 0x22AD, 0x008A, 0x00EE, 0x0004, 0x00CC, 0x0018, 0x2EEF, 0x0004, 0x007C,
    0x003A, 0x277F, 0x0004, 0x04DD, 0x0028, 0x233D, 0x0004, 0x009D, 0x004A, 0x1BBF, 0x0004, 0x88BD, 0x0018,
    0x37FF, 0x0004, 0x115D, 0x1FFD, 0x333F, 0x0002, 0x0088, 0x0002, 0x02ED, 0x0002, 0x00CA, 0x0002, 0x4CCF,
    0x0002, 0x0048, 0x0002, 0x23FF, 0x0002, 0x001A, 0x0002, 0x888F, 0x0002, 0x0088, 0x0002, 0x006C, 0x0002,
    0x002A, 0x0002, 0x00AF, 0x0002, 0x0048, 0x0002, 0x22EF, 0x0002, 0x00AC, 0x0002, 0x005F, 0x0002, 0x0088,
    0x0002, 0x444D, 0x0002, 0x00CA, 0x0002, 0xCCCF, 0x0002, 0x0048, 0x0002, 0x00FE, 0x0002, 0x001A, 0x0002,
    0x006F, 0x0002, 0x0088, 0x0002, 0x005C, 0x0002, 0x002A, 0x0002, 0x009F, 0x0002, 0x0048, 0x0002, 0x00DF,
    0x0002, 0x03FD, 0x0002, 0x222F, 0x0002, 0x0088, 0x0002, 0x02ED, 0x0002, 0x00CA, 0x0002, 0x8CCF, 0x0002,
    0x0048, 0x0002, 0x11FF, 0x0002, 0x001A, 0x0002, 0x007E, 0x0002, 0x0088, 0x0002, 0x006C, 0x0002, 0x002A,
    0x0002, 0x007F, 0x0002, 0x0048, 0x0002, 0x00EE, 0x0002, 0x00AC, 0x0002, 0x003E, 0x0002, 0x0088, 0x0002,
    0x444D, 0x0002, 0x00CA, 0x0002, 0x00BE, 0x0002, 0x0048, 0x0002, 0x00BF, 0x0002, 0x001A, 0x0002, 0x003F,
    0x0002, 0x0088, 0x0002, 0x005C, 0x0002, 0x002A, 0x0002, 0x009E, 0x0002, 0x0048, 0x0002, 0x00DE, 0x0002,
    0x03FD, 0x0002, 0x111F, 0x0004, 0x8AED, 0x0048, 0x888D, 0x0004, 0x00DC, 0x00CA, 0x3FFF, 0x0004, 0xCFFD,
    0x002A, 0x003D, 0x0004, 0x00BC, 0x005A, 0x8DDF, 0x0004, 0x8FFD, 0x0048, 0x006C, 0x0004, 0x027D, 0x008A,
    0x99FF, 0x0004, 0x00EC, 0x00FA, 0x003C, 0x0004, 0x00AC, 0x001A, 0x009F, 0x0004, 0x2FFD, 0x0048, 0x007C,
    0x0004, 0x44CD, 0x00CA, 0x67FF, 0x0004, 0x1FFD, 0x002A, 0x444D, 0x0004, 0x00AD, 0x005A, 0x8CCF, 0x0004,
    0x4FFD, 0x0048, 0x445D, 0x0004, 0x01BD, 0x008A, 0x4EEF, 0x0004, 0x45DD, 0x00FA, 0x111D, 0x0004, 0x009C,
    0x001A, 0x222F, 0x0004, 0x8AED, 0x0048, 0x888D, 0x0004, 0x00DC, 0x00CA, 0xAFFF, 0x0004, 0xCFFD, 0x002A,
    0x003D, 0x0004, 0x00BC, 0x005A, 0x11BF, 0x0004, 0x8FFD, 0x0048, 0x006C, 0x0004, 0x027D, 0x008A, 0x22EF,
    0x0004, 0x00EC, 0x00FA, 0x003C, 0x0004, 0x00AC, 0x001A, 0x227F, 0x0004, 0x2FFD, 0x0048, 0x007C, 0x0004,
    0x44CD, 0x00CA, 0x5DFF, 0x0004, 0x1FFD, 0x002A, 0x444D, 0x0004, 0x00AD, 0x005A, 0x006F, 0x0004, 0x4FFD,
    0x0048, 0x445D, 0x0004, 0x01BD, 0x008A, 0x11DF, 0x0004, 0x45DD, 0x00FA, 0x111D, 0x0004, 0x009C, 0x001A,
    0x155F, 0x0006, 0x00FC, 0x0018, 0x111D, 0x0048, 0x888D, 0x00AA, 0x4DDF, 0x0006, 0x2AAD, 0x005A, 0x67FF,
    0x0028, 0x223D, 0x00BC, 0xAAAF, 0x0006, 0x00EC, 0x0018, 0x5FFF, 0x0048, 0x006C, 0x008A, 0xCCCF, 0x0006,
    0x009D, 0x00CA, 0x44EF, 0x0028, 0x003C, 0x8FFD, 0x137F, 0x0006, 0x8EED, 0x0018, 0x1FFF, 0x0048, 0x007C,
    0x00AA, 0x4CCF, 0x0006, 0x227D, 0x005A, 0x1DDF, 0x0028, 0x444D, 0x4FFD, 0x155F, 0x0006, 0x00DC, 0x0018,
    0x2EEF, 0x0048, 0x445D, 0x008A, 0x22BF, 0x0006, 0x009C, 0x00CA, 0x8CDF, 0x0028, 0x222D, 0x2FFD, 0x226F,
    0x0006, 0x00FC, 0x0018, 0x111D, 0x0048, 0x888D, 0x00AA, 0x1BBF, 0x0006, 0x2AAD, 0x005A, 0x33FF, 0x0028,
    0x223D, 0x00BC, 0x8AAF, 0x0006, 0x00EC, 0x0018, 0x9BFF, 0x0048, 0x006C, 0x008A, 0x8ABF, 0x0006, 0x009D,
    0x00CA, 0x4EEF, 0x0028, 0x003C, 0x8FFD, 0x466F, 0x0006, 0x8EED, 0x0018, 0xCFFF, 0x0048, 0x007C, 0x00AA,
    0x8CCF, 0x0006, 0x227D, 0x005A, 0xAEEF, 0x0028, 0x444D, 0x4FFD, 0x477F, 0x0006, 0x00DC, 0x0018, 0xAFFF,
    0x0048, 0x445D, 0x008A, 0x2BBF, 0x0006, 0x009C, 0x00CA, 0x44DF, 0x0028, 0x222D, 0x2FFD, 0x133F, 0x00F6,
    0xAFFD, 0x1FFB, 0x003C, 0x0008, 0x23BD, 0x007A, 0x11DF, 0x00F6, 0x45DD, 0x2FFB, 0x4EEF, 0x00DA, 0x177D,
    0xCFFD, 0x377F, 0x00F6, 0x3FFD, 0x8FFB, 0x111D, 0x0008, 0x009C, 0x005A, 0x1BBF, 0x00F6, 0x00CD, 0x00BA,
    0x8DDF, 0x4FFB, 0x006C, 0x9BFD, 0x455F, 0x00F6, 0x67FD, 0x1FFB, 0x002C, 0x0008, 0x00AC, 0x007A, 0x009F,
    0x00F6, 0x00AD, 0x2FFB, 0x7FFF, 0x00DA, 0x004C, 0x5FFD, 0x477F, 0x00F6, 0x00EC, 0x8FFB, 0x001C, 0x0008,
    0x008C, 0x005A, 0x888F, 0x00F6, 0x00CC, 0x00BA, 0x2EEF, 0x4FFB, 0x115D, 0x8AED, 0x113F, 0x00F6, 0xAFFD,
    0x1FFB, 0x003C, 0x0008, 0x23BD, 0x007A, 0x1DDF, 0x00F6, 0x45DD, 0x2FFB, 0xBFFF, 0x00DA, 0x177D, 0xCFFD,
    0x447F, 0x00F6, 0x3FFD, 0x8FFB, 0x111D, 0x0008, 0x009C, 0x005A, 0x277F, 0x00F6, 0x00CD, 0x00BA, 0x22EF,
    0x4FFB, 0x006C, 0x9BFD, 0x444F, 0x00F6, 0x67FD, 0x1FFB, 0x002C, 0x0008, 0x00AC, 0x007A, 0x11BF, 0x00F6,
    0x00AD, 0x2FFB, 0xFFFF, 0x00DA, 0x004C, 0x5FFD, 0x233F, 0x00F6, 0x00EC, 0x8FFB, 0x001C, 0x0008, 0x008C,
    0x005A, 0x006F, 0x00F6, 0x00CC, 0x00BA, 0x8BBF, 0x4FFB, 0x115D, 0x8AED, 0x222F};

static const uint16_t dec_CxtVLC_table0[1024] = {
    0x0026, 0x00AA, 0x0046, 0x006C, 0x0086, 0x8AED, 0x0018, 0x8DDF, 0x0026, 0x01BD, 0x0046, 0x5FFF, 0x0086,
    0x027D, 0x005A, 0x155F, 0x0026, 0x003A, 0x0046, 0x444D, 0x0086, 0x4CCD, 0x0018, 0xCCCF, 0x0026, 0x2EFD,
    0x0046, 0x99FF, 0x0086, 0x009C, 0x00CA, 0x133F, 0x0026, 0x00AA, 0x0046, 0x445D, 0x0086, 0x8CCD, 0x0018,
    0x11DF, 0x0026, 0x4FFD, 0x0046, 0xCFFF, 0x0086, 0x009D, 0x005A, 0x007E, 0x0026, 0x003A, 0x0046, 0x1FFF,
    0x0086, 0x88AD, 0x0018, 0x00BE, 0x0026, 0x8FFD, 0x0046, 0x4EEF, 0x0086, 0x888D, 0x00CA, 0x111F, 0x0026,
    0x00AA, 0x0046, 0x006C, 0x0086, 0x8AED, 0x0018, 0x45DF, 0x0026, 0x01BD, 0x0046, 0x22EF, 0x0086, 0x027D,
    0x005A, 0x227F, 0x0026, 0x003A, 0x0046, 0x444D, 0x0086, 0x4CCD, 0x0018, 0x11BF, 0x0026, 0x2EFD, 0x0046,
    0x00FE, 0x0086, 0x009C, 0x00CA, 0x223F, 0x0026, 0x00AA, 0x0046, 0x445D, 0x0086, 0x8CCD, 0x0018, 0x00DE,
    0x0026, 0x4FFD, 0x0046, 0xABFF, 0x0086, 0x009D, 0x005A, 0x006F, 0x0026, 0x003A, 0x0046, 0x6EFF, 0x0086,
    0x88AD, 0x0018, 0x2AAF, 0x0026, 0x8FFD, 0x0046, 0x00EE, 0x0086, 0x888D, 0x00CA, 0x222F, 0x0004, 0x00CA,
    0x0088, 0x027D, 0x0004, 0x4CCD, 0x0028, 0x00FE, 0x0004, 0x2AFD, 0x0048, 0x005C, 0x0004, 0x009D, 0x0018,
    0x00DE, 0x0004, 0x01BD, 0x0088, 0x006C, 0x0004, 0x88AD, 0x0028, 0x11DF, 0x0004, 0x8AED, 0x0048, 0x003C,
    0x0004, 0x888D, 0x0018, 0x111F, 0x0004, 0x00CA, 0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0x88FF, 0x0004,
    0x8BFD, 0x0048, 0x444D, 0x0004, 0x009C, 0x0018, 0x00BE, 0x0004, 0x4EFD, 0x0088, 0x445D, 0x0004, 0x00AC,
    0x0028, 0x00EE, 0x0004, 0x45DD, 0x0048, 0x222D, 0x0004, 0x003D, 0x0018, 0x007E, 0x0004, 0x00CA, 0x0088,
    0x027D, 0x0004, 0x4CCD, 0x0028, 0x1FFF, 0x0004, 0x2AFD, 0x0048, 0x005C, 0x0004, 0x009D, 0x0018, 0x11BF,
    0x0004, 0x01BD, 0x0088, 0x006C, 0x0004, 0x88AD, 0x0028, 0x22EF, 0x0004, 0x8AED, 0x0048, 0x003C, 0x0004,
    0x888D, 0x0018, 0x227F, 0x0004, 0x00CA, 0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0x4EEF, 0x0004, 0x8BFD,
    0x0048, 0x444D, 0x0004, 0x009C, 0x0018, 0x2AAF, 0x0004, 0x4EFD, 0x0088, 0x445D, 0x0004, 0x00AC, 0x0028,
    0x8DDF, 0x0004, 0x45DD, 0x0048, 0x222D, 0x0004, 0x003D, 0x0018, 0x155F, 0x0004, 0x005A, 0x0088, 0x006C,
    0x0004, 0x88DD, 0x0028, 0x23FF, 0x0004, 0x11FD, 0x0048, 0x444D, 0x0004, 0x00AD, 0x0018, 0x00BE, 0x0004,
    0x137D, 0x0088, 0x155D, 0x0004, 0x00CC, 0x0028, 0x00DE, 0x0004, 0x02ED, 0x0048, 0x111D, 0x0004, 0x009D,
    0x0018, 0x007E, 0x0004, 0x005A, 0x0088, 0x455D, 0x0004, 0x44CD, 0x0028, 0x00EE, 0x0004, 0x1FFD, 0x0048,
    0x003C, 0x0004, 0x00AC, 0x0018, 0x555F, 0x0004, 0x47FD, 0x0088, 0x113D, 0x0004, 0x02BD, 0x0028, 0x477F,
    0x0004, 0x4CDD, 0x0048, 0x8FFF, 0x0004, 0x009C, 0x0018, 0x222F, 0x0004, 0x005A, 0x0088, 0x006C, 0x0004,
    0x88DD, 0x0028, 0x00FE, 0x0004, 0x11FD, 0x0048, 0x444D, 0x0004, 0x00AD, 0x0018, 0x888F, 0x0004, 0x137D,
    0x0088, 0x155D, 0x0004, 0x00CC, 0x0028, 0x8CCF, 0x0004, 0x02ED, 0x0048, 0x111D, 0x0004, 0x009D, 0x0018,
    0x006F, 0x0004, 0x005A, 0x0088, 0x455D, 0x0004, 0x44CD, 0x0028, 0x1DDF, 0x0004, 0x1FFD, 0x0048, 0x003C,
    0x0004, 0x00AC, 0x0018, 0x227F, 0x0004, 0x47FD, 0x0088, 0x113D, 0x0004, 0x02BD, 0x0028, 0x22BF, 0x0004,
    0x4CDD, 0x0048, 0x22EF, 0x0004, 0x009C, 0x0018, 0x233F, 0x0006, 0x4DDD, 0x4FFB, 0xCFFF, 0x0018, 0x113D,
    0x005A, 0x888F, 0x0006, 0x23BD, 0x008A, 0x00EE, 0x002A, 0x155D, 0xAAFD, 0x277F, 0x0006, 0x44CD, 0x8FFB,
    0x44EF, 0x0018, 0x467D, 0x004A, 0x2AAF, 0x0006, 0x00AC, 0x555B, 0x99DF, 0x1FFB, 0x003C, 0x5FFD, 0x266F,
    0x0006, 0x1DDD, 0x4FFB, 0x6EFF, 0x0018, 0x177D, 0x005A, 0x1BBF, 0x0006, 0x88AD, 0x008A, 0x5DDF, 0x002A,
    0x444D, 0x2FFD, 0x667F, 0x0006, 0x00CC, 0x8FFB, 0x2EEF, 0x0018, 0x455D, 0x004A, 0x119F, 0x0006, 0x009C,
    0x555B, 0x8CCF, 0x1FFB, 0x111D, 0x8CED, 0x006E, 0x0006, 0x4DDD, 0x4FFB, 0x3FFF, 0x0018, 0x113D, 0x005A,
    0x11BF, 0x0006, 0x23BD, 0x008A, 0x8DDF, 0x002A, 0x155D, 0xAAFD, 0x222F, 0x0006, 0x44CD, 0x8FFB, 0x00FE,
    0x0018, 0x467D, 0x004A, 0x899F, 0x0006, 0x00AC, 0x555B, 0x00DE, 0x1FFB, 0x003C, 0x5FFD, 0x446F, 0x0006,
    0x1DDD, 0x4FFB, 0x9BFF, 0x0018, 0x177D, 0x005A, 0x00BE, 0x0006, 0x88AD, 0x008A, 0xCDDF, 0x002A, 0x444D,
    0x2FFD, 0x007E, 0x0006, 0x00CC, 0x8FFB, 0x4EEF, 0x0018, 0x455D, 0x004A, 0x377F, 0x0006, 0x009C, 0x555B,
    0x8BBF, 0x1FFB, 0x111D, 0x8CED, 0x233F, 0x0004, 0x00AA, 0x0088, 0x047D, 0x0004, 0x01DD, 0x0028, 0x11DF,
    0x0004, 0x27FD, 0x0048, 0x005C, 0x0004, 0x8AAD, 0x0018, 0x2BBF, 0x0004, 0x009C, 0x0088, 0x006C, 0x0004,
    0x00CC, 0x0028, 0x00EE, 0x0004, 0x8CED, 0x0048, 0x222D, 0x0004, 0x888D, 0x0018, 0x007E, 0x0004, 0x00AA,
    0x0088, 0x006D, 0x0004, 0x88CD, 0x0028, 0x00FE, 0x0004, 0x19FD, 0x0048, 0x003C, 0x0004, 0x2AAD, 0x0018,
    0xAAAF, 0x0004, 0x8BFD, 0x0088, 0x005D, 0x0004, 0x00BD, 0x0028, 0x4CCF, 0x0004, 0x44ED, 0x0048, 0x4FFF,
    0x0004, 0x223D, 0x0018, 0x111F, 0x0004, 0x00AA, 0x0088, 0x047D, 0x0004, 0x01DD, 0x0028, 0x99FF, 0x0004,
    0x27FD, 0x0048, 0x005C, 0x0004, 0x8AAD, 0x0018, 0x00BE, 0x0004, 0x009C, 0x0088, 0x006C, 0x0004, 0x00CC,
    0x0028, 0x00DE, 0x0004, 0x8CED, 0x0048, 0x222D, 0x0004, 0x888D, 0x0018, 0x444F, 0x0004, 0x00AA, 0x0088,
    0x006D, 0x0004, 0x88CD, 0x0028, 0x2EEF, 0x0004, 0x19FD, 0x0048, 0x003C, 0x0004, 0x2AAD, 0x0018, 0x447F,
    0x0004, 0x8BFD, 0x0088, 0x005D, 0x0004, 0x00BD, 0x0028, 0x009F, 0x0004, 0x44ED, 0x0048, 0x67FF, 0x0004,
    0x223D, 0x0018, 0x133F, 0x0006, 0x00CC, 0x008A, 0x9DFF, 0x2FFB, 0x467D, 0x1FFD, 0x99BF, 0x0006, 0x2AAD,
    0x002A, 0x66EF, 0x4FFB, 0x005C, 0x2EED, 0x377F, 0x0006, 0x89BD, 0x004A, 0x00FE, 0x8FFB, 0x006C, 0x67FD,
    0x889F, 0x0006, 0x888D, 0x001A, 0x5DDF, 0x00AA, 0x222D, 0x89DD, 0x444F, 0x0006, 0x2BBD, 0x008A, 0xCFFF,
    0x2FFB, 0x226D, 0x009C, 0x00BE, 0x0006, 0xAAAD, 0x002A, 0x1DDF, 0x4FFB, 0x003C, 0x4DDD, 0x466F, 0x0006,
    0x8AAD, 0x004A, 0xAEEF, 0x8FFB, 0x445D, 0x8EED, 0x177F, 0x0006, 0x233D, 0x001A, 0x4CCF, 0x00AA, 0xAFFF,
    0x88CD, 0x133F, 0x0006, 0x00CC, 0x008A, 0x77FF, 0x2FFB, 0x467D, 0x1FFD, 0x3BBF, 0x0006, 0x2AAD, 0x002A,
    0x00EE, 0x4FFB, 0x005C, 0x2EED, 0x007E, 0x0006, 0x89BD, 0x004A, 0x4EEF, 0x8FFB, 0x006C, 0x67FD, 0x667F,
    0x0006, 0x888D, 0x001A, 0x00DE, 0x00AA, 0x222D, 0x89DD, 0x333F, 0x0006, 0x2BBD, 0x008A, 0x57FF, 0x2FFB,
    0x226D, 0x009C, 0x199F, 0x0006, 0xAAAD, 0x002A, 0x99DF, 0x4FFB, 0x003C, 0x4DDD, 0x155F, 0x0006, 0x8AAD,
    0x004A, 0xCEEF, 0x8FFB, 0x445D, 0x8EED, 0x277F, 0x0006, 0x233D, 0x001A, 0x1BBF, 0x00AA, 0x3FFF, 0x88CD,
    0x111F, 0x0006, 0x45DD, 0x2FFB, 0x111D, 0x0018, 0x467D, 0x8FFD, 0xCCCF, 0x0006, 0x19BD, 0x004A, 0x22EF,
    0x002A, 0x222D, 0x3FFD, 0x888F, 0x0006, 0x00CC, 0x008A, 0x00FE, 0x0018, 0x115D, 0xCFFD, 0x8AAF, 0x0006,
    0x00AC, 0x003A, 0x8CDF, 0x1FFB, 0x133D, 0x66FD, 0x466F, 0x0006, 0x8CCD, 0x2FFB, 0x5FFF, 0x0018, 0x006C,
    0x4FFD, 0xABBF, 0x0006, 0x22AD, 0x004A, 0x00EE, 0x002A, 0x233D, 0xAEFD, 0x377F, 0x0006, 0x2BBD, 0x008A,
    0x55DF, 0x0018, 0x005C, 0x177D, 0x119F, 0x0006, 0x009C, 0x003A, 0x4CCF, 0x1FFB, 0x333D, 0x8EED, 0x444F,
    0x0006, 0x45DD, 0x2FFB, 0x111D, 0x0018, 0x467D, 0x8FFD, 0x99BF, 0x0006, 0x19BD, 0x004A, 0x2EEF, 0x002A,
    0x222D, 0x3FFD, 0x667F, 0x0006, 0x00CC, 0x008A, 0x4EEF, 0x0018, 0x115D, 0xCFFD, 0x899F, 0x0006, 0x00AC,
    0x003A, 0x00DE, 0x1FFB, 0x133D, 0x66FD, 0x226F, 0x0006, 0x8CCD, 0x2FFB, 0x9BFF, 0x0018, 0x006C, 0x4FFD,
    0x00BE, 0x0006, 0x22AD, 0x004A, 0x1DDF, 0x002A, 0x233D, 0xAEFD, 0x007E, 0x0006, 0x2BBD, 0x008A, 0xCEEF,
    0x0018, 0x005C, 0x177D, 0x277F, 0x0006, 0x009C, 0x003A, 0x8BBF, 0x1FFB, 0x333D, 0x8EED, 0x455F, 0x1FF9,
    0x1DDD, 0xAFFB, 0x00DE, 0x8FF9, 0x001C, 0xFFFB, 0x477F, 0x4FF9, 0x177D, 0x3FFB, 0x3BBF, 0x2FF9, 0xAEEF,
    0x8EED, 0x444F, 0x1FF9, 0x22AD, 0x000A, 0x8BBF, 0x8FF9, 0x00FE, 0xCFFD, 0x007E, 0x4FF9, 0x115D, 0x5FFB,
    0x577F, 0x2FF9, 0x8DDF, 0x2EED, 0x333F, 0x1FF9, 0x2BBD, 0xAFFB, 0x88CF, 0x8FF9, 0xBFFF, 0xFFFB, 0x377F,
    0x4FF9, 0x006D, 0x3FFB, 0x00BE, 0x2FF9, 0x66EF, 0x9FFD, 0x133F, 0x1FF9, 0x009D, 0x000A, 0xABBF, 0x8FF9,
    0xDFFF, 0x6FFD, 0x006E, 0x4FF9, 0x002C, 0x5FFB, 0x888F, 0x2FF9, 0xCDDF, 0x4DDD, 0x222F, 0x1FF9, 0x1DDD,
    0xAFFB, 0x4CCF, 0x8FF9, 0x001C, 0xFFFB, 0x277F, 0x4FF9, 0x177D, 0x3FFB, 0x99BF, 0x2FF9, 0xCEEF, 0x8EED,
    0x004E, 0x1FF9, 0x22AD, 0x000A, 0x00AE, 0x8FF9, 0x7FFF, 0xCFFD, 0x005E, 0x4FF9, 0x115D, 0x5FFB, 0x009E,
    0x2FF9, 0x5DDF, 0x2EED, 0x003E, 0x1FF9, 0x2BBD, 0xAFFB, 0x00CE, 0x8FF9, 0xEFFF, 0xFFFB, 0x667F, 0x4FF9,
    0x006D, 0x3FFB, 0x8AAF, 0x2FF9, 0x00EE, 0x9FFD, 0x233F, 0x1FF9, 0x009D, 0x000A, 0x1BBF, 0x8FF9, 0x4EEF,
    0x6FFD, 0x455F, 0x4FF9, 0x002C, 0x5FFB, 0x008E, 0x2FF9, 0x99DF, 0x4DDD, 0x111F};
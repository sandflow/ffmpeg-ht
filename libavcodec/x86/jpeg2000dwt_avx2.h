/*
 * AVX2 optimized Discrete wavelet transform
 * Copyright (c) 2024 Osamu Watanabe
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

#ifndef AVCODEC_JPEG2000DWT_AVX2_H
#define AVCODEC_JPEG2000DWT_AVX2_H

#include "libavutil/attributes.h"
#include "libavutil/cpu.h"
#include "libavutil/x86/cpu.h"

#include <stdint.h>

void idwt_2d_interleave_int_avx2(void *in, void *LL, void *HL, void *LH, void *HH,
                                 int32_t u0, int32_t u1, int32_t v0, int32_t v1, int32_t w);
void idwt_2d_interleave_float_avx2(void *in, void *LL, void *HL, void *LH, void *HH,
                                   int32_t u0, int32_t u1, int32_t v0, int32_t v1, int32_t w);
void idwt_1d_filtr_53_avx2(void*, const int32_t, const int32_t, const int32_t);
void idwt_1d_filtr_97_avx2(void*, const int32_t, const int32_t, const int32_t);
void idwt_1d_filtr_97_int_avx2(void*, const int32_t, const int32_t, const int32_t);
void idwt_ver_sr_53_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                    const int32_t v1, const int32_t w);
void idwt_ver_sr_97_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                             const int32_t v1, const int32_t w);
void idwt_ver_sr_97_int_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                             const int32_t v1, const int32_t w);
#endif
/*
 * Discrete wavelet transform
 * Copyright (c) 2007 Kamil Nowosad
 * Copyright (c) 2013 Nicolas Bertrand <nicoinattendu@gmail.com>
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
 * Discrete wavelet transform
 */

#include <string.h>
#include "libavutil/error.h"
#include "libavutil/macros.h"
#include "libavutil/mem.h"
#include "jpeg2000dwt.h"
#include "jpeg2000.h"

#if HAVE_AVX2
#include "libavcodec/x86/jpeg2000dwt_avx2.h"
#elif HAVE_NEON
#include "libavcodec/aarch64/jpeg2000dwt_neon.h"
#endif

/* Defines for 9/7 DWT lifting parameters.
 * Parameters are in float. */
#define F_LFTG_ALPHA  1.586134342059924f
#define F_LFTG_BETA   0.052980118572961f
#define F_LFTG_GAMMA  0.882911075530934f
#define F_LFTG_DELTA  0.443506852043971f

/* Lifting parameters in integer format.
 * Computed as param = (float param) * (1 << 16) */
#define I_LFTG_ALPHA  38413ll // 103949ll
#define I_LFTG_BETA    3472ll //13888ll
#define I_LFTG_GAMMA   57862ll
#define I_LFTG_DELTA   29066ll
#define I_LFTG_K       80621ll
#define I_LFTG_X       53274ll

#define I_LFTG_ALPHA_SHIFT 16
#define I_LFTG_BETA_SHIFT 16
#define I_LFTG_GAMMA_SHIFT 16
#define I_LFTG_DELTA_SHIFT 16
#define I_PRESHIFT 8

static void (*idwt_2d_interleave)(void *, void *, void *, void *, void *, int32_t, int32_t, int32_t, int32_t, int32_t);
static void (*ver_sr)(void *, const int32_t, const int32_t, const int32_t, const int32_t, const int32_t);
static void (*filtr)(void*, const int32_t, const int32_t, const int32_t);

static inline void extend53(int *p, int i0, int i1)
{
    p[i0 - 1] = p[i0 + 1];
    p[i1]     = p[i1 - 2];
    p[i0 - 2] = p[i0 + 2];
    p[i1 + 1] = p[i1 - 3];
}

static inline void extend97_float(float *p, int i0, int i1)
{
    int i;

    for (i = 1; i <= 4; i++) {
        p[i0 - i]     = p[i0 + i];
        p[i1 + i - 1] = p[i1 - i - 1];
    }
}

static inline void extend97_int(int32_t *p, int i0, int i1)
{
    int i;

    for (i = 1; i <= 4; i++) {
        p[i0 - i]     = p[i0 + i];
        p[i1 + i - 1] = p[i1 - i - 1];
    }
}

static void sd_1d53(int *p, int i0, int i1)
{
    int i;

    if (i1 <= i0 + 1) {
        if (i0 == 1)
            p[1] *= 2;
        return;
    }

    extend53(p, i0, i1);

    for (i = ((i0+1)>>1) - 1; i < (i1+1)>>1; i++)
        p[2*i+1] -= (p[2*i] + p[2*i+2]) >> 1;
    for (i = ((i0+1)>>1); i < (i1+1)>>1; i++)
        p[2*i] += (p[2*i-1] + p[2*i+1] + 2) >> 2;
}

static void dwt_encode53(DWTContext *s, int *t)
{
    int lev,
        w = s->linelen[s->ndeclevels-1][0];
    int *line = s->i_linebuf;
    line += 3;

    for (lev = s->ndeclevels-1; lev >= 0; lev--){
        int lh = s->linelen[lev][0],
            lv = s->linelen[lev][1],
            mh = s->mod[lev][0],
            mv = s->mod[lev][1],
            lp;
        int *l;

        // VER_SD
        l = line + mv;
        for (lp = 0; lp < lh; lp++) {
            int i, j = 0;

            for (i = 0; i < lv; i++)
                l[i] = t[w*i + lp];

            sd_1d53(line, mv, mv + lv);

            // copy back and deinterleave
            for (i =   mv; i < lv; i+=2, j++)
                t[w*j + lp] = l[i];
            for (i = 1-mv; i < lv; i+=2, j++)
                t[w*j + lp] = l[i];
        }

        // HOR_SD
        l = line + mh;
        for (lp = 0; lp < lv; lp++){
            int i, j = 0;

            for (i = 0; i < lh; i++)
                l[i] = t[w*lp + i];

            sd_1d53(line, mh, mh + lh);

            // copy back and deinterleave
            for (i =   mh; i < lh; i+=2, j++)
                t[w*lp + j] = l[i];
            for (i = 1-mh; i < lh; i+=2, j++)
                t[w*lp + j] = l[i];
        }
    }
}
static void sd_1d97_float(float *p, int i0, int i1)
{
    int i;

    if (i1 <= i0 + 1) {
        if (i0 == 1)
            p[1] *= F_LFTG_X * 2;
        else
            p[0] *= F_LFTG_K;
        return;
    }

    extend97_float(p, i0, i1);
    i0++; i1++;

    for (i = (i0>>1) - 2; i < (i1>>1) + 1; i++)
        p[2*i+1] -= 1.586134 * (p[2*i] + p[2*i+2]);
    for (i = (i0>>1) - 1; i < (i1>>1) + 1; i++)
        p[2*i] -= 0.052980 * (p[2*i-1] + p[2*i+1]);
    for (i = (i0>>1) - 1; i < (i1>>1); i++)
        p[2*i+1] += 0.882911 * (p[2*i] + p[2*i+2]);
    for (i = (i0>>1); i < (i1>>1); i++)
        p[2*i] += 0.443506 * (p[2*i-1] + p[2*i+1]);
}

static void dwt_encode97_float(DWTContext *s, float *t)
{
    int lev,
        w = s->linelen[s->ndeclevels-1][0];
    float *line = s->f_linebuf;
    line += 5;

    for (lev = s->ndeclevels-1; lev >= 0; lev--){
        int lh = s->linelen[lev][0],
            lv = s->linelen[lev][1],
            mh = s->mod[lev][0],
            mv = s->mod[lev][1],
            lp;
        float *l;

        // HOR_SD
        l = line + mh;
        for (lp = 0; lp < lv; lp++){
            int i, j = 0;

            for (i = 0; i < lh; i++)
                l[i] = t[w*lp + i];

            sd_1d97_float(line, mh, mh + lh);

            // copy back and deinterleave
            for (i =   mh; i < lh; i+=2, j++)
                t[w*lp + j] = l[i];
            for (i = 1-mh; i < lh; i+=2, j++)
                t[w*lp + j] = l[i];
        }

        // VER_SD
        l = line + mv;
        for (lp = 0; lp < lh; lp++) {
            int i, j = 0;

            for (i = 0; i < lv; i++)
                l[i] = t[w*i + lp];

            sd_1d97_float(line, mv, mv + lv);

            // copy back and deinterleave
            for (i =   mv; i < lv; i+=2, j++)
                t[w*j + lp] = l[i];
            for (i = 1-mv; i < lv; i+=2, j++)
                t[w*j + lp] = l[i];
        }
    }
}

static void sd_1d97_int(int *p, int i0, int i1)
{
    int i;

    if (i1 <= i0 + 1) {
        if (i0 == 1)
            p[1] = (p[1] * I_LFTG_X + (1<<14)) >> 15;
        else
            p[0] = (p[0] * I_LFTG_K + (1<<15)) >> 16;
        return;
    }

    extend97_int(p, i0, i1);
    i0++; i1++;

    for (i = (i0>>1) - 2; i < (i1>>1) + 1; i++)
        p[2 * i + 1] -= (I_LFTG_ALPHA * (p[2 * i]     + p[2 * i + 2]) + (1 << 15)) >> 16;
    for (i = (i0>>1) - 1; i < (i1>>1) + 1; i++)
        p[2 * i]     -= (I_LFTG_BETA  * (p[2 * i - 1] + p[2 * i + 1]) + (1 << 15)) >> 16;
    for (i = (i0>>1) - 1; i < (i1>>1); i++)
        p[2 * i + 1] += (I_LFTG_GAMMA * (p[2 * i]     + p[2 * i + 2]) + (1 << 15)) >> 16;
    for (i = (i0>>1); i < (i1>>1); i++)
        p[2 * i]     += (I_LFTG_DELTA * (p[2 * i - 1] + p[2 * i + 1]) + (1 << 15)) >> 16;
}

static void dwt_encode97_int(DWTContext *s, int *t)
{
    int lev;
    int w = s->linelen[s->ndeclevels-1][0];
    int h = s->linelen[s->ndeclevels-1][1];
    int i;
    int *line = s->i_linebuf;
    line += 5;

    for (i = 0; i < w * h; i++)
        t[i] *= 1 << I_PRESHIFT;

    for (lev = s->ndeclevels-1; lev >= 0; lev--){
        int lh = s->linelen[lev][0],
            lv = s->linelen[lev][1],
            mh = s->mod[lev][0],
            mv = s->mod[lev][1],
            lp;
        int *l;

        // VER_SD
        l = line + mv;
        for (lp = 0; lp < lh; lp++) {
            int i, j = 0;

            for (i = 0; i < lv; i++)
                l[i] = t[w*i + lp];

            sd_1d97_int(line, mv, mv + lv);

            // copy back and deinterleave
            for (i =   mv; i < lv; i+=2, j++)
                t[w*j + lp] = ((l[i] * I_LFTG_X) + (1 << 15)) >> 16;
            for (i = 1-mv; i < lv; i+=2, j++)
                t[w*j + lp] = l[i];
        }

        // HOR_SD
        l = line + mh;
        for (lp = 0; lp < lv; lp++){
            int i, j = 0;

            for (i = 0; i < lh; i++)
                l[i] = t[w*lp + i];

            sd_1d97_int(line, mh, mh + lh);

            // copy back and deinterleave
            for (i =   mh; i < lh; i+=2, j++)
                t[w*lp + j] = ((l[i] * I_LFTG_X) + (1 << 15)) >> 16;
            for (i = 1-mh; i < lh; i+=2, j++)
                t[w*lp + j] = l[i];
        }

    }

    for (i = 0; i < w * h; i++)
        t[i] = (t[i] + ((1<<I_PRESHIFT)>>1)) >> I_PRESHIFT;
}

static void sr_1d53(unsigned *p, int i0, int i1)
{
    int i;

    if (i1 <= i0 + 1) {
        if (i0 == 1)
            p[1] = (int)p[1] >> 1;
        return;
    }

    extend53(p, i0, i1);

    for (i = (i0 >> 1); i < (i1 >> 1) + 1; i++)
        p[2 * i] -= (int)(p[2 * i - 1] + p[2 * i + 1] + 2) >> 2;
    for (i = (i0 >> 1); i < (i1 >> 1); i++)
        p[2 * i + 1] += (int)(p[2 * i] + p[2 * i + 2]) >> 1;
}

static void idwt_2d_interleave_int(void *in, void *LL, void *HL, void *LH, void *HH,
                                     int32_t u0, int32_t u1, int32_t v0, int32_t v1, int32_t w) {
    int32_t *buf = (int32_t *)in;
    const int32_t stride = w + (8 - w % 8);
    const int32_t v_offset = v0 % 2;
    const int32_t u_offset = u0 % 2;
    int32_t *sp[4] = { (int32_t *)LL, (int32_t *)HL, (int32_t *)LH, (int32_t *)HH };
    const int32_t vstart[4] = { ff_jpeg2000_ceildiv(v0, 2), ff_jpeg2000_ceildiv(v0, 2), v0 / 2, v0 / 2 };
    const int32_t vstop[4] = { ff_jpeg2000_ceildiv(v1, 2), ff_jpeg2000_ceildiv(v1, 2), v1 / 2, v1 / 2 };
    const int32_t ustart[4] = { ff_jpeg2000_ceildiv(u0, 2), u0 / 2, ff_jpeg2000_ceildiv(u0, 2), u0 / 2 };
    const int32_t ustop[4] = { ff_jpeg2000_ceildiv(u1, 2), u1 / 2, ff_jpeg2000_ceildiv(u1, 2), u1 / 2 };
    const int32_t voffset[4] = { v_offset, v_offset, 1 - v_offset, 1 - v_offset };
    const int32_t uoffset[4] = { u_offset, 1 - u_offset, u_offset, 1 - u_offset };

    for (uint8_t b = 0; b < 4; ++b) {
        for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
            int32_t *line = sp[b] + v * w;
            for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
                buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
            }
        }
    }
}

static void idwt_2d_interleave_float(void *in, void *LL, void *HL, void *LH, void *HH,
                               int32_t u0, int32_t u1, int32_t v0, int32_t v1, int32_t w) {
    float *buf = in;
    const int32_t stride = w + (8 - w % 8);
    const int32_t v_offset = v0 % 2;
    const int32_t u_offset = u0 % 2;
    float *sp[4] = { (float *)LL, (float *)HL, (float *)LH, (float *)HH };
    const int32_t vstart[4] = { ff_jpeg2000_ceildiv(v0, 2), ff_jpeg2000_ceildiv(v0, 2), v0 / 2, v0 / 2 };
    const int32_t vstop[4] = { ff_jpeg2000_ceildiv(v1, 2), ff_jpeg2000_ceildiv(v1, 2), v1 / 2, v1 / 2 };
    const int32_t ustart[4] = { ff_jpeg2000_ceildiv(u0, 2), u0 / 2, ff_jpeg2000_ceildiv(u0, 2), u0 / 2 };
    const int32_t ustop[4] = { ff_jpeg2000_ceildiv(u1, 2), u1 / 2, ff_jpeg2000_ceildiv(u1, 2), u1 / 2 };
    const int32_t voffset[4] = { v_offset, v_offset, 1 - v_offset, 1 - v_offset };
    const int32_t uoffset[4] = { u_offset, 1 - u_offset, u_offset, 1 - u_offset };

    for (uint8_t b = 0; b < 4; ++b) {
      for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
        float *line = sp[b] + v * w;
        for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
          buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
        }
      }
    }
}

static inline void extr_1d_int(int32_t *extbuf, int32_t *buf, const int32_t left, const int32_t right,
                                  const int32_t i0, const int32_t i1) {
    memcpy(extbuf + left, buf, sizeof(int32_t) * (size_t)((i1 - i0)));
    for (int32_t i = 1; i <= left; ++i) {
        extbuf[left - i] = buf[PSEo(i0 - i, i0, i1)];
    }
    for (int32_t i = 1; i <= right; ++i) {
        extbuf[left + (i1 - i0) + i - 1] = buf[PSEo(i1 - i0 + i - 1 + i0, i0, i1)];
    }
}

static inline void extr_1d_float(float *extbuf, float *buf, const int32_t left, const int32_t right,
                                  const int32_t i0, const int32_t i1) {
    memcpy(extbuf + left, buf, sizeof(float) * (size_t)((i1 - i0)));
    for (int32_t i = 1; i <= left; ++i) {
        extbuf[left - i] = buf[PSEo(i0 - i, i0, i1)];
    }
    for (int32_t i = 1; i <= right; ++i) {
        extbuf[left + (i1 - i0) + i - 1] = buf[PSEo(i1 - i0 + i - 1 + i0, i0, i1)];
    }
}

static void idwt_1d_filtr_53(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    int32_t *X = (int32_t *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    for (int32_t n = 0 + offset, i = start; i < stop + 1; ++i, n += 2) {
        X[n] -= (X[n - 1] + X[n + 1] + 2) >> 2;
    }

    for (int32_t n = 0 + offset, i = start; i < stop; ++i, n += 2) {
        X[n + 1] += (X[n] + X[n + 2]) >> 1;
    }
}

static void idwt_1d_filtr_97(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    float *X = (float *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    float sum;
    /* K and 1/K have been already done by dequantization */
    for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
        sum = X[n - 1];
        sum += X[n + 1];
        X[n] -= F_LFTG_DELTA * sum;
    }
    for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
        sum = X[n];
        sum += X[n + 2];
        X[n + 1] -= F_LFTG_GAMMA * sum;
    }
    for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
        sum = X[n - 1];
        sum += X[n + 1];
        X[n] -= -F_LFTG_BETA * sum;
    }
    for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
        sum = X[n];
        sum += X[n + 2];
        X[n + 1] -= -F_LFTG_ALPHA * sum;
    }
}

static void idwt_1d_filtr_97_int(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    int32_t *X = (int32_t *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    int64_t sum;
    /* K and 1/K have been already done by dequantization */
    for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
        sum = X[n - 1];
        sum += X[n + 1];
        X[n] -= (I_LFTG_DELTA * sum + (1 << (I_LFTG_DELTA_SHIFT - 1))) >> I_LFTG_DELTA_SHIFT;
    }
    for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
        sum = X[n];
        sum += X[n + 2];
        X[n + 1] -= (I_LFTG_GAMMA * sum + (1 << (I_LFTG_GAMMA_SHIFT - 1))) >> I_LFTG_GAMMA_SHIFT;
    }
    for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
        sum = X[n - 1];
        sum += X[n + 1];
        X[n] -= (-I_LFTG_BETA * sum + (1 << (I_LFTG_BETA_SHIFT - 1))) >> I_LFTG_BETA_SHIFT;
    }
    for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
        sum = X[n];
        sum += X[n + 2];
        X[n + 1] += sum;
        X[n + 1] -= (-I_LFTG_ALPHA * sum + (1 << (I_LFTG_ALPHA_SHIFT - 1))) >> I_LFTG_ALPHA_SHIFT;
    }
}

static void idwt_1d_sr_53(int32_t *buf, int32_t *in, const int32_t left, const int32_t right,
                          const int32_t i0, const int32_t i1) {
    extr_1d_int(buf, in, left, right, i0, i1);
    filtr(buf, left, i0, i1);
    memcpy(in, buf + left, sizeof(int32_t) * ((size_t)(i1 - i0)));
}

static void idwt_1d_sr_97(float *buf, float *in, const int32_t left, const int32_t right,
                          const int32_t i0, const int32_t i1) {
    extr_1d_float(buf, in, left, right, i0, i1);
    filtr(buf, left, i0, i1);
    memcpy(in, buf + left, sizeof(float) * ((size_t)(i1 - i0)));
}

static void idwt_1d_sr_97_int(int32_t *buf, float *in, const int32_t left, const int32_t right,
                          const int32_t i0, const int32_t i1) {
    extr_1d_int(buf, in, left, right, i0, i1);
    filtr(buf, left, i0, i1);
    memcpy(in, buf + left, sizeof(int32_t) * ((size_t)(i1 - i0)));
}

static void idwt_hor_sr_53(void *buf, void *line, const int32_t u0, const int32_t u1, const int32_t v0,
                           const int32_t v1, const int32_t w) {
    int32_t *in = buf;
    int32_t *linebuf = line;
    const int32_t stride               = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = { 1, 2 };
    const int32_t num_pse_i1[2] = { 2, 1 };
    const int32_t left                 = num_pse_i0[u0 % 2];
    const int32_t right                = num_pse_i1[u1 % 2];

    if (u0 == u1 - 1) {
        // one sample case
        for (int32_t row = 0; row < v1 - v0; ++row) {
            if (u0 % 2 != 0) {
                in[row * stride] = (int32_t)(in[row] >> 1);
            }
        }
    } else {
        // need to perform symmetric extension
        for (int32_t row = 0; row < v1 - v0; ++row) {
            idwt_1d_sr_53(linebuf, in, left, right, u0, u1);
            in += stride;
        }
    }
}

static void idwt_hor_sr_97(void *buf, void *line, const int32_t u0, const int32_t u1, const int32_t v0,
                           const int32_t v1, const int32_t w) {
    float *in = buf;
    float *linebuf = line;
    const int32_t stride               = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = {3, 4};
    const int32_t num_pse_i1[2] = {4, 3};
    const int32_t left                 = num_pse_i0[u0 % 2];
    const int32_t right                = num_pse_i1[u1 % 2];

    if (u0 == u1 - 1) {
        // one sample case
        for (int32_t row = 0; row < v1 - v0; ++row) {
            in[row * stride] *= (u0 % 2) ? F_LFTG_K / 2 : F_LFTG_X;
        }
    } else {
        // need to perform symmetric extension
        for (int32_t row = 0; row < v1 - v0; ++row) {
            idwt_1d_sr_97(linebuf, in, left, right, u0, u1);
            in += stride;
        }
    }
}

static void idwt_hor_sr_97_int(void *buf, void *line,  const int32_t u0, const int32_t u1, const int32_t v0,
                           const int32_t v1, const int32_t w) {
    int32_t *in = buf;
    int32_t *linebuf = line;
    const int32_t stride               = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = {3, 4};
    const int32_t num_pse_i1[2] = {4, 3};
    const int32_t left                 = num_pse_i0[u0 % 2];
    const int32_t right                = num_pse_i1[u1 % 2];

    if (u0 == u1 - 1) {
        // one sample case
        for (int32_t row = 0; row < v1 - v0; ++row) {
            if (u0 % 2)
                in[row * stride] = (in[row * stride] * I_LFTG_K + (1 << 16)) >> 17;
            else
                in[row * stride] = (in[row * stride] * I_LFTG_X + (1 << 15)) >> 16;
        }
    } else {
        // need to perform symmetric extension
        for (int32_t row = 0; row < v1 - v0; ++row) {
            idwt_1d_sr_97_int(linebuf, in, left, right, u0, u1);
            in += stride;
        }
    }
}

static void idwt_ver_sr_53(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                    const int32_t v1, const int32_t w) {
    int32_t *in = src;
    const int32_t stride            = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = {1, 2};
    const int32_t num_pse_i1[2] = {2, 1};
    const int32_t top               = num_pse_i0[v0 % 2];
    const int32_t bottom            = num_pse_i1[v1 % 2];
    if (v0 == v1 - 1 && (v0 % 2)) {
        // one sample case
        for (int32_t col = 0; col < u1 - u0; ++col) {
            in[col] = (int32_t)(in[col] >> 1);
        }
    } else {
        int32_t **buf        = (int32_t **) av_mallocz((size_t)(top + v1 - v0 + bottom) * sizeof(int32_t *));
        for (int32_t i = 1; i <= top; ++i) {
            buf[top - i] =
                    (int32_t *)(av_mallocz(sizeof(int32_t) * (size_t)stride));
            memcpy(buf[top - i], &in[PSEo(v0 - i, v0, v1) * stride],
                   sizeof(int32_t) * (size_t)(stride));
        }
        for (int32_t row = 0; row < v1 - v0; ++row) {
            buf[top + row] = &in[row * stride];
        }
        for (int32_t i = 1; i <= bottom; i++) {
            buf[top + (v1 - v0) + i - 1] =
                    (int32_t *)(av_mallocz(sizeof(int32_t) * (size_t)stride));
            memcpy(buf[top + (v1 - v0) + i - 1], &in[PSEo(v1 - v0 + i - 1 + v0, v0, v1) * stride],
                   sizeof(int32_t) * (size_t)(stride));
        }
        const int32_t start  = v0 / 2;
        const int32_t stop   = v1 / 2;
        const int32_t offset = top - v0 % 2;

        for (int32_t n = 0 + offset, i = start; i < stop + 1; ++i, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int32_t sum = buf[n - 1][col];
                sum += buf[n + 1][col];
                buf[n][col] = (int32_t)(buf[n][col] - ((sum + 2) >> 2));
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; ++i, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int32_t sum = buf[n][col];
                sum += buf[n + 2][col];
                buf[n + 1][col] = (int32_t)(buf[n + 1][col] + (sum >> 1));
            }
        }

        for (int32_t i = 1; i <= top; ++i) {
            av_freep(&buf[top - i]);
        }
        for (int32_t i = 1; i <= bottom; i++) {
            av_freep(&buf[top + (v1 - v0) + i - 1]);
        }
        av_freep(&buf);
    }
}

static void idwt_ver_sr_97(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                             const int32_t v1, const int32_t w) {
    float *in = src;
    const int32_t stride            = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = {3, 4};
    const  int32_t num_pse_i1[2] = {4, 3};
    const int32_t top               = num_pse_i0[v0 % 2];
    const int32_t bottom            = num_pse_i1[v1 % 2];
    if (v0 == v1 - 1) {
        // one sample case
        for (int32_t col = 0; col < u1 - u0; ++col) {
            in[col] *= (v0 % 2) ? F_LFTG_K / 2 : F_LFTG_X;
        }
    } else {
        float **buf        = (float **)av_mallocz((size_t)(top + v1 - v0 + bottom) * sizeof(float *));
        for (int32_t i = 1; i <= top; ++i) {
            buf[top - i] =
                    (float *)(av_mallocz(sizeof(float) * (size_t)stride));
            memcpy(buf[top - i], &in[PSEo(v0 - i, v0, v1) * stride],
                   sizeof(float) * (size_t)(stride));
        }
        for (int32_t row = 0; row < v1 - v0; ++row) {
            buf[top + row] = &in[row * stride];
        }
        for (int32_t i = 1; i <= bottom; i++) {
            buf[top + (v1 - v0) + i - 1] =
                    (float *)(av_mallocz(sizeof(float) * (size_t)stride));
            memcpy(buf[top + (v1 - v0) + i - 1], &in[PSEo(v1 - v0 + i - 1 + v0, v0, v1) * stride],
                   sizeof(float) * (size_t)(stride));
        }
        const int32_t start  = v0 / 2;
        const int32_t stop   = v1 / 2;
        const int32_t offset = top - v0 % 2;

        for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                float sum = buf[n - 1][col];
                sum += buf[n + 1][col];
                buf[n][col] -= F_LFTG_DELTA* sum;
            }
        }
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                float sum = buf[n][col];
                sum += buf[n + 2][col];
                buf[n + 1][col] -= F_LFTG_GAMMA * sum;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                float sum = buf[n - 1][col];
                sum += buf[n + 1][col];
                buf[n][col] -= -F_LFTG_BETA * sum;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                float sum = buf[n][col];
                sum += buf[n + 2][col];
                buf[n + 1][col] -= -F_LFTG_ALPHA * sum;
            }
        }

        for (int32_t i = 1; i <= top; ++i) {
            av_freep(&buf[top - i]);
        }
        for (int32_t i = 1; i <= bottom; i++) {
            av_freep(&buf[top + (v1 - v0) + i - 1]);
        }
        av_freep(&buf);
    }
}

static void idwt_ver_sr_97_int(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                             const int32_t v1, const int32_t w) {
    int32_t *in = src;
    const int32_t stride            = w + (8 - w % 8);
    const int32_t num_pse_i0[2] = {3, 4};
    const  int32_t num_pse_i1[2] = {4, 3};
    const int32_t top               = num_pse_i0[v0 % 2];
    const int32_t bottom            = num_pse_i1[v1 % 2];
    if (v0 == v1 - 1) {
        // one sample case
        for (int32_t col = 0; col < u1 - u0; ++col) {
            if (v0 % 2)
                in[col] = (in[col] * I_LFTG_K + (1 << 16)) >> 17;
            else
                in[col] = (in[col] * I_LFTG_X + (1 << 15)) >> 16;
        }
    } else {
        int32_t **buf        = (int32_t **)av_mallocz((size_t)(top + v1 - v0 + bottom) * sizeof(int32_t *));
        for (int32_t i = 1; i <= top; ++i) {
            buf[top - i] =
                    (int32_t *)(av_mallocz(sizeof(int32_t) * (size_t)stride));
            memcpy(buf[top - i], &in[PSEo(v0 - i, v0, v1) * stride],
                   sizeof(int32_t) * (size_t)(stride));
        }
        for (int32_t row = 0; row < v1 - v0; ++row) {
            buf[top + row] = &in[row * stride];
        }
        for (int32_t i = 1; i <= bottom; i++) {
            buf[top + (v1 - v0) + i - 1] =
                    (int32_t *)(av_mallocz(sizeof(int32_t) * (size_t)stride));
            memcpy(buf[top + (v1 - v0) + i - 1], &in[PSEo(v1 - v0 + i - 1 + v0, v0, v1) * stride],
                   sizeof(int32_t) * (size_t)(stride));
        }
        const int32_t start  = v0 / 2;
        const int32_t stop   = v1 / 2;
        const int32_t offset = top - v0 % 2;

        for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int64_t sum = buf[n - 1][col];
                sum += buf[n + 1][col];
                int64_t tmp0 = I_LFTG_DELTA* sum;
                int64_t tmp1 = tmp0 + (1 << (I_LFTG_DELTA_SHIFT - 1));
                int64_t tmp2 = tmp1 >> I_LFTG_DELTA_SHIFT;
                buf[n][col] -= (I_LFTG_DELTA* sum + (1 << (I_LFTG_DELTA_SHIFT - 1))) >> I_LFTG_DELTA_SHIFT;
            }
        }
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int64_t sum = buf[n][col];
                sum += buf[n + 2][col];
                buf[n + 1][col] -= (I_LFTG_GAMMA * sum + (1 << (I_LFTG_GAMMA_SHIFT - 1))) >> I_LFTG_GAMMA_SHIFT;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int64_t sum = buf[n - 1][col];
                sum += buf[n + 1][col];
                buf[n][col] -= (-I_LFTG_BETA * sum + (1 << (I_LFTG_BETA_SHIFT - 1))) >> I_LFTG_BETA_SHIFT;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
            for (int32_t col = 0; col < u1 - u0; ++col) {
                int64_t sum = buf[n][col];
                sum += buf[n + 2][col];
                buf[n + 1][col] += sum;
                buf[n + 1][col] -= (-I_LFTG_ALPHA * sum + (1 << (I_LFTG_ALPHA_SHIFT - 1))) >> I_LFTG_ALPHA_SHIFT;
            }
        }

        for (int32_t i = 1; i <= top; ++i) {
            av_freep(&buf[top - i]);
        }
        for (int32_t i = 1; i <= bottom; i++) {
            av_freep(&buf[top + (v1 - v0) + i - 1]);
        }
        av_freep(&buf);
    }
}

static void dwt_decode53(DWTContext *s, int *t)
{
    int lev;
    int w     = s->linelen[s->ndeclevels - 1][0];
    const int stride = w + (8 - w % 8);
    int32_t *line = s->i_linebuf;

    uint32_t offset[4];
    int32_t *LL, *HL, *LH, *HH;
    int32_t *buf = (int32_t *) av_mallocz(sizeof(int32_t) * stride * s->linelen[s->ndeclevels-1][1]);

#if HAVE_AVX2
    idwt_2d_interleave = idwt_2d_interleave_int_avx2;
    filtr = idwt_1d_filtr_53_avx2;
    ver_sr = idwt_ver_sr_53_avx2;
#elif HAVE_NEON
    idwt_2d_interleave = idwt_2d_interleave_int_neon;
    filtr = idwt_1d_filtr_53;
    ver_sr = idwt_ver_sr_53;
#else
    idwt_2d_interleave = idwt_2d_interleave_int;
    filtr = idwt_1d_filtr_53;
    ver_sr = idwt_ver_sr_53;
#endif
    for (lev = 0; lev < s->ndeclevels; lev++) {
        offset[0] = 0;
        offset[1] = s->u1[lev+1][0] - s->u0[lev+1][0];
        offset[2] = (s->v1[lev+1][0] - s->v0[lev+1][0]) * w;
        offset[3] = offset[1] + offset[2];
        LL = t + offset[0];
        HL = t + offset[1];
        LH = t + offset[2];
        HH = t + offset[3];


        idwt_2d_interleave(buf, LL, HL, LH, HH, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        idwt_hor_sr_53(buf, s->i_linebuf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        ver_sr(buf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);

        for (int i = 0; i < s->linelen[lev][1]; ++i) {
            memcpy(t + w * i, buf + i * stride, (s->linelen[lev][0]) * sizeof(int32_t));
        }
    }
    av_freep(&buf);
}

static void dwt_decode97_float(DWTContext *s, float *t)
{
    int lev;
    int w     = s->linelen[s->ndeclevels - 1][0];
    const int stride = w + (8 - w % 8);

    uint32_t offset[4];
    float *LL, *HL, *LH, *HH;
    float *buf = (float *) av_mallocz(sizeof(float) * stride * s->linelen[s->ndeclevels - 1][1]);
#if HAVE_AVX2
    idwt_2d_interleave = idwt_2d_interleave_float_avx2;
    filtr = idwt_1d_filtr_97_avx2;
    ver_sr = idwt_ver_sr_97_avx2;
#elif HAVE_NEON
    idwt_2d_interleave = idwt_2d_interleave_float_neon;
    filtr = idwt_1d_filtr_97_neon;
    ver_sr = idwt_ver_sr_97_neon;
#else
    idwt_2d_interleave = idwt_2d_interleave_float;
    filtr = idwt_1d_filtr_97;
    ver_sr = idwt_ver_sr_97;
#endif
    for (lev = 0; lev < s->ndeclevels; lev++) {
        offset[0] = 0;
        offset[1] = s->u1[lev+1][0] - s->u0[lev+1][0];
        offset[2] = (s->v1[lev+1][0] - s->v0[lev+1][0]) * w;
        offset[3] = offset[1] + offset[2];
        LL = t + offset[0];
        HL = t + offset[1];
        LH = t + offset[2];
        HH = t + offset[3];
        // TODO: -lowres !!!!!

        idwt_2d_interleave(buf, LL, HL, LH, HH, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        idwt_hor_sr_97(buf, s->f_linebuf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        ver_sr(buf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);

        for (int i = 0; i < s->linelen[lev][1]; ++i) {
            memcpy(t + w * i, buf + i * stride, (s->linelen[lev][0]) * sizeof(float));
        }
    }

    av_freep(&buf);
}

static void dwt_decode97_int(DWTContext *s, int32_t *t)
{
    int lev;
    int w     = s->linelen[s->ndeclevels - 1][0];
    int h = s->linelen[s->ndeclevels - 1][1];
    const int stride = w + (8 - w % 8);

    uint32_t offset[4];
    int32_t *LL, *HL, *LH, *HH;
    int32_t *buf = (int32_t *) av_mallocz(sizeof(int32_t) * stride * h);
#if HAVE_AVX2
    idwt_2d_interleave = idwt_2d_interleave_int_avx2;
    filtr = idwt_1d_filtr_97_int_avx2;
    ver_sr = idwt_ver_sr_97_int_avx2;
#elif HAVE_NEON
    idwt_2d_interleave = idwt_2d_interleave_int_neon;
    filtr = idwt_1d_filtr_97_int;
    ver_sr = idwt_ver_sr_97_int;
#else
    idwt_2d_interleave = idwt_2d_interleave_int;
    filtr = idwt_1d_filtr_97_int;
    ver_sr = idwt_ver_sr_97_int;
#endif
    for (int i = 0; i < w * h; i++)
        t[i] *= 1 << I_PRESHIFT;
    for (lev = 0; lev < s->ndeclevels; lev++) {
        offset[0] = 0;
        offset[1] = s->u1[lev+1][0] - s->u0[lev+1][0];
        offset[2] = (s->v1[lev+1][0] - s->v0[lev+1][0]) * w;
        offset[3] = offset[1] + offset[2];
        LL = t + offset[0];
        HL = t + offset[1];
        LH = t + offset[2];
        HH = t + offset[3];
        // TODO: -lowres !!!!!

        idwt_2d_interleave(buf, LL, HL, LH, HH, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        idwt_hor_sr_97_int(buf, s->i_linebuf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);
        ver_sr(buf, s->u[lev][0], s->u[lev][1], s->v[lev][0], s->v[lev][1], w);

        for (int i = 0; i < s->linelen[lev][1]; ++i) {
            memcpy(t + w * i, buf + i * stride, (s->linelen[lev][0]) * sizeof(int32_t));
        }
    }
    for (int i = 0; i < w * h; i++)
        // Shifting down to the binary point.
        // In FF_DWT97_INT, the binary point of the input coefficients is 1 bit above from the LSB.
        // So, we need `>> (I_PRESHIFT + 1)`  here.
        t[i] = (t[i] + ((1<<(I_PRESHIFT + 1))>>1)) >> (I_PRESHIFT + 1);
    av_freep(&buf);
}

int ff_jpeg2000_dwt_init(DWTContext *s, int border[2][2],
                         int decomp_levels, int type)
{
    int i, j, lev = decomp_levels, maxlen,
        b[2][2];

    s->ndeclevels = decomp_levels;
    s->type       = type;

    for (i = 0; i < 2; i++)
        for (j = 0; j < 2; j++)
            b[i][j] = border[i][j];

    maxlen = FFMAX(b[0][1] - b[0][0],
                   b[1][1] - b[1][0]);
    while (--lev >= 0) {
        s->u[lev][0] = b[0][0];
        s->u[lev][1] = b[0][1];
        s->v[lev][0] = b[1][0];
        s->v[lev][1] = b[1][1];
        for (i = 0; i < 2; i++) {
            s->linelen[lev][i] = b[i][1] - b[i][0];
            s->mod[lev][i]     = b[i][0] & 1;
            for (j = 0; j < 2; j++)
                b[i][j] = (b[i][j] + 1) >> 1;
        }
    }
    switch (type) {
    case FF_DWT97:
        s->f_linebuf = av_malloc_array((maxlen + 12), sizeof(*s->f_linebuf));
        if (!s->f_linebuf)
            return AVERROR(ENOMEM);
        break;
     case FF_DWT97_INT:
        s->i_linebuf = av_malloc_array((maxlen + 12), sizeof(*s->i_linebuf));
        if (!s->i_linebuf)
            return AVERROR(ENOMEM);
        break;
    case FF_DWT53:
        s->i_linebuf = av_malloc_array((maxlen +  16), sizeof(*s->i_linebuf));
        if (!s->i_linebuf)
            return AVERROR(ENOMEM);
        break;
    default:
        return -1;
    }
    return 0;
}

int ff_dwt_encode(DWTContext *s, void *t)
{
    if (s->ndeclevels == 0)
        return 0;

    switch(s->type){
        case FF_DWT97:
            dwt_encode97_float(s, t); break;
        case FF_DWT97_INT:
            dwt_encode97_int(s, t); break;
        case FF_DWT53:
            dwt_encode53(s, t); break;
        default:
            return -1;
    }
    return 0;
}

int ff_dwt_decode(DWTContext *s, void *t)
{
    if (s->ndeclevels == 0)
        return 0;

    switch (s->type) {
    case FF_DWT97:
        dwt_decode97_float(s, t);
        break;
    case FF_DWT97_INT:
        dwt_decode97_int(s, t);
        break;
    case FF_DWT53:
        dwt_decode53(s, t);
        break;
    default:
        return -1;
    }
    return 0;
}

void ff_dwt_destroy(DWTContext *s)
{
    av_freep(&s->f_linebuf);
    av_freep(&s->i_linebuf);
}

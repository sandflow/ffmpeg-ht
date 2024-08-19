#include "libavcodec/jpeg2000.h"
#include "libavcodec/jpeg2000dwt.h"
#include "libavcodec/aarch64/jpeg2000dwt_neon.h"
#include "libavutil/mem.h"

#include <arm_neon.h>

void idwt_2d_interleave_int_neon(void *in, void *LL, void *HL, void *LH, void *HH,
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

    if ((ustop[0] - ustart[0]) != (ustop[1] - ustart[1])) {
        for (uint8_t b = 0; b < 2; ++b) {
            for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
                int32_t *line = sp[b] + v * w;
                for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
                    buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
                }
            }
        }
    } else {
        int32_t *first, *second;
        first  = sp[0];
        second = sp[1];
        if (uoffset[0] > uoffset[1]) {
            first  = sp[1];
            second = sp[0];
        }

        int32x4_t vfirst0, vfirst1, vsecond0, vsecond1;
        for (int32_t v = 0, vb = vstart[0]; vb < vstop[0]; ++vb, ++v) {
            int32_t *dp = buf + (2 * v + voffset[0]) * stride;
            int32_t len  = ustop[0] - ustart[0];
            int32_t *line0 = first + v * w;
            int32_t *line1 = second + v * w;
            for (; len >= 8; len -= 8) {
                vfirst0  = vld1q_s32(line0);
                vsecond0 = vld1q_s32(line1);
                vst1q_s32(dp, vzip1q_s32(vfirst0, vsecond0));
                vst1q_s32(dp + 4, vzip2q_s32(vfirst0, vsecond0));
                vfirst1  = vld1q_s32(line0 + 4);
                vsecond1 = vld1q_s32(line1 + 4);
                vst1q_s32(dp + 8, vzip1q_s32(vfirst1, vsecond1));
                vst1q_s32(dp + 12, vzip2q_s32(vfirst1, vsecond1));
                line0 += 8;
                line1 += 8;
                dp += 16;
            }
            for (; len > 0; --len) {
                *dp++ = *line0++;
                *dp++ = *line1++;
            }
        }
    }

    if ((ustop[2] - ustart[2]) != (ustop[3] - ustart[3])) {
        for (uint8_t b = 2; b < 4; ++b) {
            for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
                int32_t *line = sp[b] + v * w;
                for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
                    buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
                }
            }
        }
    } else {
        int32_t *first, *second;
        first  = sp[2];
        second = sp[3];
        if (uoffset[2] > uoffset[3]) {
            first  = sp[3];
            second = sp[2];
        }
        int32x4_t vfirst0, vfirst1, vsecond0, vsecond1;
        for (int32_t v = 0, vb = vstart[2]; vb < vstop[2]; ++vb, ++v) {
            int32_t *dp = buf + (2 * v + voffset[2]) * stride;
            int32_t len  = ustop[2] - ustart[2];
            int32_t *line0 = first + v * w;
            int32_t *line1 = second + v * w;
            for (; len >= 8; len -= 8) {
                vfirst0  = vld1q_s32(line0);
                vsecond0 = vld1q_s32(line1);
                vst1q_s32(dp, vzip1q_s32(vfirst0, vsecond0));
                vst1q_s32(dp + 4, vzip2q_s32(vfirst0, vsecond0));
                vfirst1  = vld1q_s32(line0 + 4);
                vsecond1 = vld1q_s32(line1 + 4);
                vst1q_s32(dp + 8, vzip1q_s32(vfirst1, vsecond1));
                vst1q_s32(dp + 12, vzip2q_s32(vfirst1, vsecond1));
                line0 += 8;
                line1 += 8;
                dp += 16;
            }
            for (; len > 0; --len) {
                *dp++ = *line0++;
                *dp++ = *line1++;
            }
        }
    }
}

void idwt_2d_interleave_float_neon(void *in, void *LL, void *HL, void *LH, void *HH,
                                    int32_t u0, int32_t u1, int32_t v0, int32_t v1, int32_t w) {
    float *buf = (float *)in;
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

    if ((ustop[0] - ustart[0]) != (ustop[1] - ustart[1])) {
        for (uint8_t b = 0; b < 2; ++b) {
            for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
                float *line = sp[b] + v * w;
                for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
                    buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
                }
            }
        }
    } else {
        float *first, *second;
        first  = sp[0];
        second = sp[1];
        if (uoffset[0] > uoffset[1]) {
            first  = sp[1];
            second = sp[0];
        }

        float32x4_t vfirst0, vfirst1, vsecond0, vsecond1;
        for (int32_t v = 0, vb = vstart[0]; vb < vstop[0]; ++vb, ++v) {
            float *dp = buf + (2 * v + voffset[0]) * stride;
            int32_t len  = ustop[0] - ustart[0];
            float *line0 = first + v * w;
            float *line1 = second + v * w;
            for (; len >= 8; len -= 8) {
                vfirst0  = vld1q_f32(line0);
                vsecond0 = vld1q_f32(line1);
                vst1q_f32(dp, vzip1q_f32(vfirst0, vsecond0));
                vst1q_f32(dp + 4, vzip2q_f32(vfirst0, vsecond0));
                vfirst1  = vld1q_f32(line0 + 4);
                vsecond1 = vld1q_f32(line1 + 4);
                vst1q_f32(dp + 8, vzip1q_f32(vfirst1, vsecond1));
                vst1q_f32(dp + 12, vzip2q_f32(vfirst1, vsecond1));
                line0 += 8;
                line1 += 8;
                dp += 16;
            }
            for (; len > 0; --len) {
                *dp++ = *line0++;
                *dp++ = *line1++;
            }
        }
    }

    if ((ustop[2] - ustart[2]) != (ustop[3] - ustart[3])) {
        for (uint8_t b = 2; b < 4; ++b) {
            for (int32_t v = 0, vb = vstart[b]; vb < vstop[b]; ++vb, ++v) {
                float *line = sp[b] + v * w;
                for (int32_t u = 0, ub = ustart[b]; ub < ustop[b]; ++ub, ++u) {
                    buf[2 * u + uoffset[b] + (2 * v + voffset[b]) * stride] = *(line++);
                }
            }
        }
    } else {
        float *first, *second;
        first  = sp[2];
        second = sp[3];
        if (uoffset[2] > uoffset[3]) {
            first  = sp[3];
            second = sp[2];
        }
        float32x4_t vfirst0, vfirst1, vsecond0, vsecond1;
        for (int32_t v = 0, vb = vstart[2]; vb < vstop[2]; ++vb, ++v) {
            float *dp = buf + (2 * v + voffset[2]) * stride;
            int32_t len  = ustop[2] - ustart[2];
            float *line0 = first + v * w;
            float *line1 = second + v * w;
            for (; len >= 8; len -= 8) {
                vfirst0  = vld1q_f32(line0);
                vsecond0 = vld1q_f32(line1);
                vst1q_f32(dp, vzip1q_f32(vfirst0, vsecond0));
                vst1q_f32(dp + 4, vzip2q_f32(vfirst0, vsecond0));
                vfirst1  = vld1q_f32(line0 + 4);
                vsecond1 = vld1q_f32(line1 + 4);
                vst1q_f32(dp + 8, vzip1q_f32(vfirst1, vsecond1));
                vst1q_f32(dp + 12, vzip2q_f32(vfirst1, vsecond1));
                line0 += 8;
                line1 += 8;
                dp += 16;
            }
            for (; len > 0; --len) {
                *dp++ = *line0++;
                *dp++ = *line1++;
            }
        }
    }
}

// irreversible IDWT
static av_always_inline void idwt_97_float_step0(const int32_t init_pos, const int32_t simdlen, float *const X,
                                                     const int32_t n0, const int32_t n1) {
    float32x4_t vcoeff = vdupq_n_f32(F_LFTG_DELTA);
    for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
        float32x4x2_t xin0 = vld2q_f32(X + n + n0);
        float32x4x2_t xin2 = vld2q_f32(X + n + n1);
        float32x4_t xsum = vaddq_f32(xin0.val[0], xin2.val[0]);
        xin0.val[1] = vfmsq_f32(xin0.val[1], vcoeff, xsum);
        vst2q_f32(X + n + n0, xin0);
    }
};

static av_always_inline void idwt_97_float_step1(const int32_t init_pos, const int32_t simdlen, float *const X,
                                                     const int32_t n0, const int32_t n1) {
    float32x4_t vcoeff = vdupq_n_f32(F_LFTG_GAMMA);
    for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
        float32x4x2_t xin0 = vld2q_f32(X + n + n0);
        float32x4x2_t xin2 = vld2q_f32(X + n + n1);
        float32x4_t xsum = vaddq_f32(xin0.val[0], xin2.val[0]);
        xin0.val[1] = vfmsq_f32(xin0.val[1], vcoeff, xsum);
        vst2q_f32(X + n + n0, xin0);
    }
};

static av_always_inline void idwt_97_float_step2(const int32_t init_pos, const int32_t simdlen, float *const X,
                                                     const int32_t n0, const int32_t n1) {
    float32x4_t vcoeff = vdupq_n_f32(-F_LFTG_BETA);
    for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
        float32x4x2_t xin0 = vld2q_f32(X + n + n0);
        float32x4x2_t xin2 = vld2q_f32(X + n + n1);
        float32x4_t xsum = vaddq_f32(xin0.val[0], xin2.val[0]);
        xin0.val[1] = vfmsq_f32(xin0.val[1], vcoeff, xsum);
        vst2q_f32(X + n + n0, xin0);
    }
};

static av_always_inline void idwt_97_float_step3(const int32_t init_pos, const int32_t simdlen, float *const X,
                                                     const int32_t n0, const int32_t n1) {
    float32x4_t vcoeff = vdupq_n_f32(-F_LFTG_ALPHA);
    for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
        float32x4x2_t xin0 = vld2q_f32(X + n + n0);
        float32x4x2_t xin2 = vld2q_f32(X + n + n1);
        float32x4_t xsum = vaddq_f32(xin0.val[0], xin2.val[0]);
        xin0.val[1] = vfmsq_f32(xin0.val[1], vcoeff, xsum);
        vst2q_f32(X + n + n0, xin0);
    }
};

void idwt_1d_filtr_97_neon(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    float *X = (float *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    // step 1
    int32_t simdlen = stop + 2 - (start - 1);
    idwt_97_float_step0(offset - 2, simdlen, X, -1, 1);

    // step 2
    simdlen = stop + 1 - (start - 1);
    idwt_97_float_step1(offset - 2, simdlen, X, 0, 2);

    // step 3
    simdlen = stop + 1 - start;
    idwt_97_float_step2(offset, simdlen, X, -1, 1);

    // step 4
    simdlen = stop - start;
    idwt_97_float_step3(offset, simdlen, X, 0, 2);
}

void idwt_ver_sr_97_neon(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
                         const int32_t v1, const int32_t w) {
    float *in = (float*)src;
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

        float32x4_t vcoeff;
        float32x4x2_t x0, x1, x2;
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
            float *sp0, *sp1, *sp2;
            sp0 = buf[n - 1], sp1 = buf[n], sp2 = buf[n + 1];
            vcoeff = vdupq_n_f32(F_LFTG_DELTA);
            for (int32_t col = stride; col > 0; col -= 8) {
                x0 = vld2q_f32(sp0);
                x1 = vld2q_f32(sp1);
                x2 = vld2q_f32(sp2);
                x1.val[0] = vfmsq_f32(x1.val[0], vcoeff, vaddq_f32(x0.val[0], x2.val[0]));
                x1.val[1] = vfmsq_f32(x1.val[1], vcoeff, vaddq_f32(x0.val[1], x2.val[1]));
                vst2q_f32(sp1, x1);
                sp0 += 8;
                sp1 += 8;
                sp2 += 8;
            }
        }
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
            float *sp0, *sp1, *sp2;
            sp0 = buf[n], sp1 = buf[n + 1], sp2 = buf[n + 2];
            vcoeff = vdupq_n_f32(F_LFTG_GAMMA);
            for (int32_t col = stride; col > 0; col -= 8) {
                x0 = vld2q_f32(sp0);
                x1 = vld2q_f32(sp1);
                x2 = vld2q_f32(sp2);
                x1.val[0] = vfmsq_f32(x1.val[0], vcoeff, vaddq_f32(x0.val[0], x2.val[0]));
                x1.val[1] = vfmsq_f32(x1.val[1], vcoeff, vaddq_f32(x0.val[1], x2.val[1]));
                vst2q_f32(sp1, x1);
                sp0 += 8;
                sp1 += 8;
                sp2 += 8;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
            float *sp0, *sp1, *sp2;
            sp0 = buf[n - 1], sp1 = buf[n], sp2 = buf[n + 1];
            vcoeff = vdupq_n_f32(-F_LFTG_BETA);
            for (int32_t col = stride; col > 0; col -= 8) {
                x0 = vld2q_f32(sp0);
                x1 = vld2q_f32(sp1);
                x2 = vld2q_f32(sp2);
                x1.val[0] = vfmsq_f32(x1.val[0], vcoeff, vaddq_f32(x0.val[0], x2.val[0]));
                x1.val[1] = vfmsq_f32(x1.val[1], vcoeff, vaddq_f32(x0.val[1], x2.val[1]));
                vst2q_f32(sp1, x1);
                sp0 += 8;
                sp1 += 8;
                sp2 += 8;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
            float *sp0, *sp1, *sp2;
            sp0 = buf[n], sp1 = buf[n + 1], sp2 = buf[n + 2];
            vcoeff = vdupq_n_f32(-F_LFTG_ALPHA);
            for (int32_t col = stride; col > 0; col -= 8) {
                x0 = vld2q_f32(sp0);
                x1 = vld2q_f32(sp1);
                x2 = vld2q_f32(sp2);
                x1.val[0] = vfmsq_f32(x1.val[0], vcoeff, vaddq_f32(x0.val[0], x2.val[0]));
                x1.val[1] = vfmsq_f32(x1.val[1], vcoeff, vaddq_f32(x0.val[1], x2.val[1]));
                vst2q_f32(sp1, x1);
                sp0 += 8;
                sp1 += 8;
                sp2 += 8;
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
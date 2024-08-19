#include "libavcodec/jpeg2000.h"
#include "libavcodec/jpeg2000dwt.h"
#include "libavcodec/x86/jpeg2000dwt_avx2.h"
#include "libavutil/mem.h"

#include <x86intrin.h>

void idwt_2d_interleave_int_avx2(void *in, void *LL, void *HL, void *LH, void *HH,
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
        first = sp[0];
        second = sp[1];
        if (uoffset[0] > uoffset[1]) {
            first = sp[1];
            second = sp[0];
        }
        for (int32_t v = 0, vb = vstart[0]; vb < vstop[0]; ++vb, ++v) {
            int32_t *dp = buf + (2 * v + voffset[0]) * stride;
            size_t len = (size_t) (ustop[0] - ustart[0]);
            int32_t *line0 = first + v * w;
            int32_t *line1 = second + v * w;
            // SSE version
            //  for (; len >= 8; len -= 8) {
            //    auto vfirst  = _mm_loadu_si128((__m128i *)first);
            //    auto vsecond = _mm_loadu_si128((__m128i *)second);
            //    auto vtmp0   = _mm_unpacklo_epi16(vfirst, vsecond);
            //    auto vtmp1   = _mm_unpackhi_epi16(vfirst, vsecond);
            //    _mm_storeu_si128((__m128i *)dp, vtmp0);
            //    _mm_storeu_si128((__m128i *)(dp + 8), vtmp1);
            //    first += 8;
            //    second += 8;
            //    dp += 16;
            // }

            // AVX2 version
            __m256i vfirst, vsecond;
            for (; len >= 8; len -= 8) {
                vfirst = _mm256_loadu_si256((__m256i *) line0);
                vsecond = _mm256_loadu_si256((__m256i *) line1);
                __m256i vtmp0 = _mm256_unpacklo_epi32(vfirst, vsecond);
                __m256i vtmp1 = _mm256_unpackhi_epi32(vfirst, vsecond);

                _mm256_store_si256((__m256i *) dp, _mm256_permute2x128_si256(vtmp0, vtmp1, 0x20));
                _mm256_store_si256((__m256i *) dp + 1, _mm256_permute2x128_si256(vtmp0, vtmp1, 0x31));
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
        first = sp[2];
        second = sp[3];
        if (uoffset[2] > uoffset[3]) {
            first = sp[3];
            second = sp[2];
        }
        for (int32_t v = 0, vb = vstart[2]; vb < vstop[2]; ++vb, ++v) {
            int32_t *dp = buf + (2 * v + voffset[2]) * stride;
            size_t len = (size_t) (ustop[2] - ustart[2]);
            int32_t *line0 = first + v * w;
            int32_t *line1 = second + v * w;
            // SSE version
            //  for (; len >= 8; len -= 8) {
            //    auto vfirst  = _mm_loadu_si128((__m128i *)first);
            //    auto vsecond = _mm_loadu_si128((__m128i *)second);
            //    auto vtmp0   = _mm_unpacklo_epi16(vfirst, vsecond);
            //    auto vtmp1   = _mm_unpackhi_epi16(vfirst, vsecond);
            //    _mm_storeu_si128((__m128i *)dp, vtmp0);
            //    _mm_storeu_si128((__m128i *)(dp + 8), vtmp1);
            //    first += 8;
            //    second += 8;
            //    dp += 16;
            // }

            // AVX2 version
            __m256i vfirst, vsecond;
            for (; len >= 8; len -= 8) {
                vfirst = _mm256_loadu_si256((__m256i *) line0);
                vsecond = _mm256_loadu_si256((__m256i *) line1);
                __m256i vtmp0 = _mm256_unpacklo_epi32(vfirst, vsecond);
                __m256i vtmp1 = _mm256_unpackhi_epi32(vfirst, vsecond);

                _mm256_store_si256((__m256i *) dp, _mm256_permute2x128_si256(vtmp0, vtmp1, 0x20));
                _mm256_store_si256((__m256i *) dp + 1, _mm256_permute2x128_si256(vtmp0, vtmp1, 0x31));
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

void idwt_2d_interleave_float_avx2(void *in, void *LL, void *HL, void *LH, void *HH,
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
      first = sp[0];
      second = sp[1];
      if (uoffset[0] > uoffset[1]) {
        first = sp[1];
        second = sp[0];
      }
      for (int32_t v = 0, vb = vstart[0]; vb < vstop[0]; ++vb, ++v) {
        float *dp = buf + (2 * v + voffset[0]) * stride;
        size_t len = (size_t)(ustop[0] - ustart[0]);
        float *line0 = first + v * w;
        float *line1 = second + v * w;
        // SSE version
        //  for (; len >= 8; len -= 8) {
        //    auto vfirst  = _mm_loadu_si128((__m128i *)first);
        //    auto vsecond = _mm_loadu_si128((__m128i *)second);
        //    auto vtmp0   = _mm_unpacklo_epi16(vfirst, vsecond);
        //    auto vtmp1   = _mm_unpackhi_epi16(vfirst, vsecond);
        //    _mm_storeu_si128((__m128i *)dp, vtmp0);
        //    _mm_storeu_si128((__m128i *)(dp + 8), vtmp1);
        //    first += 8;
        //    second += 8;
        //    dp += 16;
        // }

        // AVX2 version
        __m256 vfirst, vsecond;
        for (; len >= 8; len -= 8) {
          vfirst = _mm256_loadu_ps((__m256 *)line0);
          vsecond = _mm256_loadu_ps((__m256 *)line1);
          __m256 vtmp0 = _mm256_unpacklo_ps(vfirst, vsecond);
          __m256 vtmp1 = _mm256_unpackhi_ps(vfirst, vsecond);

          _mm256_store_ps((__m256 *)dp, _mm256_permute2f128_ps(vtmp0, vtmp1, 0x20));
          _mm256_store_ps((__m256 *)dp + 1, _mm256_permute2f128_ps(vtmp0, vtmp1, 0x31));
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
      first = sp[2];
      second = sp[3];
      if (uoffset[2] > uoffset[3]) {
        first = sp[3];
        second = sp[2];
      }
      for (int32_t v = 0, vb = vstart[2]; vb < vstop[2]; ++vb, ++v) {
        float *dp = buf + (2 * v + voffset[2]) * stride;
        size_t len = (size_t)(ustop[2] - ustart[2]);
        float *line0 = first + v * w;
        float *line1 = second + v * w;
        // SSE version
        //  for (; len >= 8; len -= 8) {
        //    auto vfirst  = _mm_loadu_si128((__m128i *)first);
        //    auto vsecond = _mm_loadu_si128((__m128i *)second);
        //    auto vtmp0   = _mm_unpacklo_epi16(vfirst, vsecond);
        //    auto vtmp1   = _mm_unpackhi_epi16(vfirst, vsecond);
        //    _mm_storeu_si128((__m128i *)dp, vtmp0);
        //    _mm_storeu_si128((__m128i *)(dp + 8), vtmp1);
        //    first += 8;
        //    second += 8;
        //    dp += 16;
        // }

        // AVX2 version
        __m256 vfirst, vsecond;
        for (; len >= 8; len -= 8) {
          vfirst = _mm256_loadu_ps((__m256 *)line0);
          vsecond = _mm256_loadu_ps((__m256 *)line1);
          __m256 vtmp0 = _mm256_unpacklo_ps(vfirst, vsecond);
          __m256 vtmp1 = _mm256_unpackhi_ps(vfirst, vsecond);

          _mm256_store_ps((__m256 *)dp, _mm256_permute2f128_ps(vtmp0, vtmp1, 0x20));
          _mm256_store_ps((__m256 *)dp + 1, _mm256_permute2f128_ps(vtmp0, vtmp1, 0x31));
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
static inline void idwt_irrev97_fixed_avx2_hor_step0(const int32_t init_pos, const int32_t simdlen, float *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256 vcoeff = _mm256_set1_ps(F_LFTG_DELTA);
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256 xin0 = _mm256_loadu_ps((__m256 *)(X + n + n0));
    __m256 xin2 = _mm256_loadu_ps((__m256 *)(X + n + n1));
    __m256 xsum = _mm256_add_ps(xin0, xin2);
    xsum      = _mm256_blend_ps(xsum, _mm256_setzero_ps(), 0xAA);
    xsum      = (__m256)_mm256_slli_si256((__m256i)xsum, 4);
    xin0 = _mm256_fnmadd_ps(vcoeff, xsum, xin0);
    _mm256_storeu_ps((__m256 *)(X + n + n0), xin0);
  }
};

static inline void idwt_irrev97_fixed_avx2_hor_step1(const int32_t init_pos, const int32_t simdlen, float *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256 vcoeff = _mm256_set1_ps(F_LFTG_GAMMA);
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256 xin0 = _mm256_loadu_ps((__m256 *)(X + n + n0));
    __m256 xin2 = _mm256_loadu_ps((__m256 *)(X + n + n1));
    __m256 xsum = _mm256_add_ps(xin0, xin2);
    xsum      = _mm256_blend_ps(xsum, _mm256_setzero_ps(), 0xAA);
    xsum      = (__m256)_mm256_slli_si256((__m256i)xsum, 4);
    xin0 = _mm256_fnmadd_ps(vcoeff, xsum, xin0);
    _mm256_storeu_ps((__m256 *)(X + n + n0), xin0);
  }
};

static inline void idwt_irrev97_fixed_avx2_hor_step2(const int32_t init_pos, const int32_t simdlen, float *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256 vcoeff = _mm256_set1_ps(-F_LFTG_BETA);
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256 xin0 = _mm256_loadu_ps((__m256 *)(X + n + n0));
    __m256 xin2 = _mm256_loadu_ps((__m256 *)(X + n + n1));
    __m256 xsum = _mm256_add_ps(xin0, xin2);
    xsum      = _mm256_blend_ps(xsum, _mm256_setzero_ps(), 0xAA);
    xsum      = (__m256)_mm256_slli_si256((__m256i)xsum, 4);
    xin0 = _mm256_fnmadd_ps(vcoeff, xsum, xin0);
    _mm256_storeu_ps((__m256 *)(X + n + n0), xin0);
  }
};

static inline void idwt_irrev97_fixed_avx2_hor_step3(const int32_t init_pos, const int32_t simdlen, float *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256 vcoeff = _mm256_set1_ps(-F_LFTG_ALPHA);
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256 xin0 = _mm256_loadu_ps((__m256 *)(X + n + n0));
    __m256 xin2 = _mm256_loadu_ps((__m256 *)(X + n + n1));
    __m256 xsum = _mm256_add_ps(xin0, xin2);
    xsum      = _mm256_blend_ps(xsum, _mm256_setzero_ps(), 0xAA);
    xsum      = (__m256)_mm256_slli_si256((__m256i)xsum, 4);
    xin0 = _mm256_fnmadd_ps(vcoeff, xsum, xin0);
    _mm256_storeu_ps((__m256 *)(X + n + n0), xin0);
  }
};

static inline void idwt_97_int_step0(const int32_t init_pos, const int32_t simdlen, int32_t *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256i vcoeff = _mm256_set1_epi32(I_LFTG_DELTA);
  __m256i voffset = _mm256_set1_epi64x(1 << (I_LFTG_DELTA_SHIFT - 1));
  int32_t vshift = I_LFTG_DELTA_SHIFT;
  int32_t tmp[8];
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256i xin0 = _mm256_loadu_si256((__m256i *)(X + n + n0));
    _mm256_storeu_si256((__m256i *)tmp, xin0);
    __m256i xin2 = _mm256_loadu_si256((__m256i *)(X + n + n1));
    _mm256_storeu_si256((__m256i *)tmp, xin2);
    __m256i xsum = _mm256_add_epi32(xin0, xin2);
    __m256i xtmp0 = _mm256_mul_epi32(vcoeff, xsum);
    _mm256_storeu_si256((__m256i *)tmp, xtmp0);
    xtmp0 = _mm256_srli_epi64(_mm256_add_epi64(xtmp0, voffset), vshift);
    _mm256_storeu_si256((__m256i *)tmp, xtmp0);
    xtmp0 = _mm256_blend_epi32(xtmp0, _mm256_setzero_si256(), 0xAA);
    _mm256_storeu_si256((__m256i *)tmp, xtmp0);
    xtmp0 = _mm256_slli_si256(xtmp0, 4);
    _mm256_storeu_si256((__m256i *)tmp, _mm256_sub_epi32(xin0, xtmp0));
    _mm256_storeu_si256((__m256i *)(X + n + n0), _mm256_sub_epi32(xin0, xtmp0));
  }
};

static inline void idwt_97_int_step1(const int32_t init_pos, const int32_t simdlen, int32_t *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256i vcoeff = _mm256_set1_epi32(I_LFTG_GAMMA);
  __m256i voffset = _mm256_set1_epi64x(1 << (I_LFTG_GAMMA_SHIFT - 1));
  int32_t vshift = I_LFTG_GAMMA_SHIFT;
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256i xin0 = _mm256_loadu_si256((__m256i *)(X + n + n0));
    __m256i xin2 = _mm256_loadu_si256((__m256i *)(X + n + n1));
    __m256i xsum = _mm256_add_epi32(xin0, xin2);
    __m256i xtmp0 = _mm256_mul_epi32(vcoeff, xsum);
    xtmp0 = _mm256_srli_epi64(_mm256_add_epi64(xtmp0, voffset), vshift);
    xtmp0 = _mm256_blend_epi32(xtmp0, _mm256_setzero_si256(), 0xAA);
    xtmp0 = _mm256_slli_si256(xtmp0, 4);
    _mm256_storeu_si256((__m256i *)(X + n + n0), _mm256_sub_epi32(xin0, xtmp0));
  }
};

static inline void idwt_97_int_step2(const int32_t init_pos, const int32_t simdlen, int32_t *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256i vcoeff = _mm256_set1_epi32(-I_LFTG_BETA);
  __m256i voffset = _mm256_set1_epi64x(1 << (I_LFTG_BETA_SHIFT - 1));
  int32_t vshift = I_LFTG_BETA_SHIFT;
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256i xin0 = _mm256_loadu_si256((__m256i *)(X + n + n0));
    __m256i xin2 = _mm256_loadu_si256((__m256i *)(X + n + n1));
    __m256i xsum = _mm256_add_epi32(xin0, xin2);
    __m256i xtmp0 = _mm256_mul_epi32(vcoeff, xsum);
    xtmp0 = _mm256_srli_epi64(_mm256_add_epi64(xtmp0, voffset), vshift);
    xtmp0 = _mm256_blend_epi32(xtmp0, _mm256_setzero_si256(), 0xAA);
    xtmp0 = _mm256_slli_si256(xtmp0, 4);
    _mm256_storeu_si256((__m256i *)(X + n + n0), _mm256_sub_epi32(xin0, xtmp0));
  }
};

static inline void idwt_97_int_step3(const int32_t init_pos, const int32_t simdlen, int32_t *const X,
                                            const int32_t n0, const int32_t n1) {
  __m256i vcoeff = _mm256_set1_epi32(-I_LFTG_ALPHA-65536);
  __m256i voffset = _mm256_set1_epi64x(1 << (I_LFTG_ALPHA_SHIFT - 1));
  int32_t vshift = I_LFTG_ALPHA_SHIFT;
  for (int32_t n = init_pos, i = 0; i < simdlen; i += 4, n += 8) {
    __m256i xin0 = _mm256_loadu_si256((__m256i *)(X + n + n0));
    __m256i xin2 = _mm256_loadu_si256((__m256i *)(X + n + n1));
    __m256i xsum = _mm256_add_epi32(xin0, xin2);
    // xin0 = _mm256_add_epi32(xin0, xsum);
    __m256i xtmp0 = _mm256_mul_epi32(vcoeff, xsum);
    xtmp0 = _mm256_srli_epi64(_mm256_add_epi64(xtmp0, voffset), vshift);
    xtmp0 = _mm256_blend_epi32(xtmp0, _mm256_setzero_si256(), 0xAA);
    xtmp0 = _mm256_slli_si256(xtmp0, 4);
    _mm256_storeu_si256((__m256i *)(X + n + n0), _mm256_sub_epi32(xin0, xtmp0));
  }
};

void idwt_1d_filtr_97_avx2(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    float *X = (float *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    // step 1
    int32_t simdlen = stop + 2 - (start - 1);
    idwt_irrev97_fixed_avx2_hor_step0(offset - 2, simdlen, X, -1, 1);

    // step 2
    simdlen = stop + 1 - (start - 1);
    idwt_irrev97_fixed_avx2_hor_step1(offset - 2, simdlen, X, 0, 2);

    // step 3
    simdlen = stop + 1 - start;
    idwt_irrev97_fixed_avx2_hor_step2(offset, simdlen, X, -1, 1);

    // step 4
    simdlen = stop - start;
    idwt_irrev97_fixed_avx2_hor_step3(offset, simdlen, X, 0, 2);
}

void idwt_1d_filtr_97_int_avx2(void* in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    int32_t *X = (int32_t *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    // step 1
    int32_t simdlen = stop + 2 - (start - 1);
    idwt_97_int_step0(offset - 2, simdlen, X, -1, 1);

    // step 2
    simdlen = stop + 1 - (start - 1);
    idwt_97_int_step1(offset - 2, simdlen, X, 0, 2);

    // step 3
    simdlen = stop + 1 - start;
    idwt_97_int_step2(offset, simdlen, X, -1, 1);

    // step 4
    simdlen = stop - start;
    idwt_97_int_step3(offset, simdlen, X, 0, 2);
}

void idwt_1d_filtr_53_avx2(void *in, const int32_t left, const int32_t u_i0, const int32_t u_i1) {
    int32_t *X = (int32_t *)in;
    const int32_t i0        = (int32_t)(u_i0);
    const int32_t i1        = (int32_t)(u_i1);
    const int32_t start  = i0 / 2;
    const int32_t stop   = i1 / 2;
    const int32_t offset = left - i0 % 2;

    // step 1
    int32_t simdlen = stop + 1 - start;
    int32_t *sp = X + offset;
    for (; simdlen > 0; simdlen -= 4) {
        __m256i xin0 = _mm256_loadu_si256((__m256i * )(sp - 1));
        __m256i xin2 = _mm256_loadu_si256((__m256i * )(sp + 1));
        __m256i xsum = _mm256_add_epi32(xin0, xin2);
        xsum = _mm256_add_epi32(xsum, _mm256_set1_epi32(2));
        xsum = _mm256_srai_epi32(xsum, 2);
        xsum = _mm256_blend_epi32(xsum, _mm256_setzero_si256(), 0xAA);
        xsum = _mm256_slli_si256(xsum, 4);
        xin0 = _mm256_sub_epi32(xin0, xsum);
        _mm256_storeu_si256((__m256i * )(sp - 1), xin0);
        sp += 8;
    }

    // step 2
    simdlen = stop - start;
    sp = X + offset;
    for (; simdlen > 0; simdlen -= 4) {
        __m256i xin0 = _mm256_loadu_si256((__m256i *) sp);
        __m256i xin2 = _mm256_loadu_si256((__m256i * )(sp + 2));
        __m256i xsum = _mm256_add_epi32(xin0, xin2);
        xsum = _mm256_srai_epi32(xsum, 1);
        xsum = _mm256_blend_epi32(xsum, _mm256_setzero_si256(), 0xAA);
        xsum = _mm256_slli_si256(xsum, 4);
        xin0 = _mm256_add_epi32(xin0, xsum);
        _mm256_storeu_si256((__m256i *) sp, xin0);
        sp += 8;
    }
}

void idwt_ver_sr_53_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
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

        const __m256i vone = _mm256_set1_epi32(1);
        __m256i x0, x1, x2;
        for (int32_t n = 0 + offset, i = start; i < stop + 1; ++i, n += 2) {
            int32_t *xp0 = buf[n - 1];
            int32_t *xp1 = buf[n];
            int32_t *xp2 = buf[n + 1];
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256((__m256i *) xp0);
                x2 = _mm256_load_si256((__m256i *) xp2);
                x1 = _mm256_load_si256((__m256i *) xp1);
                __m256i vout = _mm256_add_epi32(vone, _mm256_srai_epi32(_mm256_add_epi32(x0, x2), 1));
                vout = _mm256_srai_epi32(vout, 1);
                x1 = _mm256_sub_epi32(x1, vout);
                _mm256_store_si256((__m256i *) xp1, x1);
                xp0 += 8;
                xp1 += 8;
                xp2 += 8;
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; ++i, n += 2) {
            int32_t *xp0 = buf[n];
            int32_t *xp1 = buf[n + 1];
            int32_t *xp2 = buf[n + 2];
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256((__m256i *) xp0);
                x2 = _mm256_load_si256((__m256i *) xp2);
                x1 = _mm256_load_si256((__m256i *) xp1);
                x1 = _mm256_add_epi32(x1, _mm256_srai_epi32(_mm256_add_epi32(x0, x2), 1));
                _mm256_store_si256((__m256i *) xp1, x1);
                xp0 += 8;
                xp1 += 8;
                xp2 += 8;
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

void idwt_ver_sr_97_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
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

        __m256 x0, x1, x2, vcoeff;
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
            vcoeff = _mm256_set1_ps(F_LFTG_DELTA);
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_ps(buf[n - 1] + col);
                x1 = _mm256_load_ps(buf[n] + col);
                x2 = _mm256_load_ps(buf[n + 1] + col);
                _mm256_store_ps((__m256 *)(buf[n] + col), _mm256_fnmadd_ps(vcoeff, _mm256_add_ps(x0, x2), x1));
            }
        }
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
            vcoeff = _mm256_set1_ps(F_LFTG_GAMMA);
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_ps(buf[n] + col);
                x1 = _mm256_load_ps(buf[n + 1] + col);
                x2 = _mm256_load_ps(buf[n + 2] + col);
                _mm256_store_ps((__m256 *)(buf[n + 1] + col), _mm256_fnmadd_ps(vcoeff, _mm256_add_ps(x0, x2), x1));
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
            vcoeff = _mm256_set1_ps(-F_LFTG_BETA);
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_ps(buf[n - 1] + col);
                x1 = _mm256_load_ps(buf[n] + col);
                x2 = _mm256_load_ps(buf[n + 1] + col);
                _mm256_store_ps((__m256 *)(buf[n] + col), _mm256_fnmadd_ps(vcoeff, _mm256_add_ps(x0, x2), x1));
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
            vcoeff = _mm256_set1_ps(-F_LFTG_ALPHA);
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_ps(buf[n] + col);
                x1 = _mm256_load_ps(buf[n + 1] + col);
                x2 = _mm256_load_ps(buf[n + 2] + col);
                _mm256_store_ps((__m256 *)(buf[n + 1] + col), _mm256_fnmadd_ps(vcoeff, _mm256_add_ps(x0, x2), x1));
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

void idwt_ver_sr_97_int_avx2(void *src, const int32_t u0, const int32_t u1, const int32_t v0,
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

        __m256i x0, x1, x2, xsum, vcoeff, voffset;
        int32_t vshift;
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 2; i++, n += 2) {
            vcoeff = _mm256_set1_epi32(I_LFTG_DELTA);
            voffset = _mm256_set1_epi64x(1 << (I_LFTG_DELTA_SHIFT - 1));
            vshift = I_LFTG_DELTA_SHIFT;
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256(buf[n - 1] + col);
                x1 = _mm256_load_si256(buf[n] + col);
                x2 = _mm256_load_si256(buf[n + 1] + col);
                xsum = _mm256_add_epi32(x0, x2);
                x0 = _mm256_mul_epi32(vcoeff, xsum);
                x2 = _mm256_mul_epi32(vcoeff, _mm256_srli_si256(xsum, 4));
                x0 = _mm256_srli_epi64(_mm256_add_epi64(x0, voffset), vshift);
                x2 = _mm256_srli_epi64(_mm256_add_epi64(x2, voffset), vshift);
                x2 = _mm256_slli_si256(x2, 4);
                x0 = _mm256_blend_epi32(x0, x2, 0xAA);
                _mm256_store_si256((__m256i *)(buf[n] + col), _mm256_sub_epi32(x1, x0));
            }
        }
        for (int32_t n = -2 + offset, i = start - 1; i < stop + 1; i++, n += 2) {
            vcoeff = _mm256_set1_epi32(I_LFTG_GAMMA);
            voffset = _mm256_set1_epi64x(1 << (I_LFTG_GAMMA_SHIFT - 1));
            vshift = I_LFTG_GAMMA_SHIFT;
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256(buf[n] + col);
                x1 = _mm256_load_si256(buf[n + 1] + col);
                x2 = _mm256_load_si256(buf[n + 2] + col);
                xsum = _mm256_add_epi32(x0, x2);
                x0 = _mm256_mul_epi32(vcoeff, xsum);
                x2 = _mm256_mul_epi32(vcoeff, _mm256_srli_si256(xsum, 4));
                x0 = _mm256_srli_epi64(_mm256_add_epi64(x0, voffset), vshift);
                x2 = _mm256_srli_epi64(_mm256_add_epi64(x2, voffset), vshift);
                x2 = _mm256_slli_si256(x2, 4);
                x0 = _mm256_blend_epi32(x0, x2, 0xAA);
                _mm256_store_si256((__m256i *)(buf[n + 1] + col), _mm256_sub_epi32(x1, x0));
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop + 1; i++, n += 2) {
            vcoeff = _mm256_set1_epi32(-I_LFTG_BETA);
            voffset = _mm256_set1_epi64x(1 << (I_LFTG_BETA_SHIFT - 1));
            vshift = I_LFTG_BETA_SHIFT;
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256(buf[n - 1] + col);
                x1 = _mm256_load_si256(buf[n] + col);
                x2 = _mm256_load_si256(buf[n + 1] + col);
                xsum = _mm256_add_epi32(x0, x2);
                x0 = _mm256_mul_epi32(vcoeff, xsum);
                x2 = _mm256_mul_epi32(vcoeff, _mm256_srli_si256(xsum, 4));
                x0 = _mm256_srli_epi64(_mm256_add_epi64(x0, voffset), vshift);
                x2 = _mm256_srli_epi64(_mm256_add_epi64(x2, voffset), vshift);
                x2 = _mm256_slli_si256(x2, 4);
                x0 = _mm256_blend_epi32(x0, x2, 0xAA);
                int32_t tmp[8];
                _mm256_storeu_si256((__m256i *)tmp, _mm256_sub_epi32(x1, x0));
                _mm256_store_si256((__m256i *)(buf[n] + col), _mm256_sub_epi32(x1, x0));
            }
        }
        for (int32_t n = 0 + offset, i = start; i < stop; i++, n += 2) {
            vcoeff = _mm256_set1_epi32(-I_LFTG_ALPHA);
            voffset = _mm256_set1_epi64x(1 << (I_LFTG_ALPHA_SHIFT - 1));
            vshift = I_LFTG_ALPHA_SHIFT;
            for (int32_t col = 0; col < stride; col += 8) {
                x0 = _mm256_load_si256(buf[n] + col);
                x1 = _mm256_load_si256(buf[n + 1] + col);
                x2 = _mm256_load_si256(buf[n + 2] + col);
                xsum = _mm256_add_epi32(x0, x2);
                x1 = _mm256_add_epi32(x1, xsum);
                x0 = _mm256_mul_epi32(vcoeff, xsum);
                x2 = _mm256_mul_epi32(vcoeff, _mm256_srli_si256(xsum, 4));
                x0 = _mm256_srli_epi64(_mm256_add_epi64(x0, voffset), vshift);
                x2 = _mm256_srli_epi64(_mm256_add_epi64(x2, voffset), vshift);
                x2 = _mm256_slli_si256(x2, 4);
                x0 = _mm256_blend_epi32(x0, x2, 0xAA);
                _mm256_store_si256((__m256i *)(buf[n + 1] + col), _mm256_sub_epi32(x1, x0));
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
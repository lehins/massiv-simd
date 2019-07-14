#include <immintrin.h>
#include <stdio.h>
#include <stdbool.h>


// Missing intrinsics in gcc (https://gcc.gnu.org/ml/gcc-patches/2017-05/msg00611.html):
// _mm256_set_m128
// _mm256_set_m128d
// _mm256_set_m128i
// _mm256_setr_m128
// _mm256_setr_m128d
// _mm256_setr_m128i

// Copy the upper double-precision (64-bit) floating-point element of a to dst.
inline double massiv__mm_cvtsd_f64u(const __m128d val){
  return _mm_cvtsd_f64(_mm_castsi128_pd(_mm_srli_si128(_mm_castpd_si128(val), 8)));
}


// __m256d _mm256_insertf128_pd (__m256d a, __m128d b, int imm8) could work as well
/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot__m256d(const double vec1[], const double vec2[], const long len) {
  __m128d hi;
  __m128d lo;
  long i = len % 4;
  /* double e3 = i == 3 ? vec1[3] * vec2[3] : 0; */
  /* double e2 = i >= 2 ? vec1[2] * vec2[2] : 0; */
  /* double e1 = i >= 1 ? vec1[1] * vec2[1] : 0; */
  /* __m256d acc = _mm256_set_pd(e3, e2, e1, 0); */
  __m256d acc = _mm256_setzero_pd();
  if (i == 1)
    acc =  _mm256_insertf128_pd(acc, _mm_set_sd(vec1[0] * vec2[0]), 1);
  else if (i == 3)
    acc =  _mm256_insertf128_pd(acc, _mm_set_sd(vec1[2] * vec2[2]), 1);
  if (i >= 2) {
    __m128d vi1 = _mm_loadu_pd(vec1);
    __m128d vi2 = _mm_loadu_pd(vec2);
    acc = _mm256_insertf128_pd(acc, _mm_mul_pd(vi1, vi2), 0);
  }

  for (; i < len; i += 4) {
    __m256d vi1 = _mm256_loadu_pd(&vec1[i]);
    __m256d vi2 = _mm256_loadu_pd(&vec2[i]);
    acc = _mm256_add_pd(acc, _mm256_mul_pd(vi1, vi2));
  }
  hi = _mm256_extractf128_pd(acc, 1);
  lo = _mm_add_pd(hi, _mm256_extractf128_pd(acc, 0));
  return _mm_cvtsd_f64(lo) + massiv__mm_cvtsd_f64u(lo);
}




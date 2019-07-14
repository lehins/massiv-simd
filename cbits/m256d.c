#include <immintrin.h>
#include <stdio.h>
#include <stdbool.h>

#include "m128d.h"

#define massiv__mm256_set_m128d(hi, lo) \
  _mm256_insertf128_pd(_mm256_castpd128_pd256(lo), (hi), 1)

// Missing intrinsics in gcc (https://gcc.gnu.org/ml/gcc-patches/2017-05/msg00611.html):
// _mm256_set_m128
// _mm256_set_m128d
// _mm256_set_m128i
// _mm256_setr_m128
// _mm256_setr_m128d
// _mm256_setr_m128i


/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot__m256d(const double vec1[], const double vec2[], const long len) {
  __m128d hi;
  __m128d lo;
  __m256d acc;
  long i = len % 4;
  if (i == 1)
    lo = _mm_set_sd(vec1[0] * vec2[0]);
  else
    lo = i >= 2 ? massiv__mm_loadu_mul_pd(vec1, vec2) : _mm_setzero_pd();
  hi = i == 3 ? _mm_set_sd(vec1[2] * vec2[2]) : _mm_setzero_pd();
  acc = massiv__mm256_set_m128d(hi, lo);

  for (; i < len; i += 4) {
    __m256d vi1 = _mm256_loadu_pd(&vec1[i]);
    __m256d vi2 = _mm256_loadu_pd(&vec2[i]);
    acc = _mm256_add_pd(acc, _mm256_mul_pd(vi1, vi2));
  }
  hi = _mm256_extractf128_pd(acc, 1);
  lo = _mm_add_pd(hi, _mm256_extractf128_pd(acc, 0));
  return _mm_cvtsd_f64(lo) + massiv__mm_cvtsd_f64u(lo);
}

/* double massiv_dot__m256d(const double vec1[], const double vec2[], const long len) { */
/*   __m128d hi; */
/*   __m128d lo; */
/*   double acc; */
/*   __m256d acc256d = _mm256_setzero_pd(); */
/*   long slack_start = len - len % 4; */

/*   for (long i = 0; i < slack_start; i += 4) { */
/*     __m256d vi1 = _mm256_loadu_pd(&vec1[i]); */
/*     __m256d vi2 = _mm256_loadu_pd(&vec2[i]); */
/*     acc256d = _mm256_add_pd(acc256d, _mm256_mul_pd(vi1, vi2)); */
/*   } */
/*   hi = _mm256_extractf128_pd(acc256d, 1); */
/*   lo = _mm_add_pd(hi, _mm256_extractf128_pd(acc256d, 0)); */
/*   acc = _mm_cvtsd_f64(lo) + massiv__mm_cvtsd_f64u(lo); */
/*   for(int i = slack_start; i < len; i++){ */
/*     acc = acc + vec1[i] * vec2[i]; */
/*   } */
/*   return acc; */
/* } */

/* double massiv_dot__m256d_aligned(const double vec1[], const double vec2[], const long len) { */
/*   __m128d hi; */
/*   __m128d lo; */
/*   __m256d acc256d = _mm256_setzero_pd(); */
/*   for (long i = 0; i < len; i += 4) { */
/*     __m256d vi1 = _mm256_load_pd(&vec1[i]); */
/*     __m256d vi2 = _mm256_load_pd(&vec2[i]); */
/*     acc256d = _mm256_add_pd(acc256d, _mm256_mul_pd(vi1, vi2)); */
/*   } */
/*   hi = _mm256_extractf128_pd(acc256d, 1); */
/*   lo = _mm_add_pd(hi, _mm256_extractf128_pd(acc256d, 0)); */
/*   return _mm_cvtsd_f64(lo) + massiv__mm_cvtsd_f64u(lo); */
/* } */



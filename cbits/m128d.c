#include <emmintrin.h>
#include <stdio.h>

/**
 * Compute the dot product of two vectors with doubles. Both vectors are expected to have
 * at least `len` number of elements, but it is not checked.
 */
double dot__m128d(const double vec1[], const double vec2[], const long len) {
  __m128d acc;
  long i = len % 2;
  if (i == 0)
    acc = _mm_setzero_pd();
  else
    acc = _mm_set_sd(vec1[0] * vec2[0]);

  for (; i < len; i += 2) {
    __m128d v1 = _mm_loadu_pd(&vec1[i]);
    __m128d v2 = _mm_loadu_pd(&vec2[i]);
    acc = _mm_add_pd(acc, _mm_mul_pd(v1, v2));
  }
  return _mm_cvtsd_f64(acc + _mm_cvtsd_f64((__m128d)_mm_srli_si128((__m128i)acc, 8)));
}


double sum_d(const double vec[], const int vecLength) {
  int rest = vecLength % 2;

  // two partial sums
  double init = 0;
  __m128d vsum = _mm_set1_pd(init);

  double sum;
  int i;

  for (i = 0; i < vecLength - rest; i += 2) {
    __m128d v = _mm_load_pd(&vec[i]);
    vsum = _mm_add_pd(vsum, v);
  }

  vsum = _mm_add_pd(vsum, (__m128d)_mm_srli_si128((__m128i)vsum, 8));
  sum = _mm_cvtsd_f64(vsum);

  for (; i < vecLength; i++) {
    sum += vec[i];
  }

  return sum;
}

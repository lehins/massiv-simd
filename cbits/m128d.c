#include <emmintrin.h>
#include <stdio.h>


double dot__m128d(const double vec1[], const double vec2[], const long long len) {
  __m128d acc = _mm_set1_pd(0);

  for (long long i = 0; i < len; i += 2) {
    __m128d v1 = _mm_load_pd(&vec1[i]);
    __m128d v2 = _mm_load_pd(&vec2[i]);
    acc = _mm_add_pd(acc, _mm_mul_pd(v1, v2));
  }
  return _mm_cvtsd_f64(acc) + _mm_cvtsd_f64((__m128d)_mm_srli_si128((__m128i)acc, 8));
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

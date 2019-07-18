#include <emmintrin.h>
#include <stdio.h>
#include <stdbool.h>

// Grab the upper double of the __m128d
#define massiv__mm_cvtsd_f64u(v) \
  _mm_cvtsd_f64(_mm_castsi128_pd(_mm_srli_si128(_mm_castpd_si128(v), 8)))

// Load two
#define massiv__mm_loadu_mul_pd(v1, v2) \
  _mm_mul_pd(_mm_loadu_pd(v1), _mm_loadu_pd(v2))

double max_double(double num1, double num2);
double min_double(double num1, double num2);

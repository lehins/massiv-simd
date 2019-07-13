#include <emmintrin.h>
#include <stdio.h>
#include <stdbool.h>

void free_flagged(bool *not_freed, void *ptr){
  if(!*not_freed) free(ptr);
  free(not_freed);
}

inline double max_double(double num1, double num2){
  return (num1 > num2 ) ? num1 : num2;
}

inline double min_double(double num1, double num2){
  return (num1 > num2 ) ? num2 : num1;
}

// Copy the upper double-precision (64-bit) floating-point element of a to dst.
inline double massiv__mm_cvtsd_f64u(const __m128d val){
  return _mm_cvtsd_f64(_mm_castsi128_pd(_mm_srli_si128(_mm_castpd_si128(val), 8)));
}

/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot__m128d(const double vec1[], const double vec2[], const long len) {
  __m128d acc;
  long i = len % 2;
  if (i == 0)
    acc = _mm_setzero_pd();
  else
    acc = _mm_set_sd(vec1[0] * vec2[0]);

  for (; i < len; i += 2) {
    __m128d vi1 = _mm_loadu_pd(&vec1[i]);
    __m128d vi2 = _mm_loadu_pd(&vec2[i]);
    acc = _mm_add_pd(acc, _mm_mul_pd(vi1, vi2));
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}

/**
 * Set all elements of the vector to the same value
 */
void massiv_broadcast__m128d(double vec[], const long len, const double val) {
  long i = len % 2;
  if (i == 1)
    vec[0] = val;

  for (; i < len; i += 2) {
    _mm_storeu_pd(&vec[i], _mm_set1_pd(val));
  }
}

/**
 * Copy elements from one vector into another
 */
void massiv_copy__m128d(const double vec[], double res[], const long len) {
  long i = len % 2;
  if (i == 1)
    res[0] = vec[0];

  for (; i < len; i += 2) {
    __m128d vi = _mm_loadu_pd(&vec[i]);
    _mm_storeu_pd(&res[i], vi);
  }
}

/**
 * Compare two vectors with doubles. Does not short circuit on first inequality (could be
 * useful for cryptography?)
 */
bool massiv_eq__m128d(const double vec1[], const double vec2[], const long len) {
  __m128i acc;
  long i = len % 2;
  if (len == 1)
    return vec1[0] == vec2[0];
  acc = _mm_set1_epi8(0xff);
  for (; i < len; i += 2) {
    __m128d vi1 = _mm_loadu_pd(&vec1[i]);
    __m128d vi2 = _mm_loadu_pd(&vec2[i]);
    acc = _mm_and_si128(acc, _mm_castpd_si128(_mm_cmpeq_pd(vi1, vi2)));
  }
  return (long long)_mm_movepi64_pi64(acc) == 0xffffffffffffffff &&
         (long long)_mm_movepi64_pi64(_mm_srli_si128(acc, 8)) == 0xffffffffffffffff;
}

/**
 * Add two vectors of doubles element by element and store results in the resulting
 * vector.
 */
void massiv_plus__m128d(const double vec1[], const double vec2[], double res[], const long len) {
  long i = len % 2;
  if (i == 1)
    res[0] = vec1[0] + vec2[0];

  for (; i < len; i += 2) {
    __m128d vi1 = _mm_loadu_pd(&vec1[i]);
    __m128d vi2 = _mm_loadu_pd(&vec2[i]);
    _mm_storeu_pd(&res[i], _mm_add_pd(vi1, vi2));
  }
}

/**
 * Compute the sum over a vector of doubles.
 */
double massiv_sum__m128d(const double vec[], const long len) {
  __m128d acc;
  long i = len % 2;
  if (i == 0)
    acc = _mm_setzero_pd();
  else
    acc = _mm_set_sd(vec[0]);

  for (; i < len; i += 2) {
    __m128d vi = _mm_loadu_pd(&vec[i]);
    acc = _mm_add_pd(acc, vi);
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}


/**
 * Compute the product over a vector of doubles.
 */
double massiv_product__m128d(const double vec[], const long len) {
  __m128d acc;
  long i = len % 2;
  if (i == 0)
    acc = _mm_set1_pd(1);
  else
    acc = _mm_set_pd(vec[0], 1);

  for (; i < len; i += 2) {
    __m128d vi = _mm_loadu_pd(&vec[i]);
    acc = _mm_mul_pd(acc, vi);
  }
  return _mm_cvtsd_f64(acc) * massiv__mm_cvtsd_f64u(acc);
}



/**
 * Find the maximum number in the vector of doubles.
 */
double massiv_maximum__m128d(const double vec[], const long len) {
  __m128d cur;
  long rem;
  double result;
  if (len == 1)
    return vec[0];

  cur = _mm_loadu_pd(&vec[0]);
  rem = len % 2;

  for (long i = 2; i < len - rem; i += 2) {
    __m128d vi = _mm_loadu_pd(&vec[i]);
    cur = _mm_max_pd(cur, vi);
  }

  result = max_double(_mm_cvtsd_f64(cur), massiv__mm_cvtsd_f64u(cur));
  if (rem == 1)
    return max_double(result, vec[len - 1]);
  return result;
}

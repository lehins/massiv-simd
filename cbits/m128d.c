#include "m128d.h"
#include <smmintrin.h>

inline double max_double(double num1, double num2){
  return (num1 > num2 ) ? num1 : num2;
}

inline double min_double(double num1, double num2){
  return (num1 > num2 ) ? num2 : num1;
}


/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot_product__m128d_a(const double init, const double v1[], const double v2[], const long len) {
  __m128d acc = _mm_set_sd(init);
  for (long i = 0; i < len; i += 2) {
    __m128d vi1 = _mm_load_pd(&v1[i]);
    __m128d vi2 = _mm_loadu_pd(&v2[i]);
    acc = _mm_add_pd(acc, _mm_mul_pd(vi1, vi2));
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}

/**
 * Set all elements of the vector to the same value
 */
void massiv_fill__m128d_a(const double val, double vec[], const long len) {
  __m128d val128 = _mm_set1_pd(val);
  for (long i = 0; i < len; i += 2) {
    _mm_store_pd(&vec[i], val128);
  }
}

/**
 * Copy elements from one vector into another
 */
void massiv_copy__m128d_a(const double vec[], double res[], const long len) {
  for (long i = 0; i < len; i += 2) {
    __m128d vi = _mm_load_pd(&vec[i]);
    _mm_storeu_pd(&res[i], vi);
  }
}

/**
 * Compare two vectors with doubles. Does not short circuit on first inequality (could be
 * useful for cryptography?)
 */
bool massiv_eq__m128d_a(const double vec1[], const double vec2[], const long len) {
  __m128i acc = _mm_set1_epi8(0xff);
  for (long i = 0; i < len; i += 2) {
    __m128d vi1 = _mm_load_pd(&vec1[i]);
    __m128d vi2 = _mm_loadu_pd(&vec2[i]);
    acc = _mm_and_si128(acc, _mm_castpd_si128(_mm_cmpeq_pd(vi1, vi2)));
  }
  return (long long)_mm_movepi64_pi64(acc) == 0xffffffffffffffff &&
         (long long)_mm_movepi64_pi64(_mm_srli_si128(acc, 8)) == 0xffffffffffffffff;
}

/**
 * Add two vectors of doubles pointwise and store results in the supplied vector.
 */
void massiv_addition__m128d_a(const double vec1[], const double vec2[], double res[], const long len) {
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_add_pd(_mm_load_pd(&vec1[i]), _mm_loadu_pd(&vec2[i])));
  }
}

/**
 * Subtract two vectors of doubles pointwise and store results in the supplied vector.
 */
void massiv_subtraction__m128d_a(const double vec1[], const double vec2[], double res[], const long len) {
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_sub_pd(_mm_load_pd(&vec1[i]), _mm_loadu_pd(&vec2[i])));
  }
}

/**
 * Multiply two vectors of doubles pointwise and store results in the supplied vector.
 */
void massiv_multiplication__m128d_a(const double vec1[], const double vec2[], double res[], const long len) {
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_mul_pd(_mm_load_pd(&vec1[i]), _mm_loadu_pd(&vec2[i])));
  }
}

/**
 * Divide two vectors of doubles pointwise and store results in the supplied vector.
 */
void massiv_division__m128d_a(const double vec1[], const double vec2[], double res[], const long len) {
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_div_pd(_mm_load_pd(&vec1[i]), _mm_loadu_pd(&vec2[i])));
  }
}


/**
 * Compute reciprocal of each element and multiply it by a scalar in a vector of doubles
 * and store results in the supplied vector.
 */
void massiv_divide__m128d_a(const double vec[], const double den, double res[], const long len){
  __m128d den128 = _mm_set_pd1(den);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_div_pd(_mm_load_pd(&vec[i]), den128));
  }
}

/**
 * Compute reciprocal of each element and multiply it by a scalar in a vector of doubles
 * and store results in the supplied vector.
 */
void massiv_recip_multiply__m128d_a(const double vec[], const double num, double res[], const long len){
  __m128d num128 = _mm_set_pd1(num);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_div_pd(num128, _mm_load_pd(&vec[i])));
  }
}

/**
 * Compute square root of each element in a vector of doubles and store results in the
 * supplied vector.
 */
void massiv_sqrt__m128d_a(const double vec[], double res[], const long len){
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_sqrt_pd(_mm_load_pd(&vec[i])));
  }
}


/**
 * Round each element in a vector of doubles and store results in the supplied vector of doubles.
 *
 * Requires: SSE4.1 due to _mm_round_pd()
 */
void massiv_round__m128d_a(const double vec[], double res[], const long len){
  __m128d x128;
  for (long i = 0; i < len; i += 2) {
    x128 = _mm_round_pd(_mm_load_pd(&vec[i]), _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    _mm_storeu_pd(&res[i], x128);
  }
}


/**
 * Round each element in a vector of doubles and store results in the supplied vector of
 * 64bit integers.
 *
 * Requires: SSE4.1 due to _mm_round_pd()
 */
void massiv_round_64i__m128d_a(const double vec[], long long res[], const long len){
  __m128d x128;
  for (long i = 0; i < len; i += 2) {
    x128 = _mm_round_pd(_mm_load_pd(&vec[i]), _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
    res[i] = (long long) _mm_cvtsd_f64(x128);
    res[i + 1] = (long long) massiv__mm_cvtsd_f64u(x128);
  }
}


/**
 * Truncate each element in a vector of doubles and store results in the supplied vector of doubles.
 *
 * Requires: SSE4.1 due to _mm_round_pd()
 */
void massiv_truncate__m128d_a(const double vec[], double res[], const long len){
  __m128d x128;
  for (long i = 0; i < len; i += 2) {
    x128 = _mm_round_pd(_mm_load_pd(&vec[i]), _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
    _mm_storeu_pd(&res[i], x128);
  }
}

/**
 * Truncate each element in a vector of doubles and store results in the supplied vector.
 */
void massiv_truncate_64i__m128d_a(const double vec[], long long res[], const long len){
  __m128d x128;
  for (long i = 0; i < len; i += 2) {
    x128 = _mm_load_pd(&vec[i]);
    res[i] = (long long) _mm_cvtsd_f64(x128);
    res[i + 1] = (long long) massiv__mm_cvtsd_f64u(x128);
  }
}

/**
 * Add a scalar to a vector of doubles and store results in the supplied vector.
 */
void massiv_plus__m128d_a(const double vec[], const double x, double res[], const long len) {
  __m128d x128 = _mm_set_pd1(x);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_add_pd(_mm_load_pd(&vec[i]), x128));
  }
}

/**
 * Subtract a scalar from a vector of doubles and store results in the supplied vector.
 */
void massiv_minus__m128d_a(const double vec[], const double x, double res[], const long len) {
  __m128d x128 = _mm_set_pd1(x);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_sub_pd(_mm_load_pd(&vec[i]), x128));
  }
}

/**
 * Negate each element and add a scalar to it in the vector of doubles, store the results in
 * the supplied vector.
 */
void massiv_negate_plus__m128d_a(const double vec[], const double x, double res[], const long len) {
  __m128d x128 = _mm_set_pd1(x);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_sub_pd(x128, _mm_load_pd(&vec[i])));
  }
}


/**
 * Multiply with a scalar each element in a vector of doubles and store results in the
 * supplied vector.
 */
void massiv_multiply__m128d_a(const double vec[], const double x, double res[], const long len) {
  __m128d x128 = _mm_set_pd1(x);
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_mul_pd(_mm_load_pd(&vec[i]), x128));
  }
}

/**
 * Raise each element of the vector to some positive power.
 */
void massiv_power__m128d_a(const double vec[], const long pow, double res[], const long len) {
  __m128d vi, vi1;
  for (long i = 0; i < len; i += 2) {
    vi1 = _mm_load_pd(&vec[i]);
    vi = vi1;
    for(long p = 1; p < pow; p++)
      vi = _mm_mul_pd(vi, vi1);
    _mm_storeu_pd(&res[i], vi);
  }
}


/**
 * Raise a reciprocal of each element of the vector to some positive power.
 */
void massiv_recip_power__m128d_a(const double vec[], const long pow, double res[], const long len) {
  __m128d vi, vi1, num1 = _mm_set_pd1(1);
  for (long i = 0; i < len; i += 2) {
    vi1 = _mm_div_pd(num1, _mm_load_pd(&vec[i]));
    vi = vi1;
    for(long p = 1; p < pow; p++)
      vi = _mm_mul_pd(vi, vi1);
    _mm_storeu_pd(&res[i], vi);
  }
}


/**
 * Compute absolute value of each element in a vector of doubles and store results in the
 * supplied vector.
 */
void massiv_abs__m128d_a(const double vec[], double res[], const long len) {
  const __m128d mask = _mm_castsi128_pd (_mm_set1_epi64x (0x7FFFFFFFFFFFFFFF));
  for (long i = 0; i < len; i += 2) {
    _mm_storeu_pd(&res[i], _mm_and_pd(mask, _mm_load_pd(&vec[i])));
  }
}

/**
 * Compute the sum over a vector of doubles.
 */
double massiv_sum__m128d_a(const double init, const double vec[], const long len) {
  __m128d acc = _mm_set_sd(init);
  for (long i = 0; i < len; i += 2) {
    acc = _mm_add_pd(acc, _mm_load_pd(&vec[i]));
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}

/**
 * Compute the product over a vector of doubles.
 */
double massiv_product__m128d_a(const double init, const double vec[], const long len) {
  __m128d acc = _mm_set_pd(init, 1);
  for (long i = 0; i < len; i += 2) {
    acc = _mm_mul_pd(acc, _mm_load_pd(&vec[i]));
  }
  return _mm_cvtsd_f64(acc) * massiv__mm_cvtsd_f64u(acc);
}

/**
 * Raise each element to the positive even power and sum all results.
 */
double massiv_even_power_sum__m128d_a(const long pow, const double init, const double vec[], const long len) {
  __m128d acc = _mm_set_sd(init);
  for (long i = 0; i < len; i += 2) {
    __m128d vi = _mm_load_pd(&vec[i]);
    __m128d vi2 = _mm_mul_pd(vi, vi);
    __m128d viacc = vi2;
    for(long p = 2; p < pow; p+= 2)
      viacc = _mm_mul_pd(viacc, vi2);
    acc = _mm_add_pd(acc, viacc);
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}

/**
 * Compute the sum of absolute values raised to the power, where n is a positive integer.
 */
double massiv_abs_power_sum__m128d_a(const long n, const double init, const double vec[], const long len) {
  __m128d acc = _mm_set_sd(init);
  const __m128d mask = _mm_castsi128_pd (_mm_set1_epi64x (0x7FFFFFFFFFFFFFFF));
  for (long i = 0; i < len; i += 2) {
    __m128d vi = _mm_and_pd(mask, _mm_load_pd(&vec[i]));
    __m128d viacc = vi;
    for(long p = 1; p < n; p++)
      viacc = _mm_mul_pd(viacc, vi);
    acc = _mm_add_pd(acc, viacc);
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
}

/**
 * Compute the maximum of absolute values in the vector.
 */
double massiv_abs_max__m128d_a(const double init, const double vec[], const long len) {
  __m128d acc = _mm_set_sd(init);
  const __m128d mask = _mm_castsi128_pd (_mm_set1_epi64x (0x7FFFFFFFFFFFFFFF));
  for (long i = 0; i < len; i += 2) {
    __m128d vi = _mm_and_pd(mask, _mm_load_pd(&vec[i]));
    acc = _mm_max_pd(acc, vi);
  }
  return max_double(_mm_cvtsd_f64(acc), massiv__mm_cvtsd_f64u(acc));
}



/**
 * Find the maximum number in the vector of doubles.
 */
double massiv_maximum__m128d_a(const double cur, const double vec[], const long len) {
  __m128d cur128 = _mm_set1_pd(cur);
  for (long i = 0; i < len; i += 2) {
    cur128 = _mm_max_pd(cur128, _mm_load_pd(&vec[i]));
  }
  return max_double(_mm_cvtsd_f64(cur128), massiv__mm_cvtsd_f64u(cur128));
}

/**
 * Find the minimum number in the vector of doubles.
 */
double massiv_minimum__m128d_a(const double cur, const double vec[], const long len) {
  __m128d cur128 = _mm_set1_pd(cur);
  for (long i = 0; i < len; i += 2) {
    cur128 = _mm_min_pd(cur128, _mm_load_pd(&vec[i]));
  }
  return min_double(_mm_cvtsd_f64(cur128), massiv__mm_cvtsd_f64u(cur128));
}

//bool massiv_elem__m128d(const double vec[], const double, const long len) {


//void massiv_and__m128i(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_or__m128i(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_nand__m128i(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_xor__m128i(const double vec1[], const double vec2[], double res[], const long len);


//double massiv_add__m128d(const double vec[], const double x, const long len);
//double massiv_sub__m128d(const double vec[], const double x, const long len);
//double massiv_mul__m128d(const double vec[], const double x, const long len);
//double massiv_div__m128d(const double vec[], const double x, const long len);
//double massiv_pow__m128d(const double vec[], const unsigned int x, const long len);

//double massiv_ceil__m128d(const double vec[], const double x, const long len);
//double massiv_floor__m128d(const double vec[], const double x, const long len);


// less important

//bool massiv_lt__m128d(const double vec1[], const double vec2[], const long len);
//bool massiv_le__m128d(const double vec1[], const double vec2[], const long len);
//bool massiv_gt__m128d(const double vec1[], const double vec2[], const long len);
//bool massiv_ge__m128d(const double vec1[], const double vec2[], const long len);




/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot_product__m128d(const double vec1[], const double vec2[], const long len) {
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
 * Add two vectors of doubles element by element and store results in the resulting
 * vector.
 */
void massiv_addition__m128d(const double vec1[], const double vec2[], double res[], const long len) {
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
 * Set all elements of the vector to the same value
 */
void massiv_set__m128d(double vec[], const long len, const double val) {
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

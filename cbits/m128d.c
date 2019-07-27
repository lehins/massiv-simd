#include "m128d.h"

inline double max_double(double num1, double num2){
  return (num1 > num2 ) ? num1 : num2;
}

inline double min_double(double num1, double num2){
  return (num1 > num2 ) ? num2 : num1;
}


/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot_product__m128d_a(const double init, const double *v1, const double *v2, const long len) {
  __m128d acc = _mm_set_sd(init);
  for (int i = 0; i < len; i += 2) {
    __m128d vi1 = _mm_load_pd(v1 + i);
    __m128d vi2 = _mm_loadu_pd(v2 + i);
    acc = _mm_add_pd(acc, _mm_mul_pd(vi1, vi2));
  }
  return _mm_cvtsd_f64(acc) + massiv__mm_cvtsd_f64u(acc);
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
 * Absolute value of each element in a vector of doubles and store results in the
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
double massiv_sum__m128d_a(const double init, const double *v, const long len) {
  __m128d acc = _mm_set_sd(init);
  for (int i = 0; i < len; i += 2) {
    __m128d vi = _mm_loadu_pd(&v[i]);
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

//double massiv_minimum__m128d(const double vec[], const long len) {
//bool massiv_elem__m128d(const double vec[], const double, const long len) {

//double massiv_sqrt__m128d(const double vec[], const long len);

//void massiv_minus__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_times__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_divide__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_and__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_or__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_nand__m128d(const double vec1[], const double vec2[], double res[], const long len);
//void massiv_xor__m128d(const double vec1[], const double vec2[], double res[], const long len);


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
double massiv_dot_product_m128d(const double vec1[], const double vec2[], const long len) {
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

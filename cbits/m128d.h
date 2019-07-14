#include <emmintrin.h>
#include <stdio.h>
#include <stdbool.h>


#define massiv__mm_cvtsd_f64u(v) \
        _mm_cvtsd_f64(_mm_castsi128_pd(_mm_srli_si128(_mm_castpd_si128(v), 8)))


double max_double(double num1, double num2);
double min_double(double num1, double num2);

/**
 * Copy the upper double-precision (64-bit) floating-point element of a to dst.
 */

/**
 * Compute the dot product of two vectors with doubles.
 */
double massiv_dot__m128d(const double vec1[], const double vec2[], const long len);

/**
 * Set all elements of the vector to the same value
 */
void massiv_broadcast__m128d(double vec[], const long len, const double val);

/**
 * Copy elements from one vector into another
 */
void massiv_copy__m128d(const double vec[], double res[], const long len);

/**
 * Compare two vectors with doubles. Does not short circuit on first inequality (could be
 * useful for cryptography?)
 */
bool massiv_eq__m128d(const double vec1[], const double vec2[], const long len);

/**
 * Add two vectors of doubles element by element and store results in the resulting
 * vector.
 */
void massiv_plus__m128d(const double vec1[], const double vec2[], double res[], const long len);

/**
 * Compute the sum over a vector of doubles.
 */
double massiv_sum__m128d(const double vec[], const long len);


/**
 * Compute the product over a vector of doubles.
 */
double massiv_product__m128d(const double vec[], const long len);


/**
 * Find the maximum number in the vector of doubles.
 */
double massiv_maximum__m128d(const double vec[], const long len);

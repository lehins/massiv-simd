#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

void massiv_free_flagged(bool *freed, void *ptr){
  if(!*freed) free(ptr);
  free(freed);
}

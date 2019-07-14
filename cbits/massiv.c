#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

void free_flagged(bool *freed, void *ptr){
  if(!*freed) free(ptr);
  free(freed);
}

#include <stdlib.h>
#include <stdbool.h>

void free_flagged(bool *freed, void *ptr){
  if(!*freed) free(ptr);
  free(freed);
}



void free_null(void *ptr){
  if(ptr) free(ptr);
}

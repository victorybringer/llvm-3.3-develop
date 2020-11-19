#include <stdlib.h>

void test(int n) {
  int *p = malloc(sizeof(int));
  free(p);
  free(p);
  int *q = malloc(4);
  free(q);
  if(n > 0) {
    free(q);
  }
  test2(q);
}

void test2(int *q) {
  free(q);
}


#include <stdlib.h>

struct A {
  int b;
} a;

void test(int n) {
  int *p = NULL;
  int q = *p;
  if(n > 0) {
    p = (int *) malloc(4);
  }
  q = *p;
  test2(p);
  struct A *b = NULL;
  b->b;
}

void test2(int *p) {
  int q = *p;
}

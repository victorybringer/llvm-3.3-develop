#include <string.h>
#include <stdlib.h>

void test() {
  char *p1 = (char *) malloc(20); 
  char *p2 = (char *) malloc(20);
  char *p3 = (char *) malloc(20);
  char *q = p2; 
  p1 = (char *) malloc(20); 
  q = p2 + 0; 
  strcpy(p3, "hello"); 
}

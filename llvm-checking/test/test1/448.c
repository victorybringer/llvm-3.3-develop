#include <string.h>

void test(int m) {
  char buf[20];
  strcpy( buf, "a" );
  char c = buf[4]; 
  int n = 5;
  char d = buf[n];
  char e = buf[n - 5];
  test2(buf);
  char f = buf[m];
}

void test2(char buf[]) {
  char c = buf[4];
}

void test3() {
  int m = 5;
  test(m);
}

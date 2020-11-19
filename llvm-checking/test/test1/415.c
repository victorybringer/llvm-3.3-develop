void test(int n) {
  int a[10];
  a[10] = 10;
  a[n] = 10;
  int m = n - (n - 10);
  a[m] = 10;
  if(n < 10) {
    a[n] = 10;
  }
  if(n > 10) {
    a[n] = 10;
  }
}

void test2() {
  test(11);

}

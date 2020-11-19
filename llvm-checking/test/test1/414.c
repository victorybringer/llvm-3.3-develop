#define Y 1
int g(int x) {
 x = x - 5;
 int y = 100 / x;
 int z = 100 % x;
 if(y > 0) {
   int z = 100 / y;
 }
}

int f() {
  int x = 10 / 0;
  int y = 1 - Y;
  int z = x / y;
  g(5);
}

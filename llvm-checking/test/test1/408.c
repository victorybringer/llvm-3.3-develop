enum DAY {
  MON=1, TES, THU
};
enum DAY day2;
int main() {
  enum DAY day;
  switch(day) {
    case 1:
      break;
    case TES:
      break;
    default:
     return 0;
  }
  switch(day2) {
    case 1:
      break;
  }
  int a;
  switch(a) {
    case MON:
      break;
  }
  switch(a) {
    case 1L:
      break;
  }
}

void f(enum DAY day3) {
  switch(day3) {
    case 'c':
       break;
  }
}

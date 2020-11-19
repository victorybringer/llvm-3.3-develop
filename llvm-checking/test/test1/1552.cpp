class Base {
  int a;
};

class Derived : public Base {
  int b;
};

void test() {
  Derived *d = new Derived[10];
  Base *b;
  b = d;  
  b = &d[0]; 
}


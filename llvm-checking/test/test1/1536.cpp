class X {
private:
  int a;
public:
  int b;
public:
  int *f() { return &a; }
  int &g() { return a;}
};

class Y : private X {
  int *g() {return &b;}
};


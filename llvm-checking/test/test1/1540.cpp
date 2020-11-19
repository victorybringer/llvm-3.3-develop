class X {
public:  
  int *p;
  int *q;
  ~X() {
    delete q;
  }
};

class Y : public X {
  public:
    ~Y(){}
};

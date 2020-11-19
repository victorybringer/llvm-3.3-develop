

#include <stdio.h>
//#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

using namespace std;




enum DAY {
  MON=1, TES, THU
};

int func1() {
	enum DAY day;
	day = TES;
	switch(day) {
		case 1:
			return 1;
		default:
			return 0;
	}
}

int fn1() {
	char a1 = 't';
  switch(a1) {
     case 1:
     cout << 1;
        return 1;
     default:
     cout << 2;
        return 0;
   }
}





int func2(){
	int *p = NULL;
	int q = *p;
	cout<<q;
	return 0; 
}




int g(int x) {
	x = x - 5;
	int y = 100/x;
		if(y > 0) {
			int z = 100 / y;
		}
	return y;
}

int func3(){
	cout<<g(5);
	return 0; 
}

int fn3(){
    int x = 10;
    int y = x - 10;
    int z = 100%y;
    return 0;
}





int func4(){
	int a[10];
	a[10] = 0;
	cout<<a[10];
	return 0;
}



/*  A pointer of auto storage class was allocated
         storage which was neither freed nor returned to the caller.
         This represents a "memory leak".  A pointer is considered
         custodial if it uniquely points to the storage area.  It is not
         considered custodial if it has been copied.
*/

int func5(){
	char *p = new char[20];   
	char *q = p;              
	p = new char[20];         
	q = p + 0;                
	strcpy(p, "hello");
	return 0; 
}




int func6(){
	char buf[20];
	strcpy(buf, "a");
	char c = buf[4]; 
	cout<<c;
	return 0; 
}




int func7(){
	int *p = (int*)malloc(sizeof(int));
	free(p);
	free(p);
	return 0; 
}




int f(int x) {
 	if(x > 5) {
return x;
}
}

int func8(){
	cout<<f(5);
	return 0; 
}




int func9(){
	int a = -1;
	unsigned int b = 1;
	cout<<(a > b);
	return 0; 
}

int fn9(){
    int a = -1;
    unsigned int b = 1;
    if (a < b) 
        cout << 1;
    else 
        cout << 2;
}





enum color { red, green, yellow, blue, black};
struct abc { enum color c:2; };

int func10(){
	struct abc example;
	return 0; 
}




int func11() {
	enum DAY day;
	day = MON;
 	switch(day) {
  	case 1:
        return 1;
 	}
 	return 0;
}




class X {
private:
	int a;
public:
	void set(int n){
		a = n;
	}
	void get() const{
		cout<<a;
	}
	int *f() {return &a;}
};

int func12() {
	X x;
	x.set(1);
	*(x.f()) = 2;
	x.get();
 	return 0;
}




class X2 {
private:
	int q;
	int *p;
public:
	void set(int n){
		q=n;
		p=&q;
	}
	int *f() const{
		(*p)++;
		return p;
	}
};

int func13(){
	X2 x;
	x.set(5);
	cout<<*(x.f());
	return 0;	
}




class X3 {
public:
	X3(int len){
		if(len>0)
			p = new int[len];
		else
			p = NULL;
	}
 	~X3() {}
private:
	int *p;
};

int func14(){
	X3 x(5);
	return 0;	
}




class Base {
public:
	int a;
	void get() const{
		cout<<sizeof(a);
	}
};

class Derived: public Base{
public:
	double b;
	void get() const{
		cout<<sizeof(b);
	}
};

int func15(){
	Derived *d = new Derived[10];
	Base *b;
	b = d;
	b[0].get();
	return 0;	
}


//
union valueContents {
  int v_enum;
  const char *v_string;
};

struct value {
  int tag;
  double z;
  union valueContents data;
};

void value_set_method(struct value *val, int v) {
  val->data.v_enum = v;
  val->tag = 0;
}

void value_free(struct value *v) {
  switch(v->tag) {
  case 1:
    free((void*)v->data.v_string);
    break;
  default:
    break;
  }

  free(v);
}


/*
target datalayout = ""e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128""
target triple = ""x86_64-pc-linux-gnu""


@_ZStL8__ioinit = internal %class.std::ios_base::Init zeroinitializer
@.str = private constant [6 x i8] [i8 104, i8 101, i8 108, i8 108, i8 111, i8 0]
@.str1 = private constant [2 x i8] [i8 97, i8 0]
@llvm.global_ctors = appending [1 x {i32, void()*}] [{i32, void()*} {i32 65535, void() @_GLOBAL__I_a}]

define internal void @__cxx_global_var_init ( ) .text.startup {
 ; <label>:0
  call void @_ZNSt8ios_base4InitC1Ev ( %class.std::ios_base::Init* @_ZStL8__ioinit ), !dbg !5
  %1 = call i32 @__cxa_atexit ( void(i8*)* void(i8*)* bitcast (void(%class.std::ios_base::Init*) @_ZNSt8ios_base4InitD1Ev to void(i8*)*), i8* i8* getelementptr ( %class.std::ios_base::Init* @_ZStL8__ioinit ,  i32 0, i32 0 ), i8* @__dso_handle ), !dbg !6
  ret void, !dbg !7 }

define i32 @_Z5func1v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %day = alloca i32 , align 4
  store i32 2 , i32* %day , align 4, !dbg !13
  %2 = load i32* %day , align 4, !dbg !14
  switch i32 %2 , label %4 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %3 ], !dbg !15
; <label>:3
  store i32 1 , i32* %1, !dbg !16
  br label %5, !dbg !17
; <label>:4
  store i32 0 , i32* %1, !dbg !18
  br label %5, !dbg !19
; <label>:5
  %6 = load i32* %1, !dbg !20
  ret i32 %6, !dbg !21 }

define i32 @_Z3fn1v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %a1 = alloca i8 , align 1
  store i8 116 , i8* %a1 , align 1, !dbg !23
  %2 = load i8* %a1 , align 1, !dbg !24
  %3 = sext i8 %2 to i32, !dbg !25
  switch i32 %3 , label %6 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %4 ], !dbg !26
; <label>:4
  %5 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 1 ), !dbg !27
  store i32 1 , i32* %1, !dbg !28
  br label %8, !dbg !29
; <label>:6
  %7 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 2 ), !dbg !30
  store i32 0 , i32* %1, !dbg !31
  br label %8, !dbg !32
; <label>:8
  %9 = load i32* %1, !dbg !33
  ret i32 %9, !dbg !34 }

define i32 @_Z5func2v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %q = alloca i32 , align 4
  store i32* null , i32** %p , align 8, !dbg !36
  %1 = load i32** %p , align 8, !dbg !37
  %2 = load i32* %1 , align 4, !dbg !38
  store i32 %2 , i32* %q , align 4, !dbg !39
  %3 = load i32* %q , align 4, !dbg !40
  %4 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %3 ), !dbg !41
  ret i32 0, !dbg !42 }

define i32 @_Z1gi ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %y = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = load i32* %1 , align 4, !dbg !46
  %3 = sub nsw i32 %2 , 5, !dbg !47
  store i32 %3 , i32* %1 , align 4, !dbg !48
  %4 = load i32* %1 , align 4, !dbg !49
  %5 = div i32 100 , %4, !dbg !50
  store i32 %5 , i32* %y , align 4, !dbg !51
  %6 = load i32* %y , align 4, !dbg !52
  %7 = icmp sgt i32 %6 , 0, !dbg !53
  br i1 %7 , label %8 , label %11, !dbg !54
; <label>:8
  %9 = load i32* %y , align 4, !dbg !55
  %10 = div i32 100 , %9, !dbg !56
  store i32 %10 , i32* %z , align 4, !dbg !57
  br label %11, !dbg !58
; <label>:11
  %12 = load i32* %y , align 4, !dbg !59
  ret i32 %12, !dbg !60 }

define i32 @_Z5func3v ( ) {
 ; <label>:0
  %1 = call i32 @_Z1gi ( i32 5 ), !dbg !62
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !63
  ret i32 0, !dbg !64 }

define i32 @_Z3fn3v ( ) {
 ; <label>:0
  %x = alloca i32 , align 4
  %y = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 10 , i32* %x , align 4, !dbg !66
  %1 = load i32* %x , align 4, !dbg !67
  %2 = sub nsw i32 %1 , 10, !dbg !68
  store i32 %2 , i32* %y , align 4, !dbg !69
  %3 = load i32* %y , align 4, !dbg !70
  %4 = rem i32 100 , %3, !dbg !71
  store i32 %4 , i32* %z , align 4, !dbg !72
  ret i32 0, !dbg !73 }

define i32 @_Z5func4v ( ) {
 ; <label>:0
  %a = alloca [10 x i32] , align 16
  %1 = getelementptr inbounds [10 x i32]* %a , i32 0, i64 10, !dbg !75
  store i32 0 , i32* %1 , align 4, !dbg !76
  %2 = getelementptr inbounds [10 x i32]* %a , i32 0, i64 10, !dbg !77
  %3 = load i32* %2 , align 4, !dbg !78
  %4 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %3 ), !dbg !79
  ret i32 0, !dbg !80 }

define i32 @_Z5func5v ( ) {
 ; <label>:0
  %p = alloca i8* , align 8
  %q = alloca i8* , align 8
  %1 = call i8* @_Znam ( i64 20 ), !dbg !82
  store i8* %1 , i8** %p , align 8, !dbg !83
  %2 = load i8** %p , align 8, !dbg !84
  store i8* %2 , i8** %q , align 8, !dbg !85
  %3 = call i8* @_Znam ( i64 20 ), !dbg !86
  store i8* %3 , i8** %p , align 8, !dbg !87
  %4 = load i8** %p , align 8, !dbg !88
  %5 = getelementptr inbounds i8* %4 , i64 0, !dbg !89
  store i8* %5 , i8** %q , align 8, !dbg !90
  %6 = load i8** %p , align 8, !dbg !91
  %7 = call i8* @strcpy ( i8* %6, i8* i8* getelementptr ( [6 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !92
  ret i32 0, !dbg !93 }

define i32 @_Z5func6v ( ) {
 ; <label>:0
  %buf = alloca [20 x i8] , align 16
  %c = alloca i8 , align 1
  %1 = getelementptr inbounds [20 x i8]* %buf , i32 0, i32 0, !dbg !95
  %2 = call i8* @strcpy ( i8* %1, i8* i8* getelementptr ( [2 x i8]* @.str1 ,  i32 0, i32 0 ) ), !dbg !96
  %4 = getelementptr inbounds [20 x i8]* %buf , i32 0, i64 4, !dbg !97
  %5 = load i8* %4 , align 1, !dbg !98
  store i8 %5 , i8* %c , align 1, !dbg !99
  %6 = load i8* %c , align 1, !dbg !100
  %7 = call %class.std::basic_ostream* @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_c ( %class.std::basic_ostream* @_ZSt4cout, i8 %6 ), !dbg !101
  ret i32 0, !dbg !102 }

define i32 @_Z5func7v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %1 = call i8* @malloc ( i64 4 ), !dbg !104
  %2 = bitcast i8* %1 to i32*, !dbg !105
  store i32* %2 , i32** %p , align 8, !dbg !106
  %3 = load i32** %p , align 8, !dbg !107
  %4 = bitcast i32* %3 to i8*, !dbg !108
  call void @free ( i8* %4 ), !dbg !109
  %5 = load i32** %p , align 8, !dbg !110
  %6 = bitcast i32* %5 to i8*, !dbg !111
  call void @free ( i8* %6 ), !dbg !112
  ret i32 0, !dbg !113 }

define i32 @_Z1fi ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = load i32* %1 , align 4, !dbg !115
  %3 = icmp sgt i32 %2 , 5, !dbg !116
  br i1 %3 , label %4 , label %6, !dbg !117
; <label>:4
  %5 = load i32* %1 , align 4, !dbg !118
  ret i32 %5, !dbg !119
; <label>:6
  call void @llvm.trap ( ), !dbg !120
  unreachable, !dbg !121 }

define i32 @_Z5func8v ( ) {
 ; <label>:0
  %1 = call i32 @_Z1fi ( i32 5 ), !dbg !123
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !124
  ret i32 0, !dbg !125 }

define i32 @_Z5func9v ( ) {
 ; <label>:0
  %a = alloca i32 , align 4
  %b = alloca i32 , align 4
  store i32 -1 , i32* %a , align 4, !dbg !127
  store i32 1 , i32* %b , align 4, !dbg !128
  %1 = load i32* %a , align 4, !dbg !129
  %2 = load i32* %b , align 4, !dbg !130
  %3 = icmp ugt i32 %1 , %2, !dbg !131
  %4 = call %class.std::basic_ostream* @_ZNSolsEb ( %class.std::basic_ostream* @_ZSt4cout, i1 %3 ), !dbg !132
  ret i32 0, !dbg !133 }

define i32 @_Z3fn9v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %a = alloca i32 , align 4
  %b = alloca i32 , align 4
  store i32 -1 , i32* %a , align 4, !dbg !135
  store i32 1 , i32* %b , align 4, !dbg !136
  %2 = load i32* %a , align 4, !dbg !137
  %3 = load i32* %b , align 4, !dbg !138
  %4 = icmp ult i32 %2 , %3, !dbg !139
  br i1 %4 , label %5 , label %7, !dbg !140
; <label>:5
  %6 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 1 ), !dbg !141
  br label %9, !dbg !142
; <label>:7
  %8 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 2 ), !dbg !143
  br label %9
; <label>:9
  call void @llvm.trap ( )
  unreachable
; <label>:10
  %11 = load i32* %1, !dbg !144
  ret i32 %11, !dbg !145 }

define i32 @_Z6func10v ( ) {
 ; <label>:0
  %example = alloca %struct.abc , align 4
  ret i32 0, !dbg !147 }

define i32 @_Z6func11v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %day = alloca i32 , align 4
  store i32 1 , i32* %day , align 4, !dbg !149
  %2 = load i32* %day , align 4, !dbg !150
  switch i32 %2 , label %4 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %3 ], !dbg !151
; <label>:3
  store i32 1 , i32* %1, !dbg !152
  br label %5, !dbg !153
; <label>:4
  store i32 0 , i32* %1, !dbg !154
  br label %5, !dbg !155
; <label>:5
  %6 = load i32* %1, !dbg !156
  ret i32 %6, !dbg !157 }

define i32 @_Z6func12v ( ) {
 ; <label>:0
  %x = alloca %class.X , align 4
  call void @_ZN1X3setEi ( %class.X* %x, i32 1 ), !dbg !159
  %1 = call i32* @_ZN1X1fEv ( %class.X* %x ), !dbg !160
  store i32 2 , i32* %1 , align 4, !dbg !161
  call void @_ZNK1X3getEv ( %class.X* %x ), !dbg !162
  ret i32 0, !dbg !163 }

define linkonce_odr void @_ZN1X3setEi ( %class.X* %this, i32 %n ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  %2 = alloca i32 , align 4
  store %class.X* %this , %class.X** %1 , align 8
  store i32 %n , i32* %2 , align 4
  %3 = load %class.X** %1
  %4 = load i32* %2 , align 4, !dbg !184
  %5 = getelementptr inbounds %class.X* %3 , i32 0, i32 0, !dbg !185
  store i32 %4 , i32* %5 , align 4, !dbg !186
  ret void, !dbg !187 }

define linkonce_odr i32* @_ZN1X1fEv ( %class.X* %this ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  store %class.X* %this , %class.X** %1 , align 8
  %2 = load %class.X** %1
  %3 = getelementptr inbounds %class.X* %2 , i32 0, i32 0, !dbg !189
  ret i32* %3, !dbg !190 }

define linkonce_odr void @_ZNK1X3getEv ( %class.X* %this ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  store %class.X* %this , %class.X** %1 , align 8
  %2 = load %class.X** %1
  %3 = getelementptr inbounds %class.X* %2 , i32 0, i32 0, !dbg !192
  %4 = load i32* %3 , align 4, !dbg !193
  %5 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %4 ), !dbg !194
  ret void, !dbg !195 }

define i32 @_Z6func13v ( ) {
 ; <label>:0
  %x = alloca %class.X2 , align 8
  call void @_ZN2X23setEi ( %class.X2* %x, i32 5 ), !dbg !197
  %1 = call i32* @_ZNK2X21fEv ( %class.X2* %x ), !dbg !198
  %2 = load i32* %1 , align 4, !dbg !199
  %3 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %2 ), !dbg !200
  ret i32 0, !dbg !201 }

define linkonce_odr void @_ZN2X23setEi ( %class.X2* %this, i32 %n ) {
 ; <label>:0
  %1 = alloca %class.X2* , align 8
  %2 = alloca i32 , align 4
  store %class.X2* %this , %class.X2** %1 , align 8
  store i32 %n , i32* %2 , align 4
  %3 = load %class.X2** %1
  %4 = load i32* %2 , align 4, !dbg !219
  %5 = getelementptr inbounds %class.X2* %3 , i32 0, i32 0, !dbg !220
  store i32 %4 , i32* %5 , align 4, !dbg !221
  %6 = getelementptr inbounds %class.X2* %3 , i32 0, i32 0, !dbg !222
  %7 = getelementptr inbounds %class.X2* %3 , i32 0, i32 1, !dbg !223
  store i32* %6 , i32** %7 , align 8, !dbg !224
  ret void, !dbg !225 }

define linkonce_odr i32* @_ZNK2X21fEv ( %class.X2* %this ) {
 ; <label>:0
  %1 = alloca %class.X2* , align 8
  store %class.X2* %this , %class.X2** %1 , align 8
  %2 = load %class.X2** %1
  %3 = getelementptr inbounds %class.X2* %2 , i32 0, i32 1, !dbg !227
  %4 = load i32** %3 , align 8, !dbg !228
  %5 = load i32* %4 , align 4, !dbg !229
  %6 = add nsw i32 %5 , 1, !dbg !230
  store i32 %6 , i32* %4 , align 4, !dbg !231
  %7 = getelementptr inbounds %class.X2* %2 , i32 0, i32 1, !dbg !232
  %8 = load i32** %7 , align 8, !dbg !233
  ret i32* %8, !dbg !234 }

define i32 @_Z6func14v ( ) {
 ; <label>:0
  %x = alloca %class.X3 , align 8
  %1 = alloca i32
  call void @_ZN2X3C1Ei ( %class.X3* %x, i32 5 ), !dbg !236
  store i32 1 , i32* %1
  call void @_ZN2X3D1Ev ( %class.X3* %x ), !dbg !237
  ret i32 0, !dbg !238 }

define linkonce_odr void @_ZN2X3C1Ei ( %class.X3* %this, i32 %len ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  %2 = alloca i32 , align 4
  store %class.X3* %this , %class.X3** %1 , align 8
  store i32 %len , i32* %2 , align 4
  %3 = load %class.X3** %1
  %4 = load i32* %2 , align 4, !dbg !250
  call void @_ZN2X3C2Ei ( %class.X3* %3, i32 %4 ), !dbg !251
  ret void, !dbg !252 }

define linkonce_odr void @_ZN2X3D1Ev ( %class.X3* %this ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  store %class.X3* %this , %class.X3** %1 , align 8
  %2 = load %class.X3** %1
  call void @_ZN2X3D2Ev ( %class.X3* %2 ), !dbg !254
  ret void, !dbg !255 }

define i32 @_Z6func15v ( ) {
 ; <label>:0
  %d = alloca %class.Derived* , align 8
  %b = alloca %class.Base* , align 8
  %1 = call i8* @_Znam ( i64 160 ), !dbg !257
  %2 = bitcast i8* %1 to %class.Derived*, !dbg !258
  store %class.Derived* %2 , %class.Derived** %d , align 8, !dbg !259
  %3 = load %class.Derived** %d , align 8, !dbg !260
  %4 = bitcast %class.Derived* %3 to %class.Base*, !dbg !261
  store %class.Base* %4 , %class.Base** %b , align 8, !dbg !262
  %5 = load %class.Base** %b , align 8, !dbg !263
  %6 = getelementptr inbounds %class.Base* %5 , i64 0, !dbg !264
  call void @_ZNK4Base3getEv ( %class.Base* %6 ), !dbg !265
  ret i32 0, !dbg !266 }

define linkonce_odr void @_ZNK4Base3getEv ( %class.Base* %this ) {
 ; <label>:0
  %1 = alloca %class.Base* , align 8
  store %class.Base* %this , %class.Base** %1 , align 8
  %2 = load %class.Base** %1
  %3 = call %class.std::basic_ostream* @_ZNSolsEm ( %class.std::basic_ostream* @_ZSt4cout, i64 4 ), !dbg !280
  ret void, !dbg !281 }

define void @_Z16value_set_methodP5valuei ( %struct.value* %val, i32 %v ) {
 ; <label>:0
  %1 = alloca %struct.value* , align 8
  %2 = alloca i32 , align 4
  store %struct.value* %val , %struct.value** %1 , align 8
  store i32 %v , i32* %2 , align 4
  %3 = load i32* %2 , align 4, !dbg !299
  %4 = load %struct.value** %1 , align 8, !dbg !300
  %5 = getelementptr inbounds %struct.value* %4 , i32 0, i32 2, !dbg !301
  %6 = bitcast %union.valueContents* %5 to i32*, !dbg !302
  store i32 %3 , i32* %6 , align 4, !dbg !303
  %7 = load %struct.value** %1 , align 8, !dbg !304
  %8 = getelementptr inbounds %struct.value* %7 , i32 0, i32 0, !dbg !305
  store i32 0 , i32* %8 , align 4, !dbg !306
  ret void, !dbg !307 }

define void @_Z10value_freeP5value ( %struct.value* %v ) {
 ; <label>:0
  %1 = alloca %struct.value* , align 8
  store %struct.value* %v , %struct.value** %1 , align 8
  %2 = load %struct.value** %1 , align 8, !dbg !311
  %3 = getelementptr inbounds %struct.value* %2 , i32 0, i32 0, !dbg !312
  %4 = load i32* %3 , align 4, !dbg !313
  switch i32 %4 , label %10 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %5 ], !dbg !314
; <label>:5
  %6 = load %struct.value** %1 , align 8, !dbg !315
  %7 = getelementptr inbounds %struct.value* %6 , i32 0, i32 2, !dbg !316
  %8 = bitcast %union.valueContents* %7 to i8**, !dbg !317
  %9 = load i8** %8 , align 8, !dbg !318
  call void @free ( i8* %9 ), !dbg !319
  br label %11, !dbg !320
; <label>:10
  br label %11, !dbg !321
; <label>:11
  %12 = load %struct.value** %1 , align 8, !dbg !322
  %13 = bitcast %struct.value* %12 to i8*, !dbg !323
  call void @free ( i8* %13 ), !dbg !324
  ret void, !dbg !325 }

define linkonce_odr void @_ZN2X3D2Ev ( %class.X3* %this ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  store %class.X3* %this , %class.X3** %1 , align 8
  %2 = load %class.X3** %1
  ret void, !dbg !327 }

define linkonce_odr void @_ZN2X3C2Ei ( %class.X3* %this, i32 %len ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  %2 = alloca i32 , align 4
  store %class.X3* %this , %class.X3** %1 , align 8
  store i32 %len , i32* %2 , align 4
  %3 = load %class.X3** %1
  %4 = load i32* %2 , align 4, !dbg !329
  %5 = icmp sgt i32 %4 , 0, !dbg !330
  br i1 %5 , label %6 , label %16, !dbg !331
; <label>:6
  %7 = load i32* %2 , align 4, !dbg !332
  %8 = sext i32 %7 to i64, !dbg !333
  %9 = call {i64, i1} @llvm.umul.with.overflow.i64 ( i64 %8, i64 4 ), !dbg !334
  %10 = extractvalue {i64, i1} %9 1, !dbg !335
  %11 = extractvalue {i64, i1} %9 0, !dbg !336
  %12 = select i1 %10 , i64 -1 , i64 %11, !dbg !337
  %13 = call i8* @_Znam ( i64 %12 ), !dbg !338
  %14 = bitcast i8* %13 to i32*, !dbg !339
  %15 = getelementptr inbounds %class.X3* %3 , i32 0, i32 0, !dbg !340
  store i32* %14 , i32** %15 , align 8, !dbg !341
  br label %18, !dbg !342
; <label>:16
  %17 = getelementptr inbounds %class.X3* %3 , i32 0, i32 0, !dbg !343
  store i32* null , i32** %17 , align 8, !dbg !344
  br label %18
; <label>:18
  ret void, !dbg !345 }

define internal void @_GLOBAL__I_a ( ) .text.startup {
 ; <label>:0
  call void @__cxx_global_var_init ( ), !dbg !349
  ret void, !dbg !350 }
  
*/
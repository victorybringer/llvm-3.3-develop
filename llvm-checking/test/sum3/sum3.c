#include <stdio.h>

int main()
{
    int n, i, sum;
    void A(int *x, int *y);

    printf("Enter a positive number: ");
    scanf("%d", &n);

    sum = 0;
    i = 1;
    while (i <= n)
    {
     A(&sum,&i);
    }

    printf ("sum = %d\n", sum);
    printf ("i = %d\n", i);
    return 0;
}

void A(int *x, int *y)
{
    void add(int *a, int *b);
    void inc(int *z);
    add(x,y);
    inc(y);
}

void add(int *a, int *b)
{
    *a = *a + *b;
//    return(a);
}

void inc(int *z)
{
//    int add(int a, int b);
    int tmp = 1;
    add(z,&tmp);
}

/*
-----------------------
MSlicer results: 
 a@add           {11,12,13,15,27,28,33,40,41}
 b@add           {}
 i@main          {12,13,15,28,33,40,41}
 sum@main        {11,12,13,15,27,28,33,40,41}
 tmp@inc         {40}
 x@A             {11,12,13,15,27,28,33,40,41}
 y@A             {12,13,15,28,33,40,41}
 z@inc           {12,13,15,28,33,40,41}

 

@main's traces: 
 [8,9,11,12,13,13,13,13,15,13,16,18,18,19,19,13,13,13,15,13,16,
  18,18,19,19,13,13,13,15,13,16,18,18,19,19]


----
[13]: "br i1 %9 , label %10 , label %11" (name= Nothing) 
  Its prdedicate: "%9 = icmp sle i32 %7 , %8" (name:line= Nothing:[13])
  Its valueRefs: [%.pre = load i32* %i , align 4,
                  %9 = icmp sle i32 %7 , %8,
                  %i = alloca i32 , align 4,
                  %8 = load i32* %n , align 4,
                  %n = alloca i32 , align 4,
                  %7 = phi i32 [ [%.pre, %10], [1, %0] ],1] 
             --> ["i@main","n@main"]:[13,13,13]
  Its instOperands: [%9 = icmp sle i32 %7 , %8]


//-------------------------------------------------------------
define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %n = alloca i32 , align 4
  %i = alloca i32 , align 4
  %sum = alloca i32 , align 4
  store i32 0 , i32* %1
  %2 = call i32 @printf ( i8* i8* getelementptr ( [26 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !6
  %4 = call i32 @__isoc99_scanf ( i8* i8* getelementptr ( [3 x i8]* @.str1 ,  i32 0, i32 0 ), i32* %n ), !dbg !7
  store i32 0 , i32* %sum , align 4, !dbg !8
  store i32 1 , i32* %i , align 4, !dbg !9
  br label %6, !dbg !10
; <label>:6
  %7 = phi i32 [ [%.pre, %10], [1, %0] ]
  %8 = load i32* %n , align 4, !dbg !11
  %9 = icmp sle i32 %7 , %8, !dbg !12
  br i1 %9 , label %10 , label %11, !dbg !13
; <label>:10
  call void @A ( i32* %sum, i32* %i ), !dbg !14
  %.pre = load i32* %i , align 4, !dbg !15
  br label %6, !dbg !16
; <label>:11
  %12 = load i32* %sum , align 4, !dbg !17
  %13 = call i32 @printf ( i8* i8* getelementptr ( [10 x i8]* @.str2 ,  i32 0, i32 0 ), i32 %12 ), !dbg !18
  %15 = load i32* %i , align 4, !dbg !19
  %16 = call i32 @printf ( i8* i8* getelementptr ( [8 x i8]* @.str3 ,  i32 0, i32 0 ), i32 %15 ), !dbg !20
  ret i32 0, !dbg !21 }

define void @A ( i32* %x, i32* %y ) {
 ; <label>:0
  %1 = alloca i32* , align 4
  %2 = alloca i32* , align 4
  store i32* %x , i32** %1 , align 4
  store i32* %y , i32** %2 , align 4
  call void @add ( i32* %x, i32* %y ), !dbg !26
  call void @inc ( i32* %y ), !dbg !27
  ret void, !dbg !28 }

define void @add ( i32* %a, i32* %b ) {
 ; <label>:0
  %1 = alloca i32* , align 4
  %2 = alloca i32* , align 4
  store i32* %a , i32** %1 , align 4
  store i32* %b , i32** %2 , align 4
  %3 = load i32* %a , align 4, !dbg !30
  %4 = load i32* %b , align 4, !dbg !31
  %5 = add nsw i32 %3 , %4, !dbg !32
  store i32 %5 , i32* %a , align 4, !dbg !33
  ret void, !dbg !34 }

define void @inc ( i32* %z ) {
 ; <label>:0
  %1 = alloca i32* , align 4
  %tmp = alloca i32 , align 4
  store i32* %z , i32** %1 , align 4
  store i32 1 , i32* %tmp , align 4, !dbg !38
  call void @add ( i32* %z, i32* %tmp ), !dbg !39
  ret void, !dbg !40 }

*/
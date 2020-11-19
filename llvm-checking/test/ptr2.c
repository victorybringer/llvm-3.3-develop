int x , y , z;
int *x0, *y0, *z0;

main () 
{
  x0= &x; 
  y0= &y;
  z0= &z;
  if (x==y)
 S1: f (&x0, &y0, &z0) ;
  else if (y!=z)
 S2: f (&z0, &x0, &y0) ;
  else
 S3: f (&x0, &y0, &x0) ;
}

f ( int **p , int **q, int **r)
{ 
	*p = *q;
	*q = *r; 
}



/*

Variable      Labels  
----------------------
 p@f        {6,7,9,10,11,12,14,19}
 q@f        {6,7,8,9,10,11,12,14,20}
 r@f        {}
 x          {}
 x0         {7,9,10,11,12,14,19,20}
 y          {}
 y0         {6,7,8,9,10,11,14,20}
 z          {}
 z0         {6,8,9,11,12,19}


---------------
@x = common i32 0
@x0 = common i32* null
@y = common i32 0
@y0 = common i32* null
@z = common i32 0
@z0 = common i32* null

define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 0 , i32* %1
  store i32* @x , i32** @x0 , align 4, !dbg !6
  store i32* @y , i32** @y0 , align 4, !dbg !7
  store i32* @z , i32** @z0 , align 4, !dbg !8
  %2 = load i32* @x , align 4, !dbg !9
  %3 = load i32* @y , align 4, !dbg !10
  %4 = icmp eq i32 %2 , %3, !dbg !11
  br i1 %4 , label %5 , label %7, !dbg !12
; <label>:5
  %6 = call i32 @f ( i32** @x0, i32** @y0, i32** @z0 ), !dbg !13
  br label %15, !dbg !14
; <label>:7
  %8 = load i32* @z , align 4, !dbg !15
  %9 = icmp ne i32 %3 , %8, !dbg !16
  br i1 %9 , label %10 , label %12, !dbg !17
; <label>:10
  %11 = call i32 @f ( i32** @z0, i32** @x0, i32** @y0 ), !dbg !18
  br label %14, !dbg !19
; <label>:12
  %13 = call i32 @f ( i32** @x0, i32** @y0, i32** @x0 ), !dbg !20
  br label %14
; <label>:14
  br label %15
; <label>:15
  ret i32 0, !dbg !21 }

define i32 @f ( i32** %p, i32** %q, i32** %r ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32** , align 4
  %3 = alloca i32** , align 4
  %4 = alloca i32** , align 4
  store i32** %p , i32*** %2 , align 4
  store i32** %q , i32*** %3 , align 4
  store i32** %r , i32*** %4 , align 4
  %5 = load i32** %q , align 4, !dbg !27
  store i32* %5 , i32** %p , align 4, !dbg !28
  %6 = load i32** %r , align 4, !dbg !29
  store i32* %6 , i32** %q , align 4, !dbg !30
  ret i32 undef, !dbg !31 }

*/

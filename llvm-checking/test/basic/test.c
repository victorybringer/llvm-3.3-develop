#include <stdio.h>
int x,y,v,w,z;

int procedureA()
{
    int procedureB(int bb);
    int aa,i;    
    scanf("%d",&i);
    aa=x*x;
    if(i>0)
    {
        procedureB(aa);
        z=y+1;
    }
    return z;
}
int procedureB(int bb)
{
    int procedureA();
    int procedureC();
    y=bb-10;
    procedureA();
    //.......
    procedureC();
    return 0;
}

int procedureC()
{
    int s;
    scanf("%d",&s);
    v=v+s;
    w=v-s;
    return 0;
}

int main()
{
    int procedureA();
    scanf("%d%d",&v,&w);
    z=procedureA();
    v=z+v;
    printf("%d%d%d",z,v,w);
    return 0;
}



/*
______________
MSlicer:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 aa@procedureA        {"test.c: [8,9,10,12,22,41]"}
 bb@procedureB        {"test.c: [8,9,10,12,22,41]"}
 i@procedureA         {"test.c: [8,10,12,22,41]"}
 s@procedureC         {"test.c: [8,10,12,22,24,31,41]"}
 v                    {"test.c: [8,9,10,12,13,15,21,22,24,31,32,40,41,42]"}
 w                    {"test.c: [8,10,12,22,24,31,32,33,40,41]"}
 x                    {}
 y                    {"test.c: [8,9,10,12,21,22,41]"}
 z                    {"test.c: [8,9,10,12,13,15,21,22,41]"}


Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 aa@procedureA        {"test.c: []"}
 bb@procedureB        {"test.c: [13,21,41,42]"}
 i@procedureA         {"test.c: [8,10,12,13,15,21,22,24,41,42,43]"}
 s@procedureC         {"test.c: [31,32,33,42,43]"}
 v                    {"test.c: [32,33,40,42,43]"}
 w                    {"test.c: [40,42,43]"}
 x                    {"test.c: [9,12,13,21,22,24,41,42,43]"}
 y                    {"test.c: [8,9,10,12,13,15,22,41,42,43]"}
 z                    {"test.c: [8,9,10,12,13,15,22,41,42,43]"}





______________
SDGSlicer:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 aa@procedureA        {"test.c: [4,8,9,10,12,17,22,37,41]"}
 bb@procedureB        {"test.c: [4,8,9,10,12,17,22,37,41]"}
 i@procedureA         {"test.c: [4,8,10,12,17,22,37,41]"}
 s@procedureC         {"test.c: [4,8,10,12,17,22,24,28,31,37,41]"}
 v                    {"test.c: [4,8,9,10,12,13,15,17,21,22,24,28,31,32,37,40,41,42]"}
 w                    {"test.c: [4,8,10,12,17,22,24,28,31,32,33,37,40,41]"}
 x                    {"test.c: [37]"}
 y                    {"test.c: [4,8,9,10,12,17,21,22,37,41]"}
 z                    {"test.c: [4,8,9,10,12,13,15,17,21,22,37,41]"}

Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 aa@procedureA        {"test.c: [9]"}
 bb@procedureB        {"test.c: [13,15,21,41,42,43]"}
 i@procedureA         {"test.c: [4,8,9,10,12,13,14,15,17,21,22,24,25,28,31,32,33,34,41,42,43]"}
 s@procedureC         {"test.c: [31,32,33,42,43]"}
 v                    {"test.c: [32,33,40,42,43]"}
 w                    {"test.c: [32,33,40,42,43]"}
 x                    {"test.c: [9,13,15,21,41,42,43]"}
 y                    {"test.c: [13,15,41,42,43]"}
 z                    {"test.c: [15,41,42,43]"}








________________
MWeiser:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 aa@procedureA        {"test.c: [9]"}
 bb@procedureB        {"test.c: []"}
 i@procedureA         {}
 s@procedureC         {}
 v                    {"test.c: [4,9,10,12,17,22,24,28,32,41,42]"}
 w                    {"test.c: [4,9,10,12,17,22,24,28,32,33,41]"}
 x                    {}
 y                    {"test.c: [4,9,10,12,17,21,22,41]"}
 z                    {"test.c: [4,41]"}


_________________________________________
----------------------------------------
target datalayout = ""e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128""
target triple = ""i386-pc-linux-gnu""


@.str = private constant [3 x i8] [i8 37, i8 102, i8 0]
@x = common i32 0
@y = common i32 0
@z = common i32 0
@v = common i32 0
@w = common i32 0
@.str1 = private constant [5 x i8] [i8 37, i8 102, i8 37, i8 102, i8 0]
@.str2 = private constant [7 x i8] [i8 37, i8 102, i8 37, i8 102, i8 37, i8 102, i8 0]

define i32 @procedureA ( ) {
 ; <label>:0
  %aa = alloca i32 , align 4
  %i = alloca i32 , align 4
  %1 = call i32 @__isoc99_scanf ( i8* i8* getelementptr ( [3 x i8]* @.str ,  i32 0, i32 0 ), i32* %i ), !dbg !6
  %3 = load i32* @x , align 4, !dbg !7
  %4 = mul nsw i32 %3 , %3, !dbg !8
  store i32 %4 , i32* %aa , align 4, !dbg !9
  %5 = load i32* %i , align 4, !dbg !10
  %6 = icmp sgt i32 %5 , 0, !dbg !11
  br i1 %6 , label %7 , label %._crit_edge, !dbg !12
._crit_edge:
  %.pre = load i32* @z , align 4, !dbg !13
  br label %11, !dbg !14
; <label>:7
  %8 = call i32 @procedureB ( i32 %4 ), !dbg !15
  %9 = load i32* @y , align 4, !dbg !16
  %10 = add nsw i32 %9 , 1, !dbg !17
  store i32 %10 , i32* @z , align 4, !dbg !18
  br label %11, !dbg !19
; <label>:11
  %12 = phi i32 [ [%.pre, %._crit_edge], [%10, %7] ]
  ret i32 %12, !dbg !20 }

define i32 @procedureB ( i32 %bb ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 %bb , i32* %1 , align 4
  %2 = sub nsw i32 %bb , 10, !dbg !24
  store i32 %2 , i32* @y , align 4, !dbg !25
  %3 = call i32 @procedureA ( ), !dbg !26
  %4 = call i32 @procedureC ( ), !dbg !27
  ret i32 0, !dbg !28 }

define i32 @procedureC ( ) {
 ; <label>:0
  %s = alloca float , align 4
  %1 = call i32 @__isoc99_scanf ( i8* i8* getelementptr ( [3 x i8]* @.str ,  i32 0, i32 0 ), float* %s ), !dbg !30
  %2 = load i32* @v , align 4, !dbg !31
  %3 = sitofp i32 %2 to float, !dbg !32
  %4 = load float* %s , align 4, !dbg !33
  %5 = add float %3 , %4, !dbg !34
  %6 = fptosi float %5 to i32, !dbg !35
  store i32 %6 , i32* @v , align 4, !dbg !36
  %7 = sitofp i32 %6 to float, !dbg !37
  %8 = sub float %7 , %4, !dbg !38
  %9 = fptosi float %8 to i32, !dbg !39
  store i32 %9 , i32* @w , align 4, !dbg !40
  ret i32 0, !dbg !41 }

define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 0 , i32* %1
  %2 = call i32 @__isoc99_scanf ( i8* i8* getelementptr ( [5 x i8]* @.str1 ,  i32 0, i32 0 ), i32* @v, i32* @w ), !dbg !43
  %4 = call i32 @procedureA ( ), !dbg !44
  store i32 %4 , i32* @z , align 4, !dbg !45
  %5 = load i32* @v , align 4, !dbg !46
  %6 = add nsw i32 %4 , %5, !dbg !47
  store i32 %6 , i32* @v , align 4, !dbg !48
  %7 = load i32* @w , align 4, !dbg !49
  %8 = call i32 @printf ( i8* i8* getelementptr ( [7 x i8]* @.str2 ,  i32 0, i32 0 ), i32 %4, i32 %6, i32 %7 ), !dbg !50
  ret i32 0, !dbg !51 }




*/
*Main> IM.map valueLine vm
fromList [(1,[]),(6,[]),(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(14,[]),(16,[4]),(17,[]),(19,[]),(20,[]),(21,[8]),(24,[9]),(25,[9]),(26,[9]),(27,[10]),(28,[10]),(29,[10]),(30,[]),(31,[15]),(32,[10]),(33,[]),(34,[12]),(35,[13]),(36,[13]),(37,[13]),(38,[14]),(39,[]),(40,[]),(41,[15]),(42,[17]),(43,[]),(44,[]),(45,[]),(46,[]),(48,[21]),(49,[21]),(50,[22]),(51,[24]),(52,[25]),(53,[28]),(54,[]),(55,[]),(56,[31]),(57,[32]),(58,[32]),(59,[32]),(60,[32]),(61,[33]),(62,[34]),(63,[37]),(64,[]),(65,[]),(66,[]),(67,[40]),(70,[41]),(71,[41]),(72,[42]),(73,[42]),(74,[42]),(75,[43]),(76,[43]),(79,[44]),(80,[]),(81,[]),(82,[])]

*Main> IM.map valueName vm
fromList [(1,Just @.str),(6,Just @x),(8,Just @y),(9,Just @z),(10,Just @v),(11,Just @w),(12,Just @.str1),(14,Just @.str2),(16,Just @procedureA),(17,Just %0),(19,Just %aa),(20,Just %i),(21,Just %1),(24,Just %3),(25,Just %4),(26,Nothing),(27,Just %5),(28,Just %6),(29,Nothing),(30,Just %._crit_edge),(31,Just %.pre),(32,Nothing),(33,Just %7),(34,Just %8),(35,Just %9),(36,Just %10),(37,Nothing),(38,Nothing),(39,Just %11),(40,Just %12),(41,Nothing),(42,Just @procedureB),(43,Just %bb),(44,Just %0),(45,Just %1),(46,Nothing),(48,Just %2),(49,Nothing),(50,Just %3),(51,Just %4),(52,Nothing),(53,Just @procedureC),(54,Just %0),(55,Just %s),(56,Just %1),(57,Just %2),(58,Just %3),(59,Just %4),(60,Nothing),(61,Nothing),(62,Nothing),(63,Just @main),(64,Just %0),(65,Just %1),(66,Nothing),(67,Just %2),(70,Just %4),(71,Nothing),(72,Just %5),(73,Just %6),(74,Nothing),(75,Just %7),(76,Just %8),(79,Nothing),(80,Just @llvm.dbg.declare),(81,Just @__isoc99_scanf),(82,Just @printf)]




	
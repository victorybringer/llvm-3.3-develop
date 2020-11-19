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
____________________
MSlicer:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 p@f        {"test2.c: [6,7,8,9,10,11,12,14,19]"}
 q@f        {"test2.c: [6,7,8,9,10,11,12,14,20]"}
 r@f        {"test2.c: [6,7,8,9,10,11,12,14]"}
 x          {}
 x0         {"test2.c: [7,9,10,11,12,14,19,20]"}
 y          {}
 y0         {"test2.c: [6,7,8,9,10,11,14,20]"}
 z          {}
 z0         {"test2.c: [6,8,9,11,12,19]"}


Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 p@f        {"test2.c: [10,19]"}
 q@f        {"test2.c: [10,19,20]"}
 r@f        {"test2.c: [10,20]"}
 x          {"test2.c: [6,9,10,11,12,14]"}
 x0         {"test2.c: [6,10,19]"}
 y          {"test2.c: [7,9,10,11,12,14]"}
 y0         {"test2.c: [7,10,19,20]"}
 z          {"test2.c: [8,10]"}
 z0         {"test2.c: [8,10,20]"}


_____________
SDGSlicer:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 p@f        {"test2.c: [4,6,7,8,9,10,11,12,14,17,19]"}
 q@f        {"test2.c: [4,6,7,8,9,10,11,12,14,17,20]"}
 r@f        {"test2.c: [4,6,7,8,9,10,11,12,14,17]"}
 x          {}
 x0         {"test2.c: [4,6,7,9,10,11,12,14,17,19,20]"}
 y          {}
 y0         {"test2.c: [4,6,7,8,9,10,11,12,14,17,20]"}
 z          {}
 z0         {"test2.c: [4,6,8,9,11,12,17,19]"}


Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 p@f        {"test2.c: []"}
 q@f        {"test2.c: [19]"}
 r@f        {"test2.c: [20]"}
 x          {"test2.c: [6,9,10,11,12,14,15,17,19,20,21]"}
 x0         {}
 y          {"test2.c: [7,9,10,11,12,14,15,17,19,20,21]"}
 y0         {}
 z          {"test2.c: [8,11,12,14,15,17,19,20,21]"}
 z0         {}

_______
MWeiser:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 p@f        {"test2.c: [6,7,8,19]"}
 q@f        {"test2.c: [6,7,8,20]"}
 r@f        {"test2.c: [6,7,8]"}
 x          {}
 x0         {"test2.c: [6,7,8,9,10,11,12,14,17,19,20]"}
 y          {}
 y0         {"test2.c: [6,7,8,9,10,11,14,17,20]"}
 z          {}
 z0         {"test2.c: [6,7,8,9,11,12,17,19]"}


__________________
LLVM IR:

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

	SInfo {_sdg = mkGraph [(-2147483625,ActualOutNode (22,3)),(-2147483618,ActualOutNode (29,8)),(-2147483615,ActualOutNode (32,3)),(-1073741811,FinalUseNode 12),(-1073741780,FinalUseNode 43),(-1073741779,ActualOutNode (22,6)),(-1073741778,FinalUseNode 45),(-1073741777,FinalUseNode 46),(-1073741765,ActualOutNode (29,3)),(-1073741759,ActualOutNode (32,6)),(-715827816,ActualOutNode (22,8)),(-715827795,ActualOutNode (29,6)),(-715827786,ActualOutNode (32,3)),(-41,FormalOutNode 41),(-40,FormalOutNode 40),(-39,FormalOutNode 39),(-8,GlobalVariableOutNode 8),(-7,GlobalVariableOutNode 7),(-6,GlobalVariableOutNode 6),(-5,GlobalVariableOutNode 5),(-3,GlobalVariableOutNode 3),(-1,GlobalVariableOutNode 1),(1,GlobalVariableInNode 1),(3,GlobalVariableInNode 3),(5,GlobalVariableInNode 5),(6,GlobalVariableInNode 6),(7,GlobalVariableInNode 7),(8,GlobalVariableInNode 8),(9,FunctionEntryNode 9),(12,InstructionNode 12),(13,InstructionNode 13),(14,InstructionNode 14),(15,InstructionNode 15),(16,InstructionNode 16),(17,InstructionNode 17),(18,InstructionNode 18),(19,InstructionNode 19),(20,InstructionNode 20),(22,InstructionNode 22),(23,InstructionNode 23),(25,InstructionNode 25),(26,InstructionNode 26),(27,InstructionNode 27),(29,InstructionNode 29),(30,InstructionNode 30),(32,InstructionNode 32),(33,InstructionNode 33),(35,InstructionNode 35),(37,InstructionNode 37),(38,FunctionEntryNode 38),(39,FormalInNode 39),(40,FormalInNode 40),(41,FormalInNode 41),(43,InstructionNode 43),(44,InstructionNode 44),(45,InstructionNode 45),(46,InstructionNode 46),(47,InstructionNode 47),(48,InstructionNode 48),(49,InstructionNode 49),(50,InstructionNode 50),(51,InstructionNode 51),(52,InstructionNode 52),(53,InstructionNode 53),(54,InstructionNode 54),(56,FunctionEntryNode 56),(715827786,ActualInNode (32,3)),(715827795,ActualInNode (29,6)),(715827816,ActualInNode (22,8)),(1073741759,ActualInNode (32,6)),(1073741765,ActualInNode (29,3)),(1073741779,ActualInNode (22,6)),(2147483615,ActualInNode (32,3)),(2147483618,ActualInNode (29,8)),(2147483625,ActualInNode (22,3))] [(-2147483625,-39,ParaOutEdge),(-2147483625,22,ControlDepEdge),(-2147483625,1073741779,SummaryEdge),(-2147483625,2147483625,SummaryEdge),(-2147483618,-39,ParaOutEdge),(-2147483618,29,ControlDepEdge),(-2147483618,1073741765,SummaryEdge),(-2147483618,2147483618,SummaryEdge),(-2147483615,-39,ParaOutEdge),(-2147483615,32,ControlDepEdge),(-2147483615,1073741759,SummaryEdge),(-2147483615,2147483615,SummaryEdge),(-1073741811,9,ControlDepEdge),(-1073741780,38,ControlDepEdge),(-1073741779,-40,ParaOutEdge),(-1073741779,22,ControlDepEdge),(-1073741779,38,ControlDepEdge),(-1073741779,47,DataDepEdge),(-1073741779,715827816,SummaryEdge),(-1073741779,1073741779,SummaryEdge),(-1073741778,38,ControlDepEdge),(-1073741778,48,DataDepEdge),(-1073741777,38,ControlDepEdge),(-1073741777,49,DataDepEdge),(-1073741765,-40,ParaOutEdge),(-1073741765,29,ControlDepEdge),(-1073741765,715827795,SummaryEdge),(-1073741765,1073741765,SummaryEdge),(-1073741759,-40,ParaOutEdge),(-1073741759,32,ControlDepEdge),(-1073741759,715827786,SummaryEdge),(-1073741759,1073741759,SummaryEdge),(-715827816,-41,ParaOutEdge),(-715827816,22,ControlDepEdge),(-715827816,715827816,SummaryEdge),(-715827795,-41,ParaOutEdge),(-715827795,29,ControlDepEdge),(-715827795,715827795,SummaryEdge),(-715827786,-41,ParaOutEdge),(-715827786,32,ControlDepEdge),(-715827786,715827786,SummaryEdge),(-41,38,ControlDepEdge),(-40,38,ControlDepEdge),(-40,53,DataDepEdge),(-39,38,ControlDepEdge),(-39,51,DataDepEdge),(-8,-2147483618,DataDepEdge),(-8,-715827816,DataDepEdge),(-8,16,DataDepEdge),(-6,-1073741779,DataDepEdge),(-6,-1073741759,DataDepEdge),(-6,-715827795,DataDepEdge),(-3,-2147483625,DataDepEdge),(-3,-2147483615,DataDepEdge),(-3,-1073741765,DataDepEdge),(12,9,ControlDepEdge),(13,9,ControlDepEdge),(14,9,ControlDepEdge),(15,9,ControlDepEdge),(16,9,ControlDepEdge),(17,9,ControlDepEdge),(18,9,ControlDepEdge),(19,9,ControlDepEdge),(20,9,ControlDepEdge),(35,9,ControlDepEdge),(37,9,ControlDepEdge),(38,22,CallEdge),(38,29,CallEdge),(38,32,CallEdge),(39,38,ControlDepEdge),(39,2147483615,ParaInEdge),(39,2147483618,ParaInEdge),(39,2147483625,ParaInEdge),(40,38,ControlDepEdge),(40,1073741759,ParaInEdge),(40,1073741765,ParaInEdge),(40,1073741779,ParaInEdge),(41,38,ControlDepEdge),(41,715827786,ParaInEdge),(41,715827795,ParaInEdge),(41,715827816,ParaInEdge),(43,38,ControlDepEdge),(44,38,ControlDepEdge),(45,38,ControlDepEdge),(46,38,ControlDepEdge),(47,38,ControlDepEdge),(47,39,DataDepEdge),(48,38,ControlDepEdge),(48,40,DataDepEdge),(49,38,ControlDepEdge),(49,41,DataDepEdge),(50,38,ControlDepEdge),(50,40,DataDepEdge),(51,38,ControlDepEdge),(51,50,DataDepEdge),(52,38,ControlDepEdge),(52,41,DataDepEdge),(53,38,ControlDepEdge),(53,52,DataDepEdge),(54,38,ControlDepEdge),(715827786,3,DataDepEdge),(715827786,14,DataDepEdge),(715827786,32,ControlDepEdge),(715827795,6,DataDepEdge),(715827795,15,DataDepEdge),(715827795,29,ControlDepEdge),(715827816,8,DataDepEdge),(715827816,16,DataDepEdge),(715827816,22,ControlDepEdge),(1073741759,6,DataDepEdge),(1073741759,15,DataDepEdge),(1073741759,32,ControlDepEdge),(1073741765,3,DataDepEdge),(1073741765,14,DataDepEdge),(1073741765,29,ControlDepEdge),(1073741779,6,DataDepEdge),(1073741779,15,DataDepEdge),(1073741779,22,ControlDepEdge),(2147483615,3,DataDepEdge),(2147483615,14,DataDepEdge),(2147483615,32,ControlDepEdge),(2147483618,8,DataDepEdge),(2147483618,16,DataDepEdge),(2147483618,29,ControlDepEdge),(2147483625,3,DataDepEdge),(2147483625,14,DataDepEdge),(2147483625,22,ControlDepEdge)], _ddMap = fromList [(12,fromList []),(13,fromList []),(14,fromList [1]),(15,fromList [5]),(16,fromList [7]),(17,fromList [1]),(18,fromList [5]),(19,fromList [17,18]),(20,fromList [19]),(23,fromList []),(25,fromList [7]),(26,fromList [18,25]),(27,fromList [26]),(30,fromList []),(33,fromList []),(35,fromList []),(37,fromList []),(715827786,fromList [3,14]),(715827795,fromList [6,15]),(715827816,fromList [8,16]),(1073741759,fromList [6,15]),(1073741765,fromList [3,14]),(1073741779,fromList [6,15]),(2147483615,fromList [3,14]),(2147483618,fromList [8,16]),(2147483625,fromList [3,14])], _inMap = fromList [(12,fromList [(1,1),(3,3),(5,5),(6,6),(7,7),(8,8)]),(13,fromList []),(14,fromList [(13,12)]),(15,fromList [(13,12),(14,3)]),(16,fromList [(13,12),(14,3),(15,6)]),(17,fromList [(13,12),(14,3),(15,6),(16,8)]),(18,fromList [(13,12),(14,3),(15,6),(16,8)]),(19,fromList [(13,12),(14,3),(15,6),(16,8)]),(20,fromList [(13,12),(14,3),(15,6),(16,8)]),(22,fromList [(13,12),(14,3),(15,6),(16,8)]),(23,fromList [(-2147483625,3),(-1073741779,6),(-715827816,8),(13,12)]),(25,fromList [(13,12),(14,3),(15,6),(16,8)]),(26,fromList [(13,12),(14,3),(15,6),(16,8)]),(27,fromList [(13,12),(14,3),(15,6),(16,8)]),(29,fromList [(13,12),(14,3),(15,6),(16,8)]),(30,fromList [(-2147483618,8),(-1073741765,3),(-715827795,6),(13,12)]),(32,fromList [(13,12),(14,3),(15,6),(16,8)]),(33,fromList [(-2147483615,3),(-1073741759,6),(13,12),(16,8)]),(35,fromList [(-2147483618,8),(-2147483615,3),(-1073741765,3),(-1073741759,6),(-715827795,6),(13,12),(16,8)]),(37,fromList [(-2147483625,3),(-2147483618,8),(-2147483615,3),(-1073741779,6),(-1073741765,3),(-1073741759,6),(-715827816,8),(-715827795,6),(13,12),(16,8)])], _outMap = fromList [(12,fromList []),(13,fromList [(13,12)]),(14,fromList [(13,12),(14,3)]),(15,fromList [(13,12),(14,3),(15,6)]),(16,fromList [(13,12),(14,3),(15,6),(16,8)]),(17,fromList [(13,12),(14,3),(15,6),(16,8)]),(18,fromList [(13,12),(14,3),(15,6),(16,8)]),(19,fromList [(13,12),(14,3),(15,6),(16,8)]),(20,fromList [(13,12),(14,3),(15,6),(16,8)]),(22,fromList [(-2147483625,3),(-1073741779,6),(-715827816,8),(13,12)]),(23,fromList [(-2147483625,3),(-1073741779,6),(-715827816,8),(13,12)]),(25,fromList [(13,12),(14,3),(15,6),(16,8)]),(26,fromList [(13,12),(14,3),(15,6),(16,8)]),(27,fromList [(13,12),(14,3),(15,6),(16,8)]),(29,fromList [(-2147483618,8),(-1073741765,3),(-715827795,6),(13,12)]),(30,fromList [(-2147483618,8),(-1073741765,3),(-715827795,6),(13,12)]),(32,fromList [(-2147483615,3),(-1073741759,6),(13,12),(16,8)]),(33,fromList [(-2147483615,3),(-1073741759,6),(13,12),(16,8)]),(35,fromList [(-2147483618,8),(-2147483615,3),(-1073741765,3),(-1073741759,6),(-715827795,6),(13,12),(16,8)]),(37,fromList [(-2147483625,3),(-2147483618,8),(-2147483615,3),(-1073741779,6),(-1073741765,3),(-1073741759,6),(-715827816,8),(-715827795,6),(13,12),(16,8)])]}


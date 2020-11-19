int main()
{
    int i, sum;
    sum = 0;
    i = 0;

    while (i < 10)
    {
     i = i + 1;
     sum = CalcSum(sum);
    }

    i = i;
    sum = sum;
}

int CalcSum(int s)
{
    void Inc(int *x);
    Inc(&s);
    s = s + 9;
    return s;
}

void Inc(int *x)
{
    *x = *x + 1;
}


/*
________________
MSlicer results: 
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 i@main           {"sum2.c: [5,7,9,13]"}
 s@CalcSum        {"sum2.c: [4,5,7,9,10,17,20,21,22,27]"}
 sum@main         {"sum2.c: [4,5,7,9,10,14,17,20,21,22,27]"}
 x@Inc            {"sum2.c: [4,5,7,9,10,17,20,21,22,27]"}




Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 i@main           {"sum2.c: [5,7,9,10,13,14]"}
 s@CalcSum        {"sum2.c: [14,20,21,27]"}
 sum@main         {"sum2.c: [4,14]"}
 x@Inc            {"sum2.c: [20,21,27]"}



__________________
SDGSlicer results:
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 i@main           {"sum2.c: [1,5,7,9,13]"}
 s@CalcSum        {"sum2.c: [1,5,7,9,10,17,20,21,25,27]"}
 sum@main         {"sum2.c: [1,4,5,7,9,10,14]"}
 x@Inc            {"sum2.c: [1,5,7,9,10,17,20,25,27]"}



Forward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 i@main           {"sum2.c: [13]"}
 s@CalcSum        {"sum2.c: [21,22,27]"}
 sum@main         {"sum2.c: [14]"}
 x@Inc            {"sum2.c: [21,22,27]"}

_______________
MWeiser results: 
Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 i@main           {"sum2.c: [5,7,9,13]"}
 s@CalcSum        {}
 sum@main         {"sum2.c: [4,5,7,9,10,14,17]"}
 x@Inc            {"sum2.c: [27]"}


Its some statistic Info.:
	#Defined_Functions = 3
	CFG(#Nodes,#Edges) = ["main: (20,20)","CalcSum: (8,7)","Inc: (7,6)"]
	#BasicBlocks = 6
	#Insts_All = 32
	  #Insts_alloc = 5
	  #Insts_br = 1



//-------------------------------------------------------------
define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %i = alloca i32 , align 4
  %sum = alloca i32 , align 4
  store i32 0 , i32* %1
  store i32 0 , i32* %sum , align 4, !dbg !6
  store i32 0 , i32* %i , align 4, !dbg !7
  br label %2, !dbg !8
; <label>:2
  %3 = phi i32 [ [%8, %6], [0, %0] ]
  %4 = phi i32 [ [%7, %6], [0, %0] ]
  %5 = icmp slt i32 %4 , 10, !dbg !9
  br i1 %5 , label %6 , label %9, !dbg !10
; <label>:6
  %7 = add nsw i32 %4 , 1, !dbg !11
  store i32 %7 , i32* %i , align 4, !dbg !12
  %8 = call i32 @CalcSum ( i32 %3 ), !dbg !13
  store i32 %8 , i32* %sum , align 4, !dbg !14
  br label %2, !dbg !15
; <label>:9
  store i32 %4 , i32* %i , align 4, !dbg !16
  store i32 %3 , i32* %sum , align 4, !dbg !17
  ret i32 0, !dbg !18 }

define i32 @CalcSum ( i32 %s ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 %s , i32* %1 , align 4
  call void @Inc ( i32* %1 ), !dbg !22
  %2 = load i32* %1 , align 4, !dbg !23
  %3 = add nsw i32 %2 , 9, !dbg !24
  store i32 %3 , i32* %1 , align 4, !dbg !25
  ret i32 %3, !dbg !26 }

define void @Inc ( i32* %x ) {
 ; <label>:0
  %1 = alloca i32* , align 4
  store i32* %x , i32** %1 , align 4
  %2 = load i32* %x , align 4, !dbg !31
  %3 = add nsw i32 %2 , 1, !dbg !32
  store i32 %3 , i32* %x , align 4, !dbg !33
  ret void, !dbg !34 }


*/

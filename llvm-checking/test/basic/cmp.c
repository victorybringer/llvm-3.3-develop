#include <stdio.h> 

int main(int x, int y)
  { 
    int max;
    if (x > y)
     { max = x;
     }
    else 
     { 
       max = y;
     }
    return max;
  }


/*
Now entring the function Max... 
Now entring the function Max... 
  [13]21:  ret i32 %7
	SInfo {_sc = fromList [], _rcMap = fromList [(21,fromList [8])], _defMap = fromList [], _refMap = fromList []}
  []20:  %7 = phi i32 [ [%y, %5], [%x, %4] ]
	SInfo {_sc = fromList [], _rcMap = fromList [(21,fromList [8])], _defMap = fromList [(21,fromList [])], _refMap = fromList [(21,fromList [2,3,5,8,20,21])]}
  []18:  br label %6
	SInfo {_sc = fromList [], _rcMap = fromList [(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(20,fromList []),(21,fromList [])], _refMap = fromList [(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [11]17:  store i32 %y , i32* %max , align 4
	SInfo {_sc = fromList [], _rcMap = fromList [(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [8]15:  br label %6
	SInfo {_sc = fromList [], _rcMap = fromList [(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(20,fromList []),(21,fromList [])], _refMap = fromList [(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [7]14:  store i32 %x , i32* %max , align 4
	SInfo {_sc = fromList [], _rcMap = fromList [(15,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(15,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(15,fromList [15]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [6]12:  br i1 %3 , label %4 , label %5
	SInfo {_sc = fromList [14,17], _rcMap = fromList [(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [6]11:  %3 = icmp sgt i32 %x , %y
	SInfo {_sc = fromList [12,14,17], _rcMap = fromList [(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []10:  store i32 %y , i32* %2 , align 4
	SInfo {_sc = fromList [11,12,14,17], _rcMap = fromList [(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []9:  store i32 %x , i32* %1 , align 4
	SInfo {_sc = fromList [10,11,12,14,17], _rcMap = fromList [(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []8:  %max = alloca i32 , align 4
	SInfo {_sc = fromList [9,10,11,12,14,17], _rcMap = fromList [(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []7:  %2 = alloca i32 , align 4
	SInfo {_sc = fromList [8,9,10,11,12,14,17], _rcMap = fromList [(8,fromList [2,3,5,8,9,10,11,12,14,17]),(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(8,fromList []),(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(8,fromList [5,8]),(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []6:  %1 = alloca i32 , align 4
Leaving from the function Max.
	SInfo {_sc = fromList [7,8,9,10,11,12,14,17], _rcMap = fromList [(7,fromList [2,3,5,7,8,9,10,11,12,14,17]),(8,fromList [2,3,5,8,9,10,11,12,14,17]),(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(7,fromList []),(8,fromList []),(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(7,fromList [5,7]),(8,fromList [5,8]),(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
Now entring the function Max... 
  [13]21:  ret i32 %7
	SInfo {_sc = fromList [], _rcMap = fromList [(21,fromList [8])], _defMap = fromList [], _refMap = fromList []}
  []20:  %7 = phi i32 [ [%y, %5], [%x, %4] ]
	SInfo {_sc = fromList [], _rcMap = fromList [(21,fromList [8])], _defMap = fromList [(21,fromList [])], _refMap = fromList [(21,fromList [2,3,5,8,20,21])]}
  []18:  br label %6
	SInfo {_sc = fromList [], _rcMap = fromList [(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(20,fromList []),(21,fromList [])], _refMap = fromList [(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [11]17:  store i32 %y , i32* %max , align 4
	SInfo {_sc = fromList [], _rcMap = fromList [(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [8]15:  br label %6
	SInfo {_sc = fromList [], _rcMap = fromList [(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(20,fromList []),(21,fromList [])], _refMap = fromList [(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [7]14:  store i32 %x , i32* %max , align 4
	SInfo {_sc = fromList [], _rcMap = fromList [(15,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(15,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(15,fromList [15]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [6]12:  br i1 %3 , label %4 , label %5
	SInfo {_sc = fromList [14,17], _rcMap = fromList [(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  [6]11:  %3 = icmp sgt i32 %x , %y
	SInfo {_sc = fromList [12,14,17], _rcMap = fromList [(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []10:  store i32 %y , i32* %2 , align 4
	SInfo {_sc = fromList [11,12,14,17], _rcMap = fromList [(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []9:  store i32 %x , i32* %1 , align 4
	SInfo {_sc = fromList [10,11,12,14,17], _rcMap = fromList [(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []8:  %max = alloca i32 , align 4
	SInfo {_sc = fromList [9,10,11,12,14,17], _rcMap = fromList [(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []7:  %2 = alloca i32 , align 4
	SInfo {_sc = fromList [8,9,10,11,12,14,17], _rcMap = fromList [(8,fromList [2,3,5,8,9,10,11,12,14,17]),(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(8,fromList []),(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(8,fromList [5,8]),(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
  []6:  %1 = alloca i32 , align 4
Leaving from the function Max.
	SInfo {_sc = fromList [7,8,9,10,11,12,14,17], _rcMap = fromList [(7,fromList [2,3,5,7,8,9,10,11,12,14,17]),(8,fromList [2,3,5,8,9,10,11,12,14,17]),(9,fromList [2,3,9,10,11,12,14,17]),(10,fromList [2,3,10,11,12,14,17]),(11,fromList [2,3,11,12,14,17]),(12,fromList [2,3,11,12,14,17]),(14,fromList [2,14]),(15,fromList [8]),(17,fromList [3,17]),(18,fromList [8]),(20,fromList [8]),(21,fromList [8])], _defMap = fromList [(7,fromList []),(8,fromList []),(9,fromList [6]),(10,fromList [7]),(11,fromList []),(12,fromList []),(14,fromList [8]),(15,fromList []),(17,fromList [8]),(18,fromList []),(20,fromList []),(21,fromList [])], _refMap = fromList [(7,fromList [5,7]),(8,fromList [5,8]),(9,fromList [2,9]),(10,fromList [3,10]),(11,fromList [2,3,11]),(12,fromList [2,3,11,12]),(14,fromList [2,14]),(15,fromList [15]),(17,fromList [3,17]),(18,fromList [18]),(20,fromList [2,3,5,8,20]),(21,fromList [2,3,5,8,20,21])]}
["cmp.c: [6,7,11]"]: 
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  %max = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  store i32 %y , i32* %2 , align 4
  %3 = icmp sgt i32 %x , %y
  br i1 %3 , label %4 , label %5
  store i32 %x , i32* %max , align 4
  store i32 %y , i32* %max , align 4





[(1,define i32 @Max ( i32 %x, i32 %y ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  %max = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  store i32 %y , i32* %2 , align 4
  %3 = icmp sgt i32 %x , %y, !dbg !6
  br i1 %3 , label %4 , label %5, !dbg !7
; <label>:4
  store i32 %x , i32* %max , align 4, !dbg !8
  br label %6, !dbg !9
; <label>:5
  store i32 %y , i32* %max , align 4, !dbg !10
  br label %6
; <label>:6
  %7 = phi i32 [ [%y, %5], [%x, %4] ]
  ret i32 %7, !dbg !11 }),(2,i32 %x),(3,i32 %y),(4,; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  %max = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  store i32 %y , i32* %2 , align 4
  %3 = icmp sgt i32 %x , %y, !dbg !6
  br i1 %3 , label %4 , label %5, !dbg !7),(6,%1 = alloca i32 , align 4),(7,%2 = alloca i32 , align 4),(8,%max = alloca i32 , align 4),(9,store i32 %x , i32* %1 , align 4),(10,store i32 %y , i32* %2 , align 4),(11,%3 = icmp sgt i32 %x , %y),(12,br i1 %3 , label %4 , label %5),(13,; <label>:4
  store i32 %x , i32* %max , align 4, !dbg !8
  br label %6, !dbg !9),(14,store i32 %x , i32* %max , align 4),(15,br label %6),(16,; <label>:5
  store i32 %y , i32* %max , align 4, !dbg !10
  br label %6),(17,store i32 %y , i32* %max , align 4),(18,br label %6),(19,; <label>:6
  %7 = phi i32 [ [%y, %5], [%x, %4] ]
  ret i32 %7, !dbg !11),(20,%7 = phi i32 [ [%y, %5], [%x, %4] ]),(21,ret i32 %7),(22,declare void @llvm.dbg.declare ( metadata, metadata ))]


*/

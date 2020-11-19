#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <list>
using namespace std;
//规则1：指针的偏移越界
void fun3(){
	int a[5]={1,2,3,4,5};
	int *p,i;
	p=a;
	for(i=0;i<=5;i++){
		cout<<*p<<" ";
		p++;
	}
}
//规则2：数组下标访问越界
void fun2(){
	int arr[3]={1,1,1};
	for(int i=0;i<=3;i++)
	{
		cout<<arr[i]<<endl;
	}

}

//规则3：转换结构体前未校验，直接访问域，造成越界
void fun4(){
	int i=3;
typedef struct A
        {   
                int a;
                int b;
                int c;
        } p; 
  p *p1=(struct A *)&i;
  cout<<p1->b<<endl;

}
//规则4:外部数据直接运算后整数溢出，从而导致指针访问越界
int piontAbound(){
    unsigned char ucType = 0;
    unsigned char ucLen = 0;
    char *pulLen = "s";
    char *pucMsg = "m";
    while (ucType != 0)
     {
       ucLen = (*(unsigned char*)(pucMsg + (*pulLen)));
       if (0 == ucLen)
       {
         return -1;
        }
        (*pulLen) += (ucLen * 4 - 1);
        ucType = *((unsigned char*)(pucMsg + (*pulLen)));
    }
    return 0;
}
// 规则5,死循环
int deadLoop(int ulTotalLen,int ucType,int ulLeftDataSize,bool bExitLoop,int ulLeftPktLen,int pucCurPos,int ulLeftLen){
    while(ulTotalLen < ulLeftPktLen){
    /* 溢出为0后，解析从头开始*/
      switch (ucType){
           case 1:
           ulTotalLen += ulLeftDataSize;
           /*ulLeftChunkDataSize外部数据运算前未校验，可能溢出为0*/
              if (ulTotalLen >= ulLeftPktLen) {
                  bExitLoop = true;
                  break;
              }
           pucCurPos += ulLeftDataSize;
           ulLeftLen -= ulLeftDataSize;
      } // switch
            
        if (bExitLoop) {
            break;
        }
    } // while
    return 0;
}
//规则6：除零错，除（/）或模（%）运算的第二个操作数可能为0
int g(int x) {
    x = x - 5;
    int y = 100/x;
    return y;
}

int fn3(int x){
    int y = x - 10;
    int z = 100%y;
    return z;
}

int func3(){
    cout<<g(5)<<endl;
    cout<<fn3(10)<<endl;
    return 0;
}
//规则7：不可信任数据拼接导致SQL注入
char *inputChar(){
    char *str = "1' or 1 == 1#"; //source
    return str;
}

char *inputNum(){
    char *str = "1 or 1 == 1"; //source
    return str;
}

int executeSQL (string sql){
    cout<<sql<<endl;
    return 0;
}

int queryCharType(string input){
    string query = "SELECT * FROM Students WHERE id = '" + input + "'";
    executeSQL(query);
    return 0;
}

int queryNumType(string input){
    string query = "SELECT * FROM Students WHERE id = " + input;
    executeSQL(query);
    return 0;
}

//规则8: 运算过程中可能导致的溢出或反转
int handleNum(){
    unsigned char min = 0;
    unsigned char max = 255;
    min--;
    max++;
    printf("%u",min);
    printf("%u",max);
    return 0;
}
//规则9：空指针解引用
void f1(){
int *p = NULL;
int q = *p;
cout<<q<<endl;
}//直接空指针解引用
void f2(){
int *p;
int *&q=p;
q=NULL;
*p = 5;
}//间接空指针解引用
//规则10：访问已经释放的内存
struct node
{
    int value;
    struct node *next;
};
void free_list(struct node *head)
{
    for (struct node *ptr = head; ptr != NULL; ptr = ptr ->next) /* 【错误】 解引用已经释放的内存 */
    {
        free(ptr);
    }
}
void function2()
{
	char *pa;
	pa = (char*)malloc(sizeof(char)*20);
	if(NULL !=pa)
	{
		strcpy(pa,"hello");
	}
	free(pa);
	printf("pa = %s",pa);//解引用已经释放的内存
	return;
}
//规则11：引用容器前要确保容器元素存在
void NoCompliant(list<const char *> mList)
{
    const char *a = mList.front();
    const char *b = mList.back();
    if (a == NULL && b == NULL) {
        cout <<"the list is not empty"<<endl;
        return;
    }
    cout << a << endl;
}

//规则12，对路径进行规范化
char* inputPath(){
    char *str = "/safe_dir/../important.dat"; //source
    return str;
}

int standardPath(char* path){
    if(remove(path) == 0){
        cout<<"delete success"<<endl;
        return 0;
    }
 cout<<"delete failed"<<endl;
    return 0;
}
int main(){
return 0;
}


/* 
// 静态切片结果：
Backward Static SliceTable:
	#Insts_sliced = 3574.0 (Average: 36)

 Variable      SrcLineNumbers  
------------------------------
 _ZStL8__ioinit                                                                                                {}
 _ZZ4fun2vE3arr                                                                                                {}
 _ZZ4fun3vE1a                                                                                                  {}
 __cur@std::_List_base::int, std::allocator<int>::_M_clear()                                                   {"test2.cpp: [175,177,178,179,180,184]"}
 __len@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_                                                  {"test2.cpp: [115,121]"}
 __lhs@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_                                                  {"test2.cpp: [115,121]"}
 __lhs@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_PKS3_                                                  {"test2.cpp: [115]"}
 __n@__gnu_cxx::new_allocator::std, _List_node<int>::allocate(unsigned long, void const*)                      {"test2.cpp: [177,178]"}
 __p@__gnu_cxx::new_allocator::int::construct(int*, int const&)                                                {"test2.cpp: [175,177,178]"}
 __p@__gnu_cxx::new_allocator::int::destroy(int*)                                                              {"test2.cpp: [175,177,178,179,180,184]"}
 __p@__gnu_cxx::new_allocator::std, _List_node<int>::deallocate(_List_node<int>*, unsigned long)               {"test2.cpp: [175,177,178,179,180,184]"}
 __p@std::_List_base::int, std::allocator<int>::_M_put_node(std*, _List_node<int>)                             {"test2.cpp: [175,177,178,179,180,184]"}
 __p@std::list::int, std::allocator<int>::_M_create_node(int const&)                                           {"test2.cpp: [175,177,178]"}
 __position.coerce@std::list::int, std::allocator<int>::_M_insert(std, _List_iterator<int>, int const&)        {"test2.cpp: [177,178]"}
 __position@std::list::int, std::allocator<int>::_M_insert(std, _List_iterator<int>, int const&)               {"test2.cpp: [175,177,178]"}
 __r@_ZSt11__addressofIiEPT_RS0_                                                                               {"test2.cpp: [175,177,178,179,180,184]"}
 __rhs@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_                                                  {"test2.cpp: [114,115,120,121]"}
 __rhs@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_PKS3_                                                  {"test2.cpp: [115]"}
 __s@std::char_traits::char::length(char const*)                                                               {"test2.cpp: [115,121]"}
 __tmp@std::_List_base::int, std::allocator<int>::_M_clear()                                                   {"test2.cpp: [175,177,178,179,180,184]"}
 __tmp@std::list::int, std::allocator<int>::_M_insert(std, _List_iterator<int>, int const&)                    {"test2.cpp: [175,177,178]"}
 __val@__gnu_cxx::new_allocator::int::construct(int*, int const&)                                              {"test2.cpp: [177,178]"}
 __x@ (std::__detail::_List_node_base*)                                                                        {"test2.cpp: [175,177,178,179,180]"}
 __x@std::_List_iterator::int::operator!=(std::_List_iterator const&) const                                    {"test2.cpp: [175,177,178,179,180]"}
 __x@std::list::int, std::allocator<int>::_M_create_node(int const&)                                           {"test2.cpp: [177,178]"}
 __x@std::list::int, std::allocator<int>::_M_insert(std, _List_iterator<int>, int const&)                      {"test2.cpp: [177,178]"}
 __x@std::list::int, std::allocator<int>::push_back(int const&)                                                {"test2.cpp: [177,178]"}
 a@fun3                                                                                                        {"test2.cpp: [9]"}
 agg.result@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_                                             {"test2.cpp: [115,121]"}
 agg.result@_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_PKS3_                                             {"test2.cpp: [115]"}
 agg.result@std::_List_base::int, std::allocator<int>::_M_get_Tp_allocator() const                             {"test2.cpp: [175,177,178,179,180,184]"}
 arr@fun2                                                                                                      {"test2.cpp: [19]"}
 bExitLoop@deadLoop                                                                                            {}
 head@free_list                                                                                                {"test2.cpp: []"}
 i@fun2                                                                                                        {"test2.cpp: [20]"}
 i@fun3                                                                                                        {"test2.cpp: [12]"}
 i@fun4                                                                                                        {"test2.cpp: [29]"}
 input@queryCharType                                                                                           {}
 input@queryNumType                                                                                            {}
 iter@fun1                                                                                                     {"test2.cpp: [175,176,177,178,179,180]"}
 l1@fun1                                                                                                       {"test2.cpp: [175,177,178,179,180,184]"}
 llvm.global_ctors                                                                                             {}
 max@handleNum                                                                                                 {"test2.cpp: [131]"}
 min@handleNum                                                                                                 {"test2.cpp: [130]"}
 p1@fun4                                                                                                       {"test2.cpp: [29,36]"}
 p@f1                                                                                                          {"test2.cpp: [138]"}
 p@f2                                                                                                          {"test2.cpp: [145]"}
 p@fun3                                                                                                        {"test2.cpp: [9,11,12,14]"}
 pa@function2                                                                                                  {"test2.cpp: [164]"}
 path@standardPath                                                                                             {"test2.cpp: []"}
 ptr@free_list                                                                                                 {"test2.cpp: [156]"}
 pucCurPos@deadLoop                                                                                            {"test2.cpp: [60,62,64,66,67,70,74]"}
 pucMsg@piontAbound                                                                                            {"test2.cpp: [45]"}
 pulLen@piontAbound                                                                                            {"test2.cpp: [44]"}
 q@f1                                                                                                          {"test2.cpp: [139]"}
 q@f2                                                                                                          {"test2.cpp: [144]"}
 query@queryCharType                                                                                           {}
 query@queryNumType                                                                                            {}
 sql@executeSQL                                                                                                {"test2.cpp: [116,122]"}
 str@inputChar                                                                                                 {"test2.cpp: [100]"}
 str@inputNum                                                                                                  {"test2.cpp: [105]"}
 str@inputPath                                                                                                 {"test2.cpp: [187]"}
 this@ ()                                                                                                      {"test2.cpp: [175,176,177,178,179,180,184]"}
 this@ (std::__detail::_List_node_base*)                                                                       {"test2.cpp: [175,177,178,179,180]"}
 this@_ZNSaIiEC1ISt10_List_nodeIiEEERKSaIT_E                                                                   {"test2.cpp: [175,177,178,179,180,184]"}
 this@_ZNSaIiEC2ISt10_List_nodeIiEEERKSaIT_E                                                                   {"test2.cpp: [175,177,178,179,180,184]"}
 this@__gnu_cxx::new_allocator::int::construct(int*, int const&)                                               {"test2.cpp: [177,178]"}
 this@__gnu_cxx::new_allocator::int::destroy(int*)                                                             {"test2.cpp: [175,177,178,179,180,184]"}
 this@__gnu_cxx::new_allocator::std, _List_node<int>::allocate(unsigned long, void const*)                     {"test2.cpp: [175,177,178]"}
 this@__gnu_cxx::new_allocator::std, _List_node<int>::deallocate(_List_node<int>*, unsigned long)              {"test2.cpp: [175,177,178,179,180,184]"}
 this@__gnu_cxx::new_allocator::std, _List_node<int>::max_size() const                                         {"test2.cpp: [175,177,178]"}
 this@std::_List_base::int, std::allocator<int>::_List_impl::_List_impl()                                      {"test2.cpp: [175]"}
 this@std::_List_base::int, std::allocator<int>::_List_impl::~_List_impl()                                     {"test2.cpp: [175,177,178,179,180,184]"}
 this@std::_List_base::int, std::allocator<int>::_M_clear()                                                    {"test2.cpp: [175,177,178,179,180,184]"}
 this@std::_List_base::int, std::allocator<int>::_M_get_Node_allocator() const                                 {"test2.cpp: [175,177,178,179,180,184]"}
 this@std::_List_base::int, std::allocator<int>::_M_get_Tp_allocator() const                                   {"test2.cpp: [175,177,178,179,180,184]"}
 this@std::_List_base::int, std::allocator<int>::_M_get_node()                                                 {"test2.cpp: [175,177,178]"}
 this@std::_List_base::int, std::allocator<int>::_M_init()                                                     {"test2.cpp: [175,177,178,179]"}
 this@std::_List_base::int, std::allocator<int>::_M_put_node(std*, _List_node<int>)                            {"test2.cpp: [175,177,178,179,180,184]"}
 this@std::_List_iterator::int::operator!=(std::_List_iterator const&) const                                   {"test2.cpp: [175,177,178,179,180]"}
 this@std::_List_iterator::int::operator*() const                                                              {"test2.cpp: [175,177,178,179,180,182]"}
 this@std::_List_iterator::int::operator++(int)                                                                {"test2.cpp: [175,177,178,179,180]"}
 this@std::list::int, std::allocator<int>::_M_create_node(int const&)                                          {"test2.cpp: [175,177,178]"}
 this@std::list::int, std::allocator<int>::_M_insert(std, _List_iterator<int>, int const&)                     {"test2.cpp: [175,177,178]"}
 this@std::list::int, std::allocator<int>::begin()                                                             {"test2.cpp: [175,177,178,179,180]"}
 this@std::list::int, std::allocator<int>::clear()                                                             {"test2.cpp: [175,177,178,179]"}
 this@std::list::int, std::allocator<int>::end()                                                               {"test2.cpp: [175,177,178,179,180]"}
 this@std::list::int, std::allocator<int>::push_back(int const&)                                               {"test2.cpp: [175,177,178]"}
 ucLen@piontAbound                                                                                             {"test2.cpp: [43,46,48,49]"}
 ucType@deadLoop                                                                                               {"test2.cpp: []"}
 ucType@piontAbound                                                                                            {"test2.cpp: [42,46,49,54]"}
 ulLeftDataSize@deadLoop                                                                                       {"test2.cpp: []"}
 ulLeftLen@deadLoop                                                                                            {"test2.cpp: [60,62,64,66,67,71,74]"}
 ulLeftPktLen@deadLoop                                                                                         {"test2.cpp: []"}
 ulTotalLen@deadLoop                                                                                           {"test2.cpp: [60,62,64,66,67,74]"}
 x@fn3                                                                                                         {"test2.cpp: [95]"}
 x@g                                                                                                           {"test2.cpp: [82,94]"}
 y@fn3                                                                                                         {"test2.cpp: [88,95]"}
 y@g                                                                                                           {"test2.cpp: [82,83,94]"}
 z@fn3                                                                                                         {"test2.cpp: [88,89,95]"}
*/

// 对应的LLVM IR：
/*  
target datalayout = ""e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128""
target triple = ""x86_64-pc-linux-gnu""


@_ZStL8__ioinit = internal %class.std::ios_base::Init zeroinitializer
@_ZZ4fun3vE1a = private constant [5 x i32] [i32 1, i32 2, i32 3, i32 4, i32 5]
@.str = private constant [2 x i8] [i8 32, i8 0]
@_ZZ4fun2vE3arr = private constant [3 x i32] [i32 1, i32 1, i32 1]
@.str1 = private constant [2 x i8] [i8 115, i8 0]
@.str2 = private constant [2 x i8] [i8 109, i8 0]
@.str3 = private constant [14 x i8] [i8 49, i8 39, i8 32, i8 111, i8 114, i8 32, i8 49, i8 32, i8 61, i8 61, i8 32, i8 49, i8 35, i8 0]
@.str4 = private constant [12 x i8] [i8 49, i8 32, i8 111, i8 114, i8 32, i8 49, i8 32, i8 61, i8 61, i8 32, i8 49, i8 0]
@.str5 = private constant [36 x i8] [i8 83, i8 69, i8 76, i8 69, i8 67, i8 84, i8 32, i8 42, i8 32, i8 70, i8 82, i8 79, i8 77, i8 32, i8 83, i8 116, i8 117, i8 100, i8 101, i8 110, i8 116, i8 115, i8 32, i8 87, i8 72, i8 69, i8 82, i8 69, i8 32, i8 105, i8 100, i8 32, i8 61, i8 32, i8 39, i8 0]
@.str6 = private constant [2 x i8] [i8 39, i8 0]
@.str7 = private constant [35 x i8] [i8 83, i8 69, i8 76, i8 69, i8 67, i8 84, i8 32, i8 42, i8 32, i8 70, i8 82, i8 79, i8 77, i8 32, i8 83, i8 116, i8 117, i8 100, i8 101, i8 110, i8 116, i8 115, i8 32, i8 87, i8 72, i8 69, i8 82, i8 69, i8 32, i8 105, i8 100, i8 32, i8 61, i8 32, i8 0]
@.str8 = private constant [3 x i8] [i8 37, i8 117, i8 0]
@.str9 = private constant [6 x i8] [i8 104, i8 101, i8 108, i8 108, i8 111, i8 0]
@.str10 = private constant [8 x i8] [i8 112, i8 97, i8 32, i8 61, i8 32, i8 37, i8 115, i8 0]
@.str11 = private constant [27 x i8] [i8 47, i8 115, i8 97, i8 102, i8 101, i8 95, i8 100, i8 105, i8 114, i8 47, i8 46, i8 46, i8 47, i8 105, i8 109, i8 112, i8 111, i8 114, i8 116, i8 97, i8 110, i8 116, i8 46, i8 100, i8 97, i8 116, i8 0]
@.str12 = private constant [15 x i8] [i8 100, i8 101, i8 108, i8 101, i8 116, i8 101, i8 32, i8 115, i8 117, i8 99, i8 99, i8 101, i8 115, i8 115, i8 0]
@.str13 = private constant [14 x i8] [i8 100, i8 101, i8 108, i8 101, i8 116, i8 101, i8 32, i8 102, i8 97, i8 105, i8 108, i8 101, i8 100, i8 0]
@llvm.global_ctors = appending [1 x {i32, void()*}] [{i32, void()*} {i32 65535, void() @_GLOBAL__I_a}]

define internal void @__cxx_global_var_init ( ) .text.startup {
 ; <label>:0
  call void @_ZNSt8ios_base4InitC1Ev ( %class.std::ios_base::Init* @_ZStL8__ioinit ), !dbg !5
  %1 = call i32 @__cxa_atexit ( void(i8*)* void(i8*)* bitcast (void(%class.std::ios_base::Init*) @_ZNSt8ios_base4InitD1Ev to void(i8*)*), i8* i8* getelementptr ( %class.std::ios_base::Init* @_ZStL8__ioinit ,  i32 0, i32 0 ), i8* @__dso_handle ), !dbg !6
  ret void, !dbg !7 }

define void @_Z4fun3v ( ) {
 ; <label>:0
  %a = alloca [5 x i32] , align 16
  %p = alloca i32* , align 8
  %i = alloca i32 , align 4
  %1 = bitcast [5 x i32]* %a to i8*, !dbg !10
  call void @llvm.memcpy.p0i8.p0i8.i64 ( i8* %1, i8* i8* bitcast ([5 x i32]* @_ZZ4fun3vE1a to i8*), i64 20, i32 16, i1 0 ), !dbg !11
  %3 = getelementptr inbounds [5 x i32]* %a , i32 0, i32 0, !dbg !12
  store i32* %3 , i32** %p , align 8, !dbg !13
  store i32 0 , i32* %i , align 4, !dbg !14
  br label %4, !dbg !15
; <label>:4
  %5 = phi i32* [ [%13, %8], [%3, %0] ]
  %6 = phi i32 [ [%14, %8], [0, %0] ]
  %7 = icmp sle i32 %6 , 5, !dbg !16
  br i1 %7 , label %8 , label %15, !dbg !17
; <label>:8
  %9 = load i32* %5 , align 4, !dbg !18
  %10 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %9 ), !dbg !19
  %11 = call %class.std::basic_ostream* @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc ( %class.std::basic_ostream* %10, i8* i8* getelementptr ( [2 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !20
  %13 = getelementptr inbounds i32* %5 , i32 1, !dbg !21
  store i32* %13 , i32** %p , align 8, !dbg !22
  %14 = add nsw i32 %6 , 1, !dbg !23
  store i32 %14 , i32* %i , align 4, !dbg !24
  br label %4, !dbg !25
; <label>:15
  ret void, !dbg !26 }

define void @_Z4fun2v ( ) {
 ; <label>:0
  %arr = alloca [3 x i32] , align 4
  %i = alloca i32 , align 4
  %1 = bitcast [3 x i32]* %arr to i8*, !dbg !28
  call void @llvm.memcpy.p0i8.p0i8.i64 ( i8* %1, i8* i8* bitcast ([3 x i32]* @_ZZ4fun2vE3arr to i8*), i64 12, i32 4, i1 0 ), !dbg !29
  store i32 0 , i32* %i , align 4, !dbg !30
  br label %3, !dbg !31
; <label>:3
  %4 = phi i32 [ [%12, %6], [0, %0] ]
  %5 = icmp sle i32 %4 , 3, !dbg !32
  br i1 %5 , label %6 , label %13, !dbg !33
; <label>:6
  %7 = sext i32 %4 to i64, !dbg !34
  %8 = getelementptr inbounds [3 x i32]* %arr , i32 0, i64 %7, !dbg !35
  %9 = load i32* %8 , align 4, !dbg !36
  %10 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %9 ), !dbg !37
  %11 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %10, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !38
  %12 = add nsw i32 %4 , 1, !dbg !39
  store i32 %12 , i32* %i , align 4, !dbg !40
  br label %3, !dbg !41
; <label>:13
  ret void, !dbg !42 }

define void @_Z4fun4v ( ) {
 ; <label>:0
  %i = alloca i32 , align 4
  %p1 = alloca %struct.A* , align 8
  store i32 3 , i32* %i , align 4, !dbg !44
  %1 = bitcast i32* %i to %struct.A*, !dbg !45
  store %struct.A* %1 , %struct.A** %p1 , align 8, !dbg !46
  %2 = getelementptr inbounds %struct.A* %1 , i32 0, i32 1, !dbg !47
  %3 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 undef ), !dbg !48
  %4 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %3, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !49
  ret void, !dbg !50 }

define i32 @_Z11piontAboundv ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %ucType = alloca i8 , align 1
  %ucLen = alloca i8 , align 1
  %pulLen = alloca i8* , align 8
  %pucMsg = alloca i8* , align 8
  store i8 0 , i8* %ucType , align 1, !dbg !55
  store i8 0 , i8* %ucLen , align 1, !dbg !56
  store i8* i8* getelementptr ( [2 x i8]* @.str1 ,  i32 0, i32 0 ) , i8** %pulLen , align 8, !dbg !57
  store i8* i8* getelementptr ( [2 x i8]* @.str2 ,  i32 0, i32 0 ) , i8** %pucMsg , align 8, !dbg !58
  br label %4, !dbg !59
; <label>:4
  br i1 0 , label %5 , label %8, !dbg !60
; <label>:5
  store i8 undef , i8* %ucLen , align 1, !dbg !61
  br i1 -1 , label %6 , label %7, !dbg !62
; <label>:6
  store i32 -1 , i32* %1, !dbg !63
  br label %9, !dbg !64
; <label>:7
  store i8 114 , i8* i8* getelementptr ( [2 x i8]* @.str1 ,  i32 0, i32 0 ) , align 1, !dbg !65
  store i8 undef , i8* %ucType , align 1, !dbg !66
  br label %4, !dbg !67
; <label>:8
  store i32 0 , i32* %1, !dbg !68
  br label %9, !dbg !69
; <label>:9
  %10 = phi i32 [ [0, %8], [-1, %6] ]
  ret i32 %10, !dbg !70 }

define i32 @_Z8deadLoopiiibiii ( i32 %ulTotalLen, i32 %ucType, i32 %ulLeftDataSize, i1 %bExitLoop, i32 %ulLeftPktLen, i32 %pucCurPos, i32 %ulLeftLen ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  %3 = alloca i32 , align 4
  %4 = alloca i8 , align 1
  %5 = alloca i32 , align 4
  %6 = alloca i32 , align 4
  %7 = alloca i32 , align 4
  store i32 %ulTotalLen , i32* %1 , align 4
  store i32 %ucType , i32* %2 , align 4
  store i32 %ulLeftDataSize , i32* %3 , align 4
  %8 = zext i1 %bExitLoop to i8
  store i8 %8 , i8* %4 , align 1
  store i32 %ulLeftPktLen , i32* %5 , align 4
  store i32 %pucCurPos , i32* %6 , align 4
  store i32 %ulLeftLen , i32* %7 , align 4
  br label %9, !dbg !75
; <label>:9
  %10 = phi i32 [ [%24, %30], [%ulLeftLen, %0] ]
  %11 = phi i32 [ [%25, %30], [%pucCurPos, %0] ]
  %12 = phi i8 [ [%26, %30], [%8, %0] ]
  %13 = phi i32 [ [%27, %30], [%ulTotalLen, %0] ], !dbg !76
  %14 = icmp slt i32 %13 , %ulLeftPktLen, !dbg !77
  br i1 %14 , label %15 , label %31, !dbg !78
; <label>:15
  switch i32 %ucType , label %23 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %16 ], !dbg !79
; <label>:16
  %17 = add nsw i32 %13 , %ulLeftDataSize, !dbg !80
  store i32 %17 , i32* %1 , align 4, !dbg !81
  %18 = icmp sge i32 %17 , %ulLeftPktLen, !dbg !82
  br i1 %18 , label %19 , label %20, !dbg !83
; <label>:19
  store i8 1 , i8* %4 , align 1, !dbg !84
  br label %23, !dbg !85
; <label>:20
  %21 = add nsw i32 %11 , %ulLeftDataSize, !dbg !86
  store i32 %21 , i32* %6 , align 4, !dbg !87
  %22 = sub nsw i32 %10 , %ulLeftDataSize, !dbg !88
  store i32 %22 , i32* %7 , align 4, !dbg !89
  br label %23, !dbg !90
; <label>:23
  %24 = phi i32 [ [%22, %20], [%10, %19], [%10, %15] ]
  %25 = phi i32 [ [%21, %20], [%11, %19], [%11, %15] ]
  %26 = phi i8 [ [%12, %20], [1, %19], [%12, %15] ], !dbg !91
  %27 = phi i32 [ [%17, %20], [%17, %19], [%13, %15] ]
  %28 = trunc i8 %26 to i1, !dbg !92
  br i1 %28 , label %29 , label %30, !dbg !93
; <label>:29
  br label %31, !dbg !94
; <label>:30
  br label %9, !dbg !95
; <label>:31
  ret i32 0, !dbg !96 }

define i32 @_Z1gi ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %y = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = sub nsw i32 %x , 5, !dbg !100
  store i32 %2 , i32* %1 , align 4, !dbg !101
  %3 = div i32 100 , %2, !dbg !102
  store i32 %3 , i32* %y , align 4, !dbg !103
  ret i32 %3, !dbg !104 }

define i32 @_Z3fn3i ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %y = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = sub nsw i32 %x , 10, !dbg !106
  store i32 %2 , i32* %y , align 4, !dbg !107
  %3 = rem i32 100 , %2, !dbg !108
  store i32 %3 , i32* %z , align 4, !dbg !109
  ret i32 %3, !dbg !110 }

define i32 @_Z5func3v ( ) {
 ; <label>:0
  %1 = call i32 @_Z1gi ( i32 5 ), !dbg !112
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !113
  %3 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %2, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !114
  %4 = call i32 @_Z3fn3i ( i32 10 ), !dbg !115
  %5 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %4 ), !dbg !116
  %6 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %5, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !117
  ret i32 0, !dbg !118 }

define i8* @_Z9inputCharv ( ) {
 ; <label>:0
  %str = alloca i8* , align 8
  store i8* i8* getelementptr ( [14 x i8]* @.str3 ,  i32 0, i32 0 ) , i8** %str , align 8, !dbg !124
  ret i8* i8* getelementptr ( [14 x i8]* @.str3 ,  i32 0, i32 0 ), !dbg !125 }

define i8* @_Z8inputNumv ( ) {
 ; <label>:0
  %str = alloca i8* , align 8
  store i8* i8* getelementptr ( [12 x i8]* @.str4 ,  i32 0, i32 0 ) , i8** %str , align 8, !dbg !127
  ret i8* i8* getelementptr ( [12 x i8]* @.str4 ,  i32 0, i32 0 ), !dbg !128 }

define i32 @_Z10executeSQLSs ( %class.std::basic_string* %sql ) {
 ; <label>:0
  %1 = call %class.std::basic_ostream* @_ZStlsIcSt11char_traitsIcESaIcEERSt13basic_ostreamIT_T0_ES7_RKSbIS4_S5_T1_E ( %class.std::basic_ostream* @_ZSt4cout, %class.std::basic_string* %sql ), !dbg !533
  %2 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %1, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !534
  ret i32 0, !dbg !535 }

define i32 @_Z13queryCharTypeSs ( %class.std::basic_string* %input ) {
 ; <label>:0
  %query = alloca %class.std::basic_string , align 8
  %1 = alloca %class.std::basic_string , align 8
  %2 = alloca i8*
  %3 = alloca i32
  %4 = alloca %class.std::basic_string , align 8
  %5 = alloca i32
  call void @_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_ ( %class.std::basic_string* %1, i8* i8* getelementptr ( [36 x i8]* @.str5 ,  i32 0, i32 0 ), %class.std::basic_string* %input ), !dbg !538
  invoke void(%class.std::basic_string*, %class.std::basic_string*, i8*) @_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_PKS3_ ( %class.std::basic_string* %query, %class.std::basic_string* %1, i8* i8* getelementptr ( [2 x i8]* @.str6 ,  i32 0, i32 0 ) ) to label %8 unwind label %13, !dbg !539
; <label>:8
  call void @_ZNSsD1Ev ( %class.std::basic_string* %1 ), !dbg !540
  invoke void(%class.std::basic_string*, %class.std::basic_string*) @_ZNSsC1ERKSs ( %class.std::basic_string* %4, %class.std::basic_string* %query ) to label %9 unwind label %19, !dbg !541
; <label>:9
  %10 = invoke i32(%class.std::basic_string*) @_Z10executeSQLSs ( %class.std::basic_string* %4 ) to label %11 unwind label %23, !dbg !542
; <label>:11
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %4 ) to label %12 unwind label %19, !dbg !543
; <label>:12
  store i32 1 , i32* %5
  call void @_ZNSsD1Ev ( %class.std::basic_string* %query ), !dbg !544
  ret i32 0, !dbg !545
; <label>:13
  %14 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !546
  %16 = extractvalue {i8*, i32} %14 0, !dbg !547
  store i8* %16 , i8** %2, !dbg !548
  %17 = extractvalue {i8*, i32} %14 1, !dbg !549
  store i32 %17 , i32* %3, !dbg !550
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %1 ) to label %18 unwind label %37, !dbg !551
; <label>:18
  br label %32, !dbg !552
; <label>:19
  %20 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !553
  %21 = extractvalue {i8*, i32} %20 0, !dbg !554
  store i8* %21 , i8** %2, !dbg !555
  %22 = extractvalue {i8*, i32} %20 1, !dbg !556
  store i32 %22 , i32* %3, !dbg !557
  br label %28, !dbg !558
; <label>:23
  %24 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !559
  %25 = extractvalue {i8*, i32} %24 0, !dbg !560
  store i8* %25 , i8** %2, !dbg !561
  %26 = extractvalue {i8*, i32} %24 1, !dbg !562
  store i32 %26 , i32* %3, !dbg !563
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %4 ) to label %27 unwind label %37, !dbg !564
; <label>:27
  br label %28, !dbg !565
; <label>:28
  %29 = phi i32 [ [%26, %27], [%22, %19] ]
  %30 = phi i8* [ [%25, %27], [%21, %19] ]
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %query ) to label %31 unwind label %37, !dbg !566
; <label>:31
  br label %32, !dbg !567
; <label>:32
  %33 = phi i32 [ [%29, %31], [%17, %18] ]
  %34 = phi i8* [ [%30, %31], [%16, %18] ]
  %35 = insertvalue {i8*, i32} undef , i8* %34 , 0, !dbg !568
  %36 = insertvalue {i8*, i32} %35 , i32 %33 , 1, !dbg !569
  resume {i8*, i32} %36, !dbg !570
; <label>:37
  %38 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !571
  %39 = extractvalue {i8*, i32} %38 0, !dbg !572
  call void @__clang_call_terminate ( i8* %39 ), !dbg !573
  unreachable, !dbg !574 }

define linkonce_odr void @_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_ERKS6_PKS3_ ( %class.std::basic_string* sret noalias %agg.result, %class.std::basic_string* %__lhs, i8* %__rhs ) {
 ; <label>:0
  %1 = alloca %class.std::basic_string* , align 8
  %2 = alloca i8* , align 8
  %3 = alloca i1
  %4 = alloca i8*
  %5 = alloca i32
  %6 = alloca i32
  store %class.std::basic_string* %__lhs , %class.std::basic_string** %1 , align 8
  store i8* %__rhs , i8** %2 , align 8
  store i1 0 , i1* %3, !dbg !581
  call void @_ZNSsC1ERKSs ( %class.std::basic_string* %agg.result, %class.std::basic_string* %__lhs ), !dbg !582
  %7 = invoke %class.std::basic_string*(%class.std::basic_string*, i8*) @_ZNSs6appendEPKc ( %class.std::basic_string* %agg.result, i8* %__rhs ) to label %8 unwind label %9, !dbg !583
; <label>:8
  store i1 -1 , i1* %3, !dbg !584
  store i32 1 , i32* %6
  br i1 -1 , label %14 , label %13, !dbg !585
; <label>:9
  %10 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !586
  %11 = extractvalue {i8*, i32} %10 0, !dbg !587
  store i8* %11 , i8** %4, !dbg !588
  %12 = extractvalue {i8*, i32} %10 1, !dbg !589
  store i32 %12 , i32* %5, !dbg !590
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %agg.result ) to label %15 unwind label %16, !dbg !591
; <label>:13
  call void @_ZNSsD1Ev ( %class.std::basic_string* %agg.result ), !dbg !592
  br label %14, !dbg !593
; <label>:14
  ret void, !dbg !594
; <label>:15
  resume {i8*, i32} %10, !dbg !595
; <label>:16
  %17 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !596
  %18 = extractvalue {i8*, i32} %17 0, !dbg !597
  call void @__clang_call_terminate ( i8* %18 ), !dbg !598
  unreachable, !dbg !599 }

define linkonce_odr void @_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_ ( %class.std::basic_string* sret noalias %agg.result, i8* %__lhs, %class.std::basic_string* %__rhs ) {
 ; <label>:0
  %1 = alloca i8* , align 8
  %2 = alloca %class.std::basic_string* , align 8
  %__len = alloca i64 , align 8
  %3 = alloca i1
  %4 = alloca i8*
  %5 = alloca i32
  %6 = alloca i32
  store i8* %__lhs , i8** %1 , align 8
  store %class.std::basic_string* %__rhs , %class.std::basic_string** %2 , align 8
  %7 = call i64 @_ZNSt11char_traitsIcE6lengthEPKc ( i8* %__lhs ), !dbg !608
  store i64 %7 , i64* %__len , align 8, !dbg !609
  store i1 0 , i1* %3, !dbg !610
  call void @_ZNSsC1Ev ( %class.std::basic_string* %agg.result ), !dbg !611
  %8 = invoke i64(%class.std::basic_string*) @_ZNKSs4sizeEv ( %class.std::basic_string* %__rhs ) to label %9 unwind label %16, !dbg !612
; <label>:9
  %10 = add i64 %7 , %8, !dbg !613
  invoke void(%class.std::basic_string*, i64) @_ZNSs7reserveEm ( %class.std::basic_string* %agg.result, i64 %10 ) to label %11 unwind label %16, !dbg !614
; <label>:11
  %12 = invoke %class.std::basic_string*(%class.std::basic_string*, i8*, i64) @_ZNSs6appendEPKcm ( %class.std::basic_string* %agg.result, i8* %__lhs, i64 %7 ) to label %13 unwind label %16, !dbg !615
; <label>:13
  %14 = invoke %class.std::basic_string*(%class.std::basic_string*, %class.std::basic_string*) @_ZNSs6appendERKSs ( %class.std::basic_string* %agg.result, %class.std::basic_string* %__rhs ) to label %15 unwind label %16, !dbg !616
; <label>:15
  store i1 -1 , i1* %3, !dbg !617
  store i32 1 , i32* %6
  br i1 -1 , label %21 , label %20, !dbg !618
; <label>:16
  %17 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !619
  %18 = extractvalue {i8*, i32} %17 0, !dbg !620
  store i8* %18 , i8** %4, !dbg !621
  %19 = extractvalue {i8*, i32} %17 1, !dbg !622
  store i32 %19 , i32* %5, !dbg !623
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %agg.result ) to label %22 unwind label %23, !dbg !624
; <label>:20
  call void @_ZNSsD1Ev ( %class.std::basic_string* %agg.result ), !dbg !625
  br label %21, !dbg !626
; <label>:21
  ret void, !dbg !627
; <label>:22
  resume {i8*, i32} %17, !dbg !628
; <label>:23
  %24 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !629
  %25 = extractvalue {i8*, i32} %24 0, !dbg !630
  call void @__clang_call_terminate ( i8* %25 ), !dbg !631
  unreachable, !dbg !632 }

define linkonce_odr hidden void @__clang_call_terminate ( i8* % ) {
 ; <label>:0
  %1 = call i8* @__cxa_begin_catch ( i8* % )
  call void @_ZSt9terminatev ( )
  unreachable }

define i32 @_Z12queryNumTypeSs ( %class.std::basic_string* %input ) {
 ; <label>:0
  %query = alloca %class.std::basic_string , align 8
  %1 = alloca %class.std::basic_string , align 8
  %2 = alloca i8*
  %3 = alloca i32
  %4 = alloca i32
  call void @_ZStplIcSt11char_traitsIcESaIcEESbIT_T0_T1_EPKS3_RKS6_ ( %class.std::basic_string* %query, i8* i8* getelementptr ( [35 x i8]* @.str7 ,  i32 0, i32 0 ), %class.std::basic_string* %input ), !dbg !635
  invoke void(%class.std::basic_string*, %class.std::basic_string*) @_ZNSsC1ERKSs ( %class.std::basic_string* %1, %class.std::basic_string* %query ) to label %6 unwind label %10, !dbg !636
; <label>:6
  %7 = invoke i32(%class.std::basic_string*) @_Z10executeSQLSs ( %class.std::basic_string* %1 ) to label %8 unwind label %14, !dbg !637
; <label>:8
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %1 ) to label %9 unwind label %10, !dbg !638
; <label>:9
  store i32 1 , i32* %4
  call void @_ZNSsD1Ev ( %class.std::basic_string* %query ), !dbg !639
  ret i32 0, !dbg !640
; <label>:10
  %11 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !641
  %12 = extractvalue {i8*, i32} %11 0, !dbg !642
  store i8* %12 , i8** %2, !dbg !643
  %13 = extractvalue {i8*, i32} %11 1, !dbg !644
  store i32 %13 , i32* %3, !dbg !645
  br label %19, !dbg !646
; <label>:14
  %15 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !647
  %16 = extractvalue {i8*, i32} %15 0, !dbg !648
  store i8* %16 , i8** %2, !dbg !649
  %17 = extractvalue {i8*, i32} %15 1, !dbg !650
  store i32 %17 , i32* %3, !dbg !651
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %1 ) to label %18 unwind label %25, !dbg !652
; <label>:18
  br label %19, !dbg !653
; <label>:19
  %20 = phi i32 [ [%17, %18], [%13, %10] ]
  %21 = phi i8* [ [%16, %18], [%12, %10] ]
  invoke void(%class.std::basic_string*) @_ZNSsD1Ev ( %class.std::basic_string* %query ) to label %22 unwind label %25, !dbg !654
; <label>:22
  %23 = insertvalue {i8*, i32} undef , i8* %21 , 0, !dbg !655
  %24 = insertvalue {i8*, i32} %23 , i32 %20 , 1, !dbg !656
  resume {i8*, i32} %24, !dbg !657
; <label>:25
  %26 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !658
  %27 = extractvalue {i8*, i32} %26 0, !dbg !659
  call void @__clang_call_terminate ( i8* %27 ), !dbg !660
  unreachable, !dbg !661 }

define i32 @_Z9handleNumv ( ) {
 ; <label>:0
  %min = alloca i8 , align 1
  %max = alloca i8 , align 1
  store i8 0 , i8* %min , align 1, !dbg !663
  store i8 -1 , i8* %max , align 1, !dbg !664
  store i8 -1 , i8* %min , align 1, !dbg !665
  store i8 0 , i8* %max , align 1, !dbg !666
  %1 = call i32 @printf ( i8* i8* getelementptr ( [3 x i8]* @.str8 ,  i32 0, i32 0 ), i32 255 ), !dbg !667
  %3 = call i32 @printf ( i8* i8* getelementptr ( [3 x i8]* @.str8 ,  i32 0, i32 0 ), i32 0 ), !dbg !668
  ret i32 0, !dbg !669 }

define void @_Z2f1v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %q = alloca i32 , align 4
  store i32* null , i32** %p , align 8, !dbg !671
  %1 = load i32* null , align 4, !dbg !672
  store i32 %1 , i32* %q , align 4, !dbg !673
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !674
  %3 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %2, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !675
  ret void, !dbg !676 }

define void @_Z2f2v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %q = alloca i32** , align 8
  store i32** %p , i32*** %q , align 8, !dbg !678
  store i32* null , i32** %p , align 8, !dbg !679
  store i32 5 , i32* null , align 4, !dbg !680
  ret void, !dbg !681 }

define void @_Z9free_listP4node ( %struct.node* %head ) {
 ; <label>:0
  %1 = alloca %struct.node* , align 8
  %ptr = alloca %struct.node* , align 8
  store %struct.node* %head , %struct.node** %1 , align 8
  store %struct.node* %head , %struct.node** %ptr , align 8, !dbg !690
  br label %2, !dbg !691
; <label>:2
  %3 = phi %struct.node* [ [%8, %5], [%head, %0] ]
  %4 = icmp ne %struct.node* %3 , null, !dbg !692
  br i1 %4 , label %5 , label %9, !dbg !693
; <label>:5
  %6 = bitcast %struct.node* %3 to i8*, !dbg !694
  call void @free ( i8* %6 ), !dbg !695
  %7 = getelementptr inbounds %struct.node* %3 , i32 0, i32 1, !dbg !696
  %8 = load %struct.node** %7 , align 8, !dbg !697
  store %struct.node* %8 , %struct.node** %ptr , align 8, !dbg !698
  br label %2, !dbg !699
; <label>:9
  ret void, !dbg !700 }

define void @_Z9function2v ( ) {
 ; <label>:0
  %pa = alloca i8* , align 8
  %1 = call i8* @malloc ( i64 20 ), !dbg !702
  store i8* %1 , i8** %pa , align 8, !dbg !703
  %2 = icmp ne i8* null , %1, !dbg !704
  br i1 %2 , label %3 , label %6, !dbg !705
; <label>:3
  %4 = call i8* @strcpy ( i8* %1, i8* i8* getelementptr ( [6 x i8]* @.str9 ,  i32 0, i32 0 ) ), !dbg !706
  br label %6, !dbg !707
; <label>:6
  call void @free ( i8* %1 ), !dbg !708
  %7 = call i32 @printf ( i8* i8* getelementptr ( [8 x i8]* @.str10 ,  i32 0, i32 0 ), i8* %1 ), !dbg !709
  ret void, !dbg !710 }

define void @_Z4fun1v ( ) {
 ; <label>:0
  %l1 = alloca %class.std::list , align 8
  %iter = alloca %struct.std::_List_iterator , align 8
  %1 = alloca i8*
  %2 = alloca i32
  %3 = alloca i32 , align 4
  %4 = alloca i32 , align 4
  %5 = alloca %struct.std::_List_iterator , align 8
  %6 = alloca %struct.std::_List_iterator , align 8
  %7 = alloca %struct.std::_List_iterator , align 8
  call void @_ZNSt4listIiSaIiEEC1Ev ( %class.std::list* %l1 ), !dbg !712
  invoke void(%struct.std::_List_iterator*) @_ZNSt14_List_iteratorIiEC1Ev ( %struct.std::_List_iterator* %iter ) to label %8 unwind label %34, !dbg !713
; <label>:8
  store i32 1 , i32* %3 , align 4, !dbg !714
  invoke void(%class.std::list*, i32*) @_ZNSt4listIiSaIiEE9push_backERKi ( %class.std::list* %l1, i32* %3 ) to label %9 unwind label %34, !dbg !715
; <label>:9
  store i32 2 , i32* %4 , align 4, !dbg !716
  invoke void(%class.std::list*, i32*) @_ZNSt4listIiSaIiEE9push_backERKi ( %class.std::list* %l1, i32* %4 ) to label %10 unwind label %34, !dbg !717
; <label>:10
  invoke void(%class.std::list*) @_ZNSt4listIiSaIiEE5clearEv ( %class.std::list* %l1 ) to label %11 unwind label %34, !dbg !718
; <label>:11
  %12 = invoke %struct.std::__detail::_List_node_base*(%class.std::list*) @_ZNSt4listIiSaIiEE5beginEv ( %class.std::list* %l1 ) to label %13 unwind label %34, !dbg !719
; <label>:13
  %14 = getelementptr %struct.std::_List_iterator* %5 , i32 0, i32 0, !dbg !720
  store %struct.std::__detail::_List_node_base* %12 , %struct.std::__detail::_List_node_base** %14, !dbg !721
  %15 = bitcast %struct.std::_List_iterator* %iter to i8*, !dbg !722
  %16 = bitcast %struct.std::_List_iterator* %5 to i8*, !dbg !723
  call void @llvm.memcpy.p0i8.p0i8.i64 ( i8* %15, i8* %16, i64 8, i32 8, i1 0 ), !dbg !724
  br label %17, !dbg !725
; <label>:17
  %18 = invoke %struct.std::__detail::_List_node_base*(%class.std::list*) @_ZNSt4listIiSaIiEE3endEv ( %class.std::list* %l1 ) to label %19 unwind label %34, !dbg !726
; <label>:19
  %20 = getelementptr %struct.std::_List_iterator* %6 , i32 0, i32 0, !dbg !727
  store %struct.std::__detail::_List_node_base* %18 , %struct.std::__detail::_List_node_base** %20, !dbg !728
  %21 = invoke i1(%struct.std::_List_iterator*, %struct.std::_List_iterator*) @_ZNKSt14_List_iteratorIiEneERKS0_ ( %struct.std::_List_iterator* %iter, %struct.std::_List_iterator* %6 ) to label %22 unwind label %34, !dbg !729
; <label>:22
  br i1 %21 , label %23 , label %38, !dbg !730
; <label>:23
  %24 = invoke i32*(%struct.std::_List_iterator*) @_ZNKSt14_List_iteratorIiEdeEv ( %struct.std::_List_iterator* %iter ) to label %25 unwind label %34, !dbg !731
; <label>:25
  %26 = load i32* %24, !dbg !732
  %27 = invoke %class.std::basic_ostream*(%class.std::basic_ostream*, i32) @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %26 ) to label %28 unwind label %34, !dbg !733
; <label>:28
  %29 = invoke %class.std::basic_ostream*(%class.std::basic_ostream*, i8*) @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc ( %class.std::basic_ostream* %27, i8* i8* getelementptr ( [2 x i8]* @.str ,  i32 0, i32 0 ) ) to label %30 unwind label %34, !dbg !734
; <label>:30
  %31 = invoke %struct.std::__detail::_List_node_base*(%struct.std::_List_iterator*, i32) @_ZNSt14_List_iteratorIiEppEi ( %struct.std::_List_iterator* %iter, i32 0 ) to label %32 unwind label %34, !dbg !735
; <label>:32
  %33 = getelementptr %struct.std::_List_iterator* %7 , i32 0, i32 0, !dbg !736
  store %struct.std::__detail::_List_node_base* %31 , %struct.std::__detail::_List_node_base** %33, !dbg !737
  br label %17, !dbg !738
; <label>:34
  %35 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !739
  %36 = extractvalue {i8*, i32} %35 0, !dbg !740
  store i8* %36 , i8** %1, !dbg !741
  %37 = extractvalue {i8*, i32} %35 1, !dbg !742
  store i32 %37 , i32* %2, !dbg !743
  invoke void(%class.std::list*) @_ZNSt4listIiSaIiEED1Ev ( %class.std::list* %l1 ) to label %39 unwind label %40, !dbg !744
; <label>:38
  call void @_ZNSt4listIiSaIiEED1Ev ( %class.std::list* %l1 ), !dbg !745
  ret void, !dbg !746
; <label>:39
  resume {i8*, i32} %35, !dbg !747
; <label>:40
  %41 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !748
  %42 = extractvalue {i8*, i32} %41 0, !dbg !749
  call void @__clang_call_terminate ( i8* %42 ), !dbg !750
  unreachable, !dbg !751 }

define linkonce_odr void @_ZNSt4listIiSaIiEEC1Ev ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  call void @_ZNSt4listIiSaIiEEC2Ev ( %class.std::list* %this ), !dbg !1132
  ret void, !dbg !1133 }

define linkonce_odr void @_ZNSt14_List_iteratorIiEC1Ev ( %struct.std::_List_iterator* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  call void @_ZNSt14_List_iteratorIiEC2Ev ( %struct.std::_List_iterator* %this ), !dbg !1135
  ret void, !dbg !1136 }

define linkonce_odr void @_ZNSt4listIiSaIiEE9push_backERKi ( %class.std::list* %this, i32* %__x ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  %2 = alloca i32* , align 8
  %3 = alloca %struct.std::_List_iterator , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  store i32* %__x , i32** %2 , align 8
  %4 = call %struct.std::__detail::_List_node_base* @_ZNSt4listIiSaIiEE3endEv ( %class.std::list* %this ), !dbg !1138
  %5 = getelementptr %struct.std::_List_iterator* %3 , i32 0, i32 0, !dbg !1139
  store %struct.std::__detail::_List_node_base* %4 , %struct.std::__detail::_List_node_base** %5, !dbg !1140
  call void @_ZNSt4listIiSaIiEE9_M_insertESt14_List_iteratorIiERKi ( %class.std::list* %this, %struct.std::__detail::_List_node_base* %4, i32* %__x ), !dbg !1141
  ret void, !dbg !1142 }

define linkonce_odr void @_ZNSt4listIiSaIiEE5clearEv ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  %2 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1144
  call void @_ZNSt10_List_baseIiSaIiEE8_M_clearEv ( %class.std::_List_base* %2 ), !dbg !1145
  call void @_ZNSt10_List_baseIiSaIiEE7_M_initEv ( %class.std::_List_base* %2 ), !dbg !1146
  ret void, !dbg !1147 }

define linkonce_odr %struct.std::__detail::_List_node_base* @_ZNSt4listIiSaIiEE5beginEv ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator , align 8
  %2 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %2 , align 8
  %3 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1149
  %4 = getelementptr inbounds %class.std::_List_base* %3 , i32 0, i32 0, !dbg !1150
  %5 = getelementptr inbounds %struct.std::_List_base<int* %4 , i32 0, i32 0, !dbg !1151
  %6 = getelementptr inbounds %struct.std::__detail::_List_node_base* %5 , i32 0, i32 0, !dbg !1152
  %7 = load %struct.std::__detail::_List_node_base** %6 , align 8, !dbg !1153
  call void @_ZNSt14_List_iteratorIiEC1EPNSt8__detail15_List_node_baseE ( %struct.std::_List_iterator* %1, %struct.std::__detail::_List_node_base* %7 ), !dbg !1154
  %8 = getelementptr %struct.std::_List_iterator* %1 , i32 0, i32 0, !dbg !1155
  %9 = load %struct.std::__detail::_List_node_base** %8, !dbg !1156
  ret %struct.std::__detail::_List_node_base* %9, !dbg !1157 }

define linkonce_odr i1 @_ZNKSt14_List_iteratorIiEneERKS0_ ( %struct.std::_List_iterator* %this, %struct.std::_List_iterator* %__x ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  %2 = alloca %struct.std::_List_iterator* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  store %struct.std::_List_iterator* %__x , %struct.std::_List_iterator** %2 , align 8
  %3 = getelementptr inbounds %struct.std::_List_iterator* %this , i32 0, i32 0, !dbg !1159
  %4 = load %struct.std::__detail::_List_node_base** %3 , align 8, !dbg !1160
  %5 = getelementptr inbounds %struct.std::_List_iterator* %__x , i32 0, i32 0, !dbg !1161
  %6 = load %struct.std::__detail::_List_node_base** %5 , align 8, !dbg !1162
  %7 = icmp ne %struct.std::__detail::_List_node_base* %4 , %6, !dbg !1163
  ret i1 %7, !dbg !1164 }

define linkonce_odr %struct.std::__detail::_List_node_base* @_ZNSt4listIiSaIiEE3endEv ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator , align 8
  %2 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %2 , align 8
  %3 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1166
  %4 = getelementptr inbounds %class.std::_List_base* %3 , i32 0, i32 0, !dbg !1167
  %5 = getelementptr inbounds %struct.std::_List_base<int* %4 , i32 0, i32 0, !dbg !1168
  call void @_ZNSt14_List_iteratorIiEC1EPNSt8__detail15_List_node_baseE ( %struct.std::_List_iterator* %1, %struct.std::__detail::_List_node_base* %5 ), !dbg !1169
  %6 = getelementptr %struct.std::_List_iterator* %1 , i32 0, i32 0, !dbg !1170
  %7 = load %struct.std::__detail::_List_node_base** %6, !dbg !1171
  ret %struct.std::__detail::_List_node_base* %7, !dbg !1172 }

define linkonce_odr i32* @_ZNKSt14_List_iteratorIiEdeEv ( %struct.std::_List_iterator* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  %2 = getelementptr inbounds %struct.std::_List_iterator* %this , i32 0, i32 0, !dbg !1174
  %3 = load %struct.std::__detail::_List_node_base** %2 , align 8, !dbg !1175
  %4 = bitcast %struct.std::__detail::_List_node_base* %3 to %struct.std::_List_node*, !dbg !1176
  %5 = getelementptr inbounds %struct.std::_List_node* %4 , i32 0, i32 1, !dbg !1177
  ret i32* %5, !dbg !1178 }

define linkonce_odr %struct.std::__detail::_List_node_base* @_ZNSt14_List_iteratorIiEppEi ( %struct.std::_List_iterator* %this, i32 % ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator , align 8
  %2 = alloca %struct.std::_List_iterator* , align 8
  %3 = alloca i32 , align 4
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %2 , align 8
  store i32 % , i32* %3 , align 4
  %4 = bitcast %struct.std::_List_iterator* %1 to i8*, !dbg !1180
  %5 = bitcast %struct.std::_List_iterator* %this to i8*, !dbg !1181
  call void @llvm.memcpy.p0i8.p0i8.i64 ( i8* %4, i8* %5, i64 8, i32 8, i1 0 ), !dbg !1182
  %6 = getelementptr inbounds %struct.std::_List_iterator* %this , i32 0, i32 0, !dbg !1183
  %7 = load %struct.std::__detail::_List_node_base** %6 , align 8, !dbg !1184
  %8 = getelementptr inbounds %struct.std::__detail::_List_node_base* %7 , i32 0, i32 0, !dbg !1185
  %9 = load %struct.std::__detail::_List_node_base** %8 , align 8, !dbg !1186
  store %struct.std::__detail::_List_node_base* %9 , %struct.std::__detail::_List_node_base** %6 , align 8, !dbg !1187
  %10 = getelementptr %struct.std::_List_iterator* %1 , i32 0, i32 0, !dbg !1188
  %11 = load %struct.std::__detail::_List_node_base** %10, !dbg !1189
  ret %struct.std::__detail::_List_node_base* %11, !dbg !1190 }

define linkonce_odr void @_ZNSt4listIiSaIiEED1Ev ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  call void @_ZNSt4listIiSaIiEED2Ev ( %class.std::list* %this ), !dbg !1192
  ret void, !dbg !1193 }

define i8* @_Z9inputPathv ( ) {
 ; <label>:0
  %str = alloca i8* , align 8
  store i8* i8* getelementptr ( [27 x i8]* @.str11 ,  i32 0, i32 0 ) , i8** %str , align 8, !dbg !1195
  ret i8* i8* getelementptr ( [27 x i8]* @.str11 ,  i32 0, i32 0 ), !dbg !1196 }

define i32 @_Z12standardPathPc ( i8* %path ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i8* , align 8
  store i8* %path , i8** %2 , align 8
  %3 = call i32 @remove ( i8* %path ), !dbg !1200
  %4 = icmp eq i32 %3 , 0, !dbg !1201
  br i1 %4 , label %5 , label %9, !dbg !1202
; <label>:5
  %6 = call %class.std::basic_ostream* @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc ( %class.std::basic_ostream* @_ZSt4cout, i8* i8* getelementptr ( [15 x i8]* @.str12 ,  i32 0, i32 0 ) ), !dbg !1203
  %8 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %6, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !1204
  store i32 0 , i32* %1, !dbg !1205
  br label %13, !dbg !1206
; <label>:9
  %10 = call %class.std::basic_ostream* @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc ( %class.std::basic_ostream* @_ZSt4cout, i8* i8* getelementptr ( [14 x i8]* @.str13 ,  i32 0, i32 0 ) ), !dbg !1207
  %12 = call %class.std::basic_ostream* @_ZNSolsEPFRSoS_E ( %class.std::basic_ostream* %10, %class.std::basic_ostream*(%class.std::basic_ostream*) @_ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_ ), !dbg !1208
  store i32 0 , i32* %1, !dbg !1209
  br label %13, !dbg !1210
; <label>:13
  ret i32 0, !dbg !1211 }

define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 0 , i32* %1
  ret i32 0, !dbg !1213 }

define linkonce_odr void @_ZNSt14_List_iteratorIiEC1EPNSt8__detail15_List_node_baseE ( %struct.std::_List_iterator* %this, %struct.std::__detail::_List_node_base* %__x ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  %2 = alloca %struct.std::__detail::_List_node_base* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  store %struct.std::__detail::_List_node_base* %__x , %struct.std::__detail::_List_node_base** %2 , align 8
  call void @_ZNSt14_List_iteratorIiEC2EPNSt8__detail15_List_node_baseE ( %struct.std::_List_iterator* %this, %struct.std::__detail::_List_node_base* %__x ), !dbg !1215
  ret void, !dbg !1216 }

define linkonce_odr void @_ZNSt14_List_iteratorIiEC2EPNSt8__detail15_List_node_baseE ( %struct.std::_List_iterator* %this, %struct.std::__detail::_List_node_base* %__x ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  %2 = alloca %struct.std::__detail::_List_node_base* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  store %struct.std::__detail::_List_node_base* %__x , %struct.std::__detail::_List_node_base** %2 , align 8
  %3 = getelementptr inbounds %struct.std::_List_iterator* %this , i32 0, i32 0, !dbg !1218
  store %struct.std::__detail::_List_node_base* %__x , %struct.std::__detail::_List_node_base** %3 , align 8, !dbg !1219
  ret void, !dbg !1220 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE8_M_clearEv ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  %__cur = alloca %struct.std::_List_node* , align 8
  %__tmp = alloca %struct.std::_List_node* , align 8
  %2 = alloca %class.std::allocator , align 1
  %3 = alloca i8*
  %4 = alloca i32
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %5 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1222
  %6 = getelementptr inbounds %struct.std::_List_base<int* %5 , i32 0, i32 0, !dbg !1223
  %7 = getelementptr inbounds %struct.std::__detail::_List_node_base* %6 , i32 0, i32 0, !dbg !1224
  %8 = load %struct.std::__detail::_List_node_base** %7 , align 8, !dbg !1225
  %9 = bitcast %struct.std::__detail::_List_node_base* %8 to %struct.std::_List_node*, !dbg !1226
  store %struct.std::_List_node* %9 , %struct.std::_List_node** %__cur , align 8, !dbg !1227
  br label %10, !dbg !1228
; <label>:10
  %11 = phi %struct.std::_List_node* [ [%17, %22], [%9, %0] ]
  %12 = bitcast %struct.std::_List_node* %11 to %struct.std::__detail::_List_node_base*, !dbg !1229
  %13 = icmp ne %struct.std::__detail::_List_node_base* %12 , %6, !dbg !1230
  br i1 %13 , label %14 , label %27, !dbg !1231
; <label>:14
  store %struct.std::_List_node* %11 , %struct.std::_List_node** %__tmp , align 8, !dbg !1232
  %15 = getelementptr inbounds %struct.std::__detail::_List_node_base* %12 , i32 0, i32 0, !dbg !1233
  %16 = load %struct.std::__detail::_List_node_base** %15 , align 8, !dbg !1234
  %17 = bitcast %struct.std::__detail::_List_node_base* %16 to %struct.std::_List_node*, !dbg !1235
  store %struct.std::_List_node* %17 , %struct.std::_List_node** %__cur , align 8, !dbg !1236
  call void @_ZNKSt10_List_baseIiSaIiEE19_M_get_Tp_allocatorEv ( %class.std::allocator* %2, %class.std::_List_base* %this ), !dbg !1237
  %18 = bitcast %class.std::allocator* %2 to %class.__gnu_cxx::new_allocator*, !dbg !1238
  %19 = getelementptr inbounds %struct.std::_List_node* %11 , i32 0, i32 1, !dbg !1239
  %20 = invoke i32*(i32*) @_ZSt11__addressofIiEPT_RS0_ ( i32* %19 ) to label %21 unwind label %23, !dbg !1240
; <label>:21
  invoke void(%class.__gnu_cxx::new_allocator*, i32*) @_ZN9__gnu_cxx13new_allocatorIiE7destroyEPi ( %class.__gnu_cxx::new_allocator* %18, i32* %20 ) to label %22 unwind label %23, !dbg !1241
; <label>:22
  call void @_ZNSaIiED1Ev ( %class.std::allocator* %2 ), !dbg !1242
  call void @_ZNSt10_List_baseIiSaIiEE11_M_put_nodeEPSt10_List_nodeIiE ( %class.std::_List_base* %this, %struct.std::_List_node* %11 ), !dbg !1243
  br label %10, !dbg !1244
; <label>:23
  %24 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !1245
  %25 = extractvalue {i8*, i32} %24 0, !dbg !1246
  store i8* %25 , i8** %3, !dbg !1247
  %26 = extractvalue {i8*, i32} %24 1, !dbg !1248
  store i32 %26 , i32* %4, !dbg !1249
  call void @_ZNSaIiED1Ev ( %class.std::allocator* %2 ), !dbg !1250
  resume {i8*, i32} %24, !dbg !1251
; <label>:27
  ret void, !dbg !1252 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE7_M_initEv ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %2 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1254
  %3 = getelementptr inbounds %struct.std::_List_base<int* %2 , i32 0, i32 0, !dbg !1255
  %4 = getelementptr inbounds %struct.std::__detail::_List_node_base* %3 , i32 0, i32 0, !dbg !1256
  store %struct.std::__detail::_List_node_base* %3 , %struct.std::__detail::_List_node_base** %4 , align 8, !dbg !1257
  %5 = getelementptr inbounds %struct.std::__detail::_List_node_base* %3 , i32 0, i32 1, !dbg !1258
  store %struct.std::__detail::_List_node_base* %3 , %struct.std::__detail::_List_node_base** %5 , align 8, !dbg !1259
  ret void, !dbg !1260 }

define linkonce_odr void @_ZNKSt10_List_baseIiSaIiEE19_M_get_Tp_allocatorEv ( %class.std::allocator* sret noalias %agg.result, %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %2 = call %class.std::allocator* @_ZNKSt10_List_baseIiSaIiEE21_M_get_Node_allocatorEv ( %class.std::_List_base* %this ), !dbg !1262
  call void @_ZNSaIiEC1ISt10_List_nodeIiEEERKSaIT_E ( %class.std::allocator* %agg.result, %class.std::allocator* %2 ), !dbg !1263
  ret void, !dbg !1264 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorIiE7destroyEPi ( %class.__gnu_cxx::new_allocator* %this, i32* %__p ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  %2 = alloca i32* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  store i32* %__p , i32** %2 , align 8
  ret void, !dbg !1266 }

define linkonce_odr i32* @_ZSt11__addressofIiEPT_RS0_ ( i32* %__r ) {
 ; <label>:0
  %1 = alloca i32* , align 8
  store i32* %__r , i32** %1 , align 8
  %2 = bitcast i32* %__r to i8*, !dbg !1271
  %3 = bitcast i8* %2 to i32*, !dbg !1272
  ret i32* %3, !dbg !1273 }

define linkonce_odr void @_ZNSaIiED1Ev ( %class.std::allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  call void @_ZNSaIiED2Ev ( %class.std::allocator* %this ), !dbg !1275
  ret void, !dbg !1276 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE11_M_put_nodeEPSt10_List_nodeIiE ( %class.std::_List_base* %this, %struct.std::_List_node* %__p ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  %2 = alloca %struct.std::_List_node* , align 8
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  store %struct.std::_List_node* %__p , %struct.std::_List_node** %2 , align 8
  %3 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1278
  %4 = bitcast %struct.std::_List_base<int* %3 to %class.std::allocator*, !dbg !1279
  %5 = bitcast %class.std::allocator* %4 to %class.__gnu_cxx::new_allocator*, !dbg !1280
  call void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEE10deallocateEPS2_m ( %class.__gnu_cxx::new_allocator* %5, %struct.std::_List_node* %__p, i64 1 ), !dbg !1281
  ret void, !dbg !1282 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEE10deallocateEPS2_m ( %class.__gnu_cxx::new_allocator* %this, %struct.std::_List_node* %__p, i64 % ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  %2 = alloca %struct.std::_List_node* , align 8
  %3 = alloca i64 , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  store %struct.std::_List_node* %__p , %struct.std::_List_node** %2 , align 8
  store i64 % , i64* %3 , align 8
  %4 = bitcast %struct.std::_List_node* %__p to i8*, !dbg !1284
  call void @_ZdlPv ( i8* %4 ), !dbg !1285
  ret void, !dbg !1286 }

define linkonce_odr void @_ZNSaIiED2Ev ( %class.std::allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  %2 = bitcast %class.std::allocator* %this to %class.__gnu_cxx::new_allocator*, !dbg !1288
  call void @_ZN9__gnu_cxx13new_allocatorIiED2Ev ( %class.__gnu_cxx::new_allocator* %2 ), !dbg !1289
  ret void, !dbg !1290 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorIiED2Ev ( %class.__gnu_cxx::new_allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  ret void, !dbg !1292 }

define linkonce_odr void @_ZNSaIiEC1ISt10_List_nodeIiEEERKSaIT_E ( %class.std::allocator* %this, %class.std::allocator* % ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  %2 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  store %class.std::allocator* % , %class.std::allocator** %2 , align 8
  call void @_ZNSaIiEC2ISt10_List_nodeIiEEERKSaIT_E ( %class.std::allocator* %this, %class.std::allocator* % ), !dbg !1296
  ret void, !dbg !1297 }

define linkonce_odr %class.std::allocator* @_ZNKSt10_List_baseIiSaIiEE21_M_get_Node_allocatorEv ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %2 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1299
  %3 = bitcast %struct.std::_List_base<int* %2 to %class.std::allocator*, !dbg !1300
  ret %class.std::allocator* %3, !dbg !1301 }

define linkonce_odr void @_ZNSaIiEC2ISt10_List_nodeIiEEERKSaIT_E ( %class.std::allocator* %this, %class.std::allocator* % ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  %2 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  store %class.std::allocator* % , %class.std::allocator** %2 , align 8
  %3 = bitcast %class.std::allocator* %this to %class.__gnu_cxx::new_allocator*, !dbg !1303
  call void @_ZN9__gnu_cxx13new_allocatorIiEC2Ev ( %class.__gnu_cxx::new_allocator* %3 ), !dbg !1304
  ret void, !dbg !1305 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorIiEC2Ev ( %class.__gnu_cxx::new_allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  ret void, !dbg !1307 }

define linkonce_odr void @_ZNSt4listIiSaIiEE9_M_insertESt14_List_iteratorIiERKi ( %class.std::list* %this, %struct.std::__detail::_List_node_base* %__position.coerce, i32* %__x ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  %__position = alloca %struct.std::_List_iterator , align 8
  %2 = alloca i32* , align 8
  %__tmp = alloca %struct.std::_List_node* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  %3 = getelementptr %struct.std::_List_iterator* %__position , i32 0, i32 0
  store %struct.std::__detail::_List_node_base* %__position.coerce , %struct.std::__detail::_List_node_base** %3
  store i32* %__x , i32** %2 , align 8
  %4 = call %struct.std::_List_node* @_ZNSt4listIiSaIiEE14_M_create_nodeERKi ( %class.std::list* %this, i32* %__x ), !dbg !1309
  store %struct.std::_List_node* %4 , %struct.std::_List_node** %__tmp , align 8, !dbg !1310
  %5 = bitcast %struct.std::_List_node* %4 to %struct.std::__detail::_List_node_base*, !dbg !1311
  call void @_ZNSt8__detail15_List_node_base7_M_hookEPS0_ ( %struct.std::__detail::_List_node_base* %5, %struct.std::__detail::_List_node_base* %__position.coerce ), !dbg !1312
  ret void, !dbg !1313 }

define linkonce_odr %struct.std::_List_node* @_ZNSt4listIiSaIiEE14_M_create_nodeERKi ( %class.std::list* %this, i32* %__x ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  %2 = alloca i32* , align 8
  %__p = alloca %struct.std::_List_node* , align 8
  %3 = alloca %class.std::allocator , align 1
  %4 = alloca i8*
  %5 = alloca i32
  store %class.std::list* %this , %class.std::list** %1 , align 8
  store i32* %__x , i32** %2 , align 8
  %6 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1315
  %7 = call %struct.std::_List_node* @_ZNSt10_List_baseIiSaIiEE11_M_get_nodeEv ( %class.std::_List_base* %6 ), !dbg !1316
  store %struct.std::_List_node* %7 , %struct.std::_List_node** %__p , align 8, !dbg !1317
  invoke void(%class.std::allocator*, %class.std::_List_base*) @_ZNKSt10_List_baseIiSaIiEE19_M_get_Tp_allocatorEv ( %class.std::allocator* %3, %class.std::_List_base* %6 ) to label %8 unwind label %14, !dbg !1318
; <label>:8
  %9 = bitcast %class.std::allocator* %3 to %class.__gnu_cxx::new_allocator*, !dbg !1319
  %10 = getelementptr inbounds %struct.std::_List_node* %7 , i32 0, i32 1, !dbg !1320
  %11 = invoke i32*(i32*) @_ZSt11__addressofIiEPT_RS0_ ( i32* %10 ) to label %12 unwind label %18, !dbg !1321
; <label>:12
  invoke void(%class.__gnu_cxx::new_allocator*, i32*, i32*) @_ZN9__gnu_cxx13new_allocatorIiE9constructEPiRKi ( %class.__gnu_cxx::new_allocator* %9, i32* %11, i32* %__x ) to label %13 unwind label %18, !dbg !1322
; <label>:13
  call void @_ZNSaIiED1Ev ( %class.std::allocator* %3 ), !dbg !1323
  ret %struct.std::_List_node* %7, !dbg !1324
; <label>:14
  %15 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !1325
  %16 = extractvalue {i8*, i32} %15 0, !dbg !1326
  store i8* %16 , i8** %4, !dbg !1327
  %17 = extractvalue {i8*, i32} %15 1, !dbg !1328
  store i32 %17 , i32* %5, !dbg !1329
  br label %22, !dbg !1330
; <label>:18
  %19 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !1331
  %20 = extractvalue {i8*, i32} %19 0, !dbg !1332
  store i8* %20 , i8** %4, !dbg !1333
  %21 = extractvalue {i8*, i32} %19 1, !dbg !1334
  store i32 %21 , i32* %5, !dbg !1335
  call void @_ZNSaIiED1Ev ( %class.std::allocator* %3 ), !dbg !1336
  br label %22, !dbg !1337
; <label>:22
  %23 = phi i8* [ [%20, %18], [%16, %14] ]
  %24 = call i8* @__cxa_begin_catch ( i8* %23 ), !dbg !1338
  invoke void(%class.std::_List_base*, %struct.std::_List_node*) @_ZNSt10_List_baseIiSaIiEE11_M_put_nodeEPSt10_List_nodeIiE ( %class.std::_List_base* %6, %struct.std::_List_node* %7 ) to label %25 unwind label %26, !dbg !1339
; <label>:25
  invoke void() @__cxa_rethrow ( ) to label %34 unwind label %26, !dbg !1340
; <label>:26
  %27 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !1341
  %28 = extractvalue {i8*, i32} %27 0, !dbg !1342
  store i8* %28 , i8** %4, !dbg !1343
  %29 = extractvalue {i8*, i32} %27 1, !dbg !1344
  store i32 %29 , i32* %5, !dbg !1345
  invoke void() @__cxa_end_catch ( ) to label %30 unwind label %31, !dbg !1346
; <label>:30
  resume {i8*, i32} %27, !dbg !1347
; <label>:31
  %32 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) catch i8* null, !dbg !1348
  %33 = extractvalue {i8*, i32} %32 0, !dbg !1349
  call void @__clang_call_terminate ( i8* %33 ), !dbg !1350
  unreachable, !dbg !1351
; <label>:34
  unreachable }

define linkonce_odr %struct.std::_List_node* @_ZNSt10_List_baseIiSaIiEE11_M_get_nodeEv ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %2 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1353
  %3 = bitcast %struct.std::_List_base<int* %2 to %class.std::allocator*, !dbg !1354
  %4 = bitcast %class.std::allocator* %3 to %class.__gnu_cxx::new_allocator*, !dbg !1355
  %5 = call %struct.std::_List_node* @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEE8allocateEmPKv ( %class.__gnu_cxx::new_allocator* %4, i64 1, i8* null ), !dbg !1356
  ret %struct.std::_List_node* %5, !dbg !1357 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorIiE9constructEPiRKi ( %class.__gnu_cxx::new_allocator* %this, i32* %__p, i32* %__val ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  %2 = alloca i32* , align 8
  %3 = alloca i32* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  store i32* %__p , i32** %2 , align 8
  store i32* %__val , i32** %3 , align 8
  %4 = bitcast i32* %__p to i8*, !dbg !1359
  %5 = icmp eq i8* %4 , null, !dbg !1360
  br i1 %5 , label %9 , label %6, !dbg !1361
; <label>:6
  %7 = bitcast i8* %4 to i32*, !dbg !1362
  %8 = load i32* %__val , align 4, !dbg !1363
  store i32 %8 , i32* %7 , align 4, !dbg !1364
  br label %9, !dbg !1365
; <label>:9
  %10 = phi i32* [ [%7, %6], [null, %0] ], !dbg !1366
  ret void, !dbg !1367 }

define linkonce_odr %struct.std::_List_node* @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEE8allocateEmPKv ( %class.__gnu_cxx::new_allocator* %this, i64 %__n, i8* % ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  %2 = alloca i64 , align 8
  %3 = alloca i8* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  store i64 %__n , i64* %2 , align 8
  store i8* % , i8** %3 , align 8
  %4 = call i64 @_ZNK9__gnu_cxx13new_allocatorISt10_List_nodeIiEE8max_sizeEv ( %class.__gnu_cxx::new_allocator* %this ), !dbg !1369
  %5 = icmp ugt i64 %__n , %4, !dbg !1370
  br i1 %5 , label %6 , label %7, !dbg !1371
; <label>:6
  call void @_ZSt17__throw_bad_allocv ( ), !dbg !1372
  unreachable, !dbg !1373
; <label>:7
  %8 = mul i64 %__n , 24, !dbg !1374
  %9 = call i8* @_Znwm ( i64 %8 ), !dbg !1375
  %10 = bitcast i8* %9 to %struct.std::_List_node*, !dbg !1376
  ret %struct.std::_List_node* %10, !dbg !1377 }

define linkonce_odr i64 @_ZNK9__gnu_cxx13new_allocatorISt10_List_nodeIiEE8max_sizeEv ( %class.__gnu_cxx::new_allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  ret i64 768614336404564650, !dbg !1379 }

define linkonce_odr void @_ZNSt14_List_iteratorIiEC2Ev ( %struct.std::_List_iterator* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_iterator* , align 8
  store %struct.std::_List_iterator* %this , %struct.std::_List_iterator** %1 , align 8
  %2 = getelementptr inbounds %struct.std::_List_iterator* %this , i32 0, i32 0, !dbg !1381
  store %struct.std::__detail::_List_node_base* null , %struct.std::__detail::_List_node_base** %2 , align 8, !dbg !1382
  ret void, !dbg !1383 }

define linkonce_odr void @_ZNSt4listIiSaIiEEC2Ev ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  %2 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1385
  call void @_ZNSt10_List_baseIiSaIiEEC2Ev ( %class.std::_List_base* %2 ), !dbg !1386
  ret void, !dbg !1387 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEEC2Ev ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  %2 = alloca i8*
  %3 = alloca i32
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  %4 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1389
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implC1Ev ( %struct.std::_List_base<int* %4 ), !dbg !1390
  invoke void(%class.std::_List_base*) @_ZNSt10_List_baseIiSaIiEE7_M_initEv ( %class.std::_List_base* %this ) to label %5 unwind label %6, !dbg !1391
; <label>:5
  ret void, !dbg !1392
; <label>:6
  %7 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !1393
  %8 = extractvalue {i8*, i32} %7 0, !dbg !1394
  store i8* %8 , i8** %2, !dbg !1395
  %9 = extractvalue {i8*, i32} %7 1, !dbg !1396
  store i32 %9 , i32* %3, !dbg !1397
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implD1Ev ( %struct.std::_List_base<int* %4 ), !dbg !1398
  resume {i8*, i32} %7, !dbg !1399 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE10_List_implC1Ev ( %struct.std::_List_base<int* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_base<int* , align 8
  store %struct.std::_List_base<int* %this , %struct.std::_List_base<int** %1 , align 8
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implC2Ev ( %struct.std::_List_base<int* %this ), !dbg !1401
  ret void, !dbg !1402 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE10_List_implD1Ev ( %struct.std::_List_base<int* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_base<int* , align 8
  store %struct.std::_List_base<int* %this , %struct.std::_List_base<int** %1 , align 8
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implD2Ev ( %struct.std::_List_base<int* %this ), !dbg !1404
  ret void, !dbg !1405 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE10_List_implD2Ev ( %struct.std::_List_base<int* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_base<int* , align 8
  store %struct.std::_List_base<int* %this , %struct.std::_List_base<int** %1 , align 8
  %2 = bitcast %struct.std::_List_base<int* %this to %class.std::allocator*, !dbg !1407
  call void @_ZNSaISt10_List_nodeIiEED2Ev ( %class.std::allocator* %2 ), !dbg !1408
  ret void, !dbg !1409 }

define linkonce_odr void @_ZNSaISt10_List_nodeIiEED2Ev ( %class.std::allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  %2 = bitcast %class.std::allocator* %this to %class.__gnu_cxx::new_allocator*, !dbg !1411
  call void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEED2Ev ( %class.__gnu_cxx::new_allocator* %2 ), !dbg !1412
  ret void, !dbg !1413 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEED2Ev ( %class.__gnu_cxx::new_allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  ret void, !dbg !1415 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEE10_List_implC2Ev ( %struct.std::_List_base<int* %this ) {
 ; <label>:0
  %1 = alloca %struct.std::_List_base<int* , align 8
  store %struct.std::_List_base<int* %this , %struct.std::_List_base<int** %1 , align 8
  %2 = bitcast %struct.std::_List_base<int* %this to %class.std::allocator*, !dbg !1417
  call void @_ZNSaISt10_List_nodeIiEEC2Ev ( %class.std::allocator* %2 ), !dbg !1418
  %3 = getelementptr inbounds %struct.std::_List_base<int* %this , i32 0, i32 0, !dbg !1419
  %4 = bitcast %struct.std::__detail::_List_node_base* %3 to i8*, !dbg !1420
  call void @llvm.memset.p0i8.i64 ( i8* %4, i8 0, i64 16, i32 8, i1 0 ), !dbg !1421
  ret void, !dbg !1422 }

define linkonce_odr void @_ZNSaISt10_List_nodeIiEEC2Ev ( %class.std::allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.std::allocator* , align 8
  store %class.std::allocator* %this , %class.std::allocator** %1 , align 8
  %2 = bitcast %class.std::allocator* %this to %class.__gnu_cxx::new_allocator*, !dbg !1424
  call void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEEC2Ev ( %class.__gnu_cxx::new_allocator* %2 ), !dbg !1425
  ret void, !dbg !1426 }

define linkonce_odr void @_ZN9__gnu_cxx13new_allocatorISt10_List_nodeIiEEC2Ev ( %class.__gnu_cxx::new_allocator* %this ) {
 ; <label>:0
  %1 = alloca %class.__gnu_cxx::new_allocator* , align 8
  store %class.__gnu_cxx::new_allocator* %this , %class.__gnu_cxx::new_allocator** %1 , align 8
  ret void, !dbg !1428 }

define linkonce_odr i64 @_ZNSt11char_traitsIcE6lengthEPKc ( i8* %__s ) {
 ; <label>:0
  %1 = alloca i8* , align 8
  store i8* %__s , i8** %1 , align 8
  %2 = call i64 @strlen ( i8* %__s ), !dbg !1481
  ret i64 %2, !dbg !1482 }

define linkonce_odr void @_ZNSt4listIiSaIiEED2Ev ( %class.std::list* %this ) {
 ; <label>:0
  %1 = alloca %class.std::list* , align 8
  store %class.std::list* %this , %class.std::list** %1 , align 8
  %2 = bitcast %class.std::list* %this to %class.std::_List_base*, !dbg !1484
  call void @_ZNSt10_List_baseIiSaIiEED2Ev ( %class.std::_List_base* %2 ), !dbg !1485
  ret void, !dbg !1486 }

define linkonce_odr void @_ZNSt10_List_baseIiSaIiEED2Ev ( %class.std::_List_base* %this ) {
 ; <label>:0
  %1 = alloca %class.std::_List_base* , align 8
  %2 = alloca i8*
  %3 = alloca i32
  store %class.std::_List_base* %this , %class.std::_List_base** %1 , align 8
  invoke void(%class.std::_List_base*) @_ZNSt10_List_baseIiSaIiEE8_M_clearEv ( %class.std::_List_base* %this ) to label %4 unwind label %6, !dbg !1488
; <label>:4
  %5 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1489
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implD1Ev ( %struct.std::_List_base<int* %5 ), !dbg !1490
  ret void, !dbg !1491
; <label>:6
  %7 = landingpad {i8*, i32} personality i8* i8* bitcast (i32(, ...) @__gxx_personality_v0 to i8*) cleanup, !dbg !1492
  %8 = extractvalue {i8*, i32} %7 0, !dbg !1493
  store i8* %8 , i8** %2, !dbg !1494
  %9 = extractvalue {i8*, i32} %7 1, !dbg !1495
  store i32 %9 , i32* %3, !dbg !1496
  %10 = getelementptr inbounds %class.std::_List_base* %this , i32 0, i32 0, !dbg !1497
  call void @_ZNSt10_List_baseIiSaIiEE10_List_implD1Ev ( %struct.std::_List_base<int* %10 ), !dbg !1498
  resume {i8*, i32} %7, !dbg !1499 }

define internal void @_GLOBAL__I_a ( ) .text.startup {
 ; <label>:0
  call void @__cxx_global_var_init ( ), !dbg !1502
  ret void, !dbg !1503 }

 */

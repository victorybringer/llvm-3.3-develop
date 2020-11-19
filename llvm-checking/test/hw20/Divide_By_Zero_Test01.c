/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2019-2020. All rights reserved.
 *
 * @description 除零错误
 *
 * @good GoodCase01;GoodCase02;GoodCase03;GoodCase04;GoodCase05;GoodCase06
 *
 * @bad BadCase01;BadCase02;BadCase03;BadCase04;BadCase05;BadCase06;BadCase07;BadCase08
 *
 * @label Coverity:DIVIDE_BY_ZERO;SecBrella:SecA_DivideByZero
 *
 * @author c00297271
 *
 * @date 2020-06-01
 *
 */


int cond01(int p1)
{
    if (p1 > 5) {
        return 1;
    } else {
        return 0;
    }
}

// @scene 路径敏感， if 条件不成立时，出现除零错误
int BadCase01(p1)
{
    int x = 0;
    if (cond01(p1)) {
        x = 1;
    }
    // POTENTIAL FLAW: 除零错误
    return 1 / x;
}


int foo02(int y)
{
    if (y < 0) {
        return 0;
    }
    return y;
}

// @scene 函数返回值做除数，并且该函数的返回值可能为零。         
void BadCase02(int y)
{
    // POTENTIAL FLAW: 除零错误
    int z = 1 / foo02(y);
}

// @scene 来自入参的除数可能为零         
void BadCase03(int y)
{
    // POTENTIAL FLAW: 除零错误
    int z = 1 / y;
}

void Outer01() {
	int p1 = 6;
	int y = 1;
    if(p1 > 5)  
        y = 0; 
	
	// y 值做参数，调用 BadCase03
	BadCase03(y);
}


#define SUB_FILE_MICRO(NUM) ((NUM) - 5)

// @scene 宏展开后的表达式返回值做除数，并且该返回值可能为零。
float BadCase04(int num)
{
    // POTENTIAL FLAW: 除零错误
    float ret = num / SUB_FILE_MICRO(5);
    return ret;
}


void GoodCase01(int actCnt, float lossTot)
{
    int i;
    for (i = 0; i < actCnt; i++) {
    }

    float lossAvg;
    if (i != 0) {
        lossAvg = lossTot / i;//good
    } else {
        lossAvg = 0;
    }
}


// @scene 除数来自外部输入，可能会出现除零错误。
void BadCase05()
{
    int a;
    scanf("%d", &a);
    // POTENTIAL FLAW: 除零错误
    int z = 1 / a;  // 目前clang不跟踪污点所以漏报
}

// @scene 除数来自外部输入，虽然经过条件判断，仍有可能为0
void BadCase06(int b)
{

    if (b < 10) {
        // POTENTIAL FLAW: 除零错误
        int z = 1 / b;
    }

}



typedef double double_t;
// @scene 除数为 chart、int 、long或者其他整形类型，且值等于零
// 
void BadCase07(int flag)
{
    char i1 = 0;
    int i2 = 0;
    unsigned long long i3 = 0;

    float i4 = 0.0;
    double_t i5 = 0.0;

    double valueA = 1.0;
    double valueC = 0.0;

    if (flag == 0) {
        // POTENTIAL FLAW: 除零错误
        100 / i1;
    } else if (flag == 10) {
        // POTENTIAL FLAW: 除零错误
        100 / i2;
    } else if (flag == 100) {
        // POTENTIAL FLAW: 除零错误
        100 / i3;
    } else if (flag == 1000) {
        // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
        100 / i4;
    } else if (flag == 1001) {
        // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
        100 / i5;
    } else if (flag == 1002) {
        // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
        valueC = valueA / i5;
    } else if (flag == 1003) {
        // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
        valueA / i5;
    }

}

//https://secsolar-szv.clouddragon.huawei.com/portal/workspace/projectDefectsView?projectName=UAC3000_V500R020C10_SecBrella_acu&cid=211533
//误报
//工具无法判断浮点型当前取值范围，无法优化
#define  ZERO_DOUBLE                0.0000000001
void GoodCase02(double rateo1, double rateo2, double meterval_o, int flag)
{
    double rateo = 0.0, rated = 0.0;

    if (flag == 0) {
        rateo = rateo1;
        rated = rateo2;
    }

    if (rateo < ZERO_DOUBLE || rated < ZERO_DOUBLE) {
        return;
    }

    double retl = meterval_o * rateo / rated;//good
}


void GoodCase03(int lo)
{
    int numEntries, totalProbe;
    numEntries = totalProbe = 0;

    for (int i = 0; i < lo; i++) {
        numEntries++;
    }

    // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
	// POTENTIAL FLAW: 除零错误
    float f1 = (float)totalProbe / (float)numEntries;     // zyz
}


void GoodCase04(int lo)
{
    float b = 0;
    float a = 1;
    a = b;
    // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
	// POTENTIAL FLAW: 除零错误
    (float)10.0 / a;//error
}



#define UINT32 unsigned int 
#define DOUBLE double 
#define FLOAT float 

void GoodCase05()
{
    UINT32 client_rate = 1;
    UINT32 time_slot = 0;
    UINT32 target_cgen_m = 0;

    // 华为c语言编程规范v5.0，只检查整形除法或者余数预算，不要求检查浮点型运算
	// POTENTIAL FLAW: 除零错误
    target_cgen_m = (UINT32)((DOUBLE)((DOUBLE)(client_rate) * 1000) / (DOUBLE)(25 * time_slot));  //zyz
}

typedef unsigned long VOS_UINT32;
typedef float VOS_FLOAT;
void Goodcase06(int u)
{
    VOS_UINT32 u32ESM1Number = 0;
    VOS_FLOAT f32ESM1PortVoltage = 0;
    VOS_FLOAT  f32ESM1TotalVoltage = 0;
    for (int i = 0; i < u; i++) {
        u32ESM1Number++;
        f32ESM1TotalVoltage += 0.1;
    }
    if (0 != u32ESM1Number) {
        f32ESM1PortVoltage = f32ESM1TotalVoltage / u32ESM1Number; // good
    }
}

int  ReadByte();
// @scene 取余预算，除数可能为0
void BadCase08()
{
    int a = ReadByte();
    // POTENTIAL FLAW: 除零错误
    int b = 1000 / a; //a可能是0    
    // POTENTIAL FLAW: 除零错误
    int c = 1000 % a;//a 可能是0    
}



/*
////
target datalayout = ""e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128""
target triple = ""x86_64-pc-linux-gnu""


@.str = private constant [3 x i8] [i8 37, i8 100, i8 0]

define i32 @cond01 ( i32 %p1 ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  store i32 %p1 , i32* %2 , align 4
  %3 = icmp sgt i32 %p1 , 5, !dbg !6
  br i1 %3 , label %4 , label %5, !dbg !7
; <label>:4
  store i32 1 , i32* %1, !dbg !8
  br label %6, !dbg !9
; <label>:5
  store i32 0 , i32* %1, !dbg !10
  br label %6, !dbg !11
; <label>:6
  %7 = phi i32 [ [0, %5], [1, %4] ]
  ret i32 %7, !dbg !12 }

define i32 @BadCase01 ( i32 %p1 ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %x = alloca i32 , align 4
  store i32 %p1 , i32* %1 , align 4
  store i32 0 , i32* %x , align 4, !dbg !14
  %2 = call i32 @cond01 ( i32 %p1 ), !dbg !15
  %3 = icmp ne i32 %2 , 0, !dbg !16
  br i1 %3 , label %4 , label %5, !dbg !17
; <label>:4
  store i32 1 , i32* %x , align 4, !dbg !18
  br label %5, !dbg !19
; <label>:5
  %6 = phi i32 [ [1, %4], [0, %0] ]
  %7 = div i32 1 , %6, !dbg !20
  ret i32 %7, !dbg !21 }

define i32 @foo02 ( i32 %y ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca i32 , align 4
  store i32 %y , i32* %2 , align 4
  %3 = icmp slt i32 %y , 0, !dbg !23
  br i1 %3 , label %4 , label %5, !dbg !24
; <label>:4
  store i32 0 , i32* %1, !dbg !25
  br label %6, !dbg !26
; <label>:5
  store i32 %y , i32* %1, !dbg !27
  br label %6, !dbg !28
; <label>:6
  %7 = phi i32 [ [%y, %5], [0, %4] ]
  ret i32 %7, !dbg !29 }

define void @BadCase02 ( i32 %y ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %y , i32* %1 , align 4
  %2 = call i32 @foo02 ( i32 %y ), !dbg !33
  %3 = div i32 1 , %2, !dbg !34
  store i32 %3 , i32* %z , align 4, !dbg !35
  ret void, !dbg !36 }

define void @BadCase03 ( i32 %y ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %y , i32* %1 , align 4
  %2 = div i32 1 , %y, !dbg !38
  store i32 %2 , i32* %z , align 4, !dbg !39
  ret void, !dbg !40 }

define void @Outer01 ( ) {
 ; <label>:0
  %p1 = alloca i32 , align 4
  %y = alloca i32 , align 4
  store i32 6 , i32* %p1 , align 4, !dbg !44
  store i32 1 , i32* %y , align 4, !dbg !45
  br i1 -1 , label %1 , label %2, !dbg !46
; <label>:1
  store i32 0 , i32* %y , align 4, !dbg !47
  br label %2, !dbg !48
; <label>:2
  %3 = phi i32 [ [0, %1], [1, %0] ]
  call void @BadCase03 ( i32 %3 ), !dbg !49
  ret void, !dbg !50 }

define float @BadCase04 ( i32 %num ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %ret = alloca float , align 4
  store i32 %num , i32* %1 , align 4
  %2 = div i32 %num , 0, !dbg !55
  %3 = sitofp i32 %2 to float, !dbg !56
  store float %3 , float* %ret , align 4, !dbg !57
  ret float %3, !dbg !58 }

define void @GoodCase01 ( i32 %actCnt, float %lossTot ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %2 = alloca float , align 4
  %i = alloca i32 , align 4
  %lossAvg = alloca float , align 4
  store i32 %actCnt , i32* %1 , align 4
  store float %lossTot , float* %2 , align 4
  store i32 0 , i32* %i , align 4, !dbg !62
  br label %3, !dbg !63
; <label>:3
  %4 = phi i32 [ [%7, %6], [0, %0] ]
  %5 = icmp slt i32 %4 , %actCnt, !dbg !64
  br i1 %5 , label %6 , label %8, !dbg !65
; <label>:6
  %7 = add nsw i32 %4 , 1, !dbg !66
  store i32 %7 , i32* %i , align 4, !dbg !67
  br label %3, !dbg !68
; <label>:8
  %9 = icmp ne i32 %4 , 0, !dbg !69
  br i1 %9 , label %10 , label %13, !dbg !70
; <label>:10
  %11 = sitofp i32 %4 to float, !dbg !71
  %12 = div float %lossTot , %11, !dbg !72
  store float %12 , float* %lossAvg , align 4, !dbg !73
  br label %14, !dbg !74
; <label>:13
  store float 0.0 , float* %lossAvg , align 4, !dbg !75
  br label %14
; <label>:14
  ret void, !dbg !76 }

define void @BadCase05 ( ) {
 ; <label>:0
  %a = alloca i32 , align 4
  %z = alloca i32 , align 4
  %1 = call i32 @scanf ( i8* i8* getelementptr ( [3 x i8]* @.str ,  i32 0, i32 0 ), i32* %a ), !dbg !78
  %3 = load i32* %a , align 4, !dbg !79
  %4 = div i32 1 , %3, !dbg !80
  store i32 %4 , i32* %z , align 4, !dbg !81
  ret void, !dbg !82 }

define void @BadCase06 ( i32 %b ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %b , i32* %1 , align 4
  %2 = icmp slt i32 %b , 10, !dbg !84
  br i1 %2 , label %3 , label %5, !dbg !85
; <label>:3
  %4 = div i32 1 , %b, !dbg !86
  store i32 %4 , i32* %z , align 4, !dbg !87
  br label %5, !dbg !88
; <label>:5
  ret void, !dbg !89 }

define void @BadCase07 ( i32 %flag ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %i1 = alloca i8 , align 1
  %i2 = alloca i32 , align 4
  %i3 = alloca i64 , align 8
  %i4 = alloca float , align 4
  %i5 = alloca double , align 8
  %valueA = alloca double , align 8
  %valueC = alloca double , align 8
  store i32 %flag , i32* %1 , align 4
  store i8 0 , i8* %i1 , align 1, !dbg !91
  store i32 0 , i32* %i2 , align 4, !dbg !92
  store i64 0 , i64* %i3 , align 8, !dbg !93
  store float 0.0 , float* %i4 , align 4, !dbg !94
  store double 0.0 , double* %i5 , align 8, !dbg !95
  store double 1.0 , double* %valueA , align 8, !dbg !96
  store double 0.0 , double* %valueC , align 8, !dbg !97
  %2 = icmp eq i32 %flag , 0, !dbg !98
  br i1 %2 , label %3 , label %4, !dbg !99
; <label>:3
  br label %32, !dbg !100
; <label>:4
  %5 = icmp eq i32 %flag , 10, !dbg !101
  br i1 %5 , label %6 , label %7, !dbg !102
; <label>:6
  br label %31, !dbg !103
; <label>:7
  %8 = icmp eq i32 %flag , 100, !dbg !104
  br i1 %8 , label %9 , label %10, !dbg !105
; <label>:9
  br label %30, !dbg !106
; <label>:10
  %11 = icmp eq i32 %flag , 1000, !dbg !107
  br i1 %11 , label %12 , label %14, !dbg !108
; <label>:12
  %13 = div float 100.0 , 0.0, !dbg !109
  br label %29, !dbg !110
; <label>:14
  %15 = icmp eq i32 %flag , 1001, !dbg !111
  br i1 %15 , label %16 , label %18, !dbg !112
; <label>:16
  %17 = div double 100.0 , 0.0, !dbg !113
  br label %28, !dbg !114
; <label>:18
  %19 = icmp eq i32 %flag , 1002, !dbg !115
  br i1 %19 , label %20 , label %22, !dbg !116
; <label>:20
  %21 = div double 1.0 , 0.0, !dbg !117
  store double %21 , double* %valueC , align 8, !dbg !118
  br label %27, !dbg !119
; <label>:22
  %23 = icmp eq i32 %flag , 1003, !dbg !120
  br i1 %23 , label %24 , label %26, !dbg !121
; <label>:24
  %25 = div double 1.0 , 0.0, !dbg !122
  br label %26, !dbg !123
; <label>:26
  br label %27
; <label>:27
  br label %28
; <label>:28
  br label %29
; <label>:29
  br label %30
; <label>:30
  br label %31
; <label>:31
  br label %32
; <label>:32
  ret void, !dbg !124 }

define void @GoodCase02 ( double %rateo1, double %rateo2, double %meterval_o, i32 %flag ) {
 ; <label>:0
  %1 = alloca double , align 8
  %2 = alloca double , align 8
  %3 = alloca double , align 8
  %4 = alloca i32 , align 4
  %rateo = alloca double , align 8
  %rated = alloca double , align 8
  %retl = alloca double , align 8
  store double %rateo1 , double* %1 , align 8
  store double %rateo2 , double* %2 , align 8
  store double %meterval_o , double* %3 , align 8
  store i32 %flag , i32* %4 , align 4
  store double 0.0 , double* %rateo , align 8, !dbg !129
  store double 0.0 , double* %rated , align 8, !dbg !130
  %5 = icmp eq i32 %flag , 0, !dbg !131
  br i1 %5 , label %6 , label %7, !dbg !132
; <label>:6
  store double %rateo1 , double* %rateo , align 8, !dbg !133
  store double %rateo2 , double* %rated , align 8, !dbg !134
  br label %7, !dbg !135
; <label>:7
  %8 = phi double [ [%rateo2, %6], [0.0, %0] ]
  %9 = phi double [ [%rateo1, %6], [0.0, %0] ]
  %10 = fcmp olt double %9 , 1.0e-10, !dbg !136
  br i1 %10 , label %13 , label %11, !dbg !137
; <label>:11
  %12 = fcmp olt double %8 , 1.0e-10, !dbg !138
  br i1 %12 , label %13 , label %14, !dbg !139
; <label>:13
  br label %17, !dbg !140
; <label>:14
  %15 = mul double %meterval_o , %9, !dbg !141
  %16 = div double %15 , %8, !dbg !142
  store double %16 , double* %retl , align 8, !dbg !143
  br label %17, !dbg !144
; <label>:17
  ret void, !dbg !145 }

define void @GoodCase03 ( i32 %lo ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %numEntries = alloca i32 , align 4
  %totalProbe = alloca i32 , align 4
  %i = alloca i32 , align 4
  %f1 = alloca float , align 4
  store i32 %lo , i32* %1 , align 4
  store i32 0 , i32* %totalProbe , align 4, !dbg !147
  store i32 0 , i32* %numEntries , align 4, !dbg !148
  store i32 0 , i32* %i , align 4, !dbg !149
  br label %2, !dbg !150
; <label>:2
  %3 = phi i32 [ [%7, %6], [0, %0] ]
  %4 = phi i32 [ [%8, %6], [0, %0] ]
  %5 = icmp slt i32 %4 , %lo, !dbg !151
  br i1 %5 , label %6 , label %9, !dbg !152
; <label>:6
  %7 = add nsw i32 %3 , 1, !dbg !153
  store i32 %7 , i32* %numEntries , align 4, !dbg !154
  %8 = add nsw i32 %4 , 1, !dbg !155
  store i32 %8 , i32* %i , align 4, !dbg !156
  br label %2, !dbg !157
; <label>:9
  %10 = sitofp i32 %3 to float, !dbg !158
  %11 = div float 0.0 , %10, !dbg !159
  store float %11 , float* %f1 , align 4, !dbg !160
  ret void, !dbg !161 }

define void @GoodCase04 ( i32 %lo ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %b = alloca float , align 4
  %a = alloca float , align 4
  store i32 %lo , i32* %1 , align 4
  store float 0.0 , float* %b , align 4, !dbg !163
  store float 1.0 , float* %a , align 4, !dbg !164
  store float 0.0 , float* %a , align 4, !dbg !165
  %2 = div float 10.0 , 0.0, !dbg !166
  ret void, !dbg !167 }

define void @GoodCase05 ( ) {
 ; <label>:0
  %client_rate = alloca i32 , align 4
  %time_slot = alloca i32 , align 4
  %target_cgen_m = alloca i32 , align 4
  store i32 1 , i32* %client_rate , align 4, !dbg !169
  store i32 0 , i32* %time_slot , align 4, !dbg !170
  store i32 0 , i32* %target_cgen_m , align 4, !dbg !171
  %1 = div double 1000.0 , 0.0, !dbg !172
  %2 = fptoui double %1 to i32, !dbg !173
  store i32 %2 , i32* %target_cgen_m , align 4, !dbg !174
  ret void, !dbg !175 }

define void @Goodcase06 ( i32 %u ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %u32ESM1Number = alloca i64 , align 8
  %f32ESM1PortVoltage = alloca float , align 4
  %f32ESM1TotalVoltage = alloca float , align 4
  %i = alloca i32 , align 4
  store i32 %u , i32* %1 , align 4
  store i64 0 , i64* %u32ESM1Number , align 8, !dbg !177
  store float 0.0 , float* %f32ESM1PortVoltage , align 4, !dbg !178
  store float 0.0 , float* %f32ESM1TotalVoltage , align 4, !dbg !179
  store i32 0 , i32* %i , align 4, !dbg !180
  br label %2, !dbg !181
; <label>:2
  %3 = phi float [ [%11, %7], [0.0, %0] ]
  %4 = phi i64 [ [%8, %7], [0, %0] ]
  %5 = phi i32 [ [%12, %7], [0, %0] ]
  %6 = icmp slt i32 %5 , %u, !dbg !182
  br i1 %6 , label %7 , label %13, !dbg !183
; <label>:7
  %8 = add i64 %4 , 1, !dbg !184
  store i64 %8 , i64* %u32ESM1Number , align 8, !dbg !185
  %9 = fpext float %3 to double, !dbg !186
  %10 = add double %9 , 0.1, !dbg !187
  %11 = fptrunc double %10 to float, !dbg !188
  store float %11 , float* %f32ESM1TotalVoltage , align 4, !dbg !189
  %12 = add nsw i32 %5 , 1, !dbg !190
  store i32 %12 , i32* %i , align 4, !dbg !191
  br label %2, !dbg !192
; <label>:13
  %14 = icmp ne i64 0 , %4, !dbg !193
  br i1 %14 , label %15 , label %18, !dbg !194
; <label>:15
  %16 = uitofp i64 %4 to float, !dbg !195
  %17 = div float %3 , %16, !dbg !196
  store float %17 , float* %f32ESM1PortVoltage , align 4, !dbg !197
  br label %18, !dbg !198
; <label>:18
  ret void, !dbg !199 }

define void @BadCase08 ( ) {
 ; <label>:0
  %a = alloca i32 , align 4
  %b = alloca i32 , align 4
  %c = alloca i32 , align 4
  %1 = call i32 @ReadByte ( ), !dbg !201
  store i32 %1 , i32* %a , align 4, !dbg !202
  %2 = div i32 1000 , %1, !dbg !203
  store i32 %2 , i32* %b , align 4, !dbg !204
  %3 = rem i32 1000 , %1, !dbg !205
  store i32 %3 , i32* %c , align 4, !dbg !206
  ret void, !dbg !207 }

  */
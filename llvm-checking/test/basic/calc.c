
/* This program is a version of the reverse Polish calculator
   described in the first edition of kernighan and ritchie, pp. 74ff. */

#define MAXVAL 100
#define MAXOP 20
#define NUMBER '0'
#define DONE 'q'
#define TOOBIG '9'
#include <math.h>
#include <stdio.h>

int sp = 0;
double val[MAXVAL];

main()
{
     int type;
     char s[MAXOP];
     double num1, num2, op2, atof(), pop();
     void push();

     type = getop(s,MAXOP);
     while (type != DONE)
     {
        switch (type) 
        { 

          case NUMBER:
            num1 = atof(s);
            push(num1);
            break;

          case '+':
            num1 = pop();
            num2 = pop();
            push(num1 + num2);
            break;
         
          case '*':
            num1 = pop();
            num2 = pop();
            push(num1 * num2);
            break;
         
          case '-':
            num1 = pop();
            num2 = pop();
            push(num2 - num1);
            break;
         
          case '/':
            num1 = pop();
            if (num1 != 0.0)
            {
               num2 = pop();
               push( num2 / num1);
            }
            else
               printf("zero divisor popped\n");
            break;
         
           case '=':
            num1 = pop();
            printf("\t%f\n",num1);
            push(num1);
            break;
         
           case 'c':
            clear();
            break;

           case TOOBIG:
            printf("%.20s ... is too long\n",s);
            break;

           default:
            printf("unknown command %c\n",type);
            break;

         }

         type = getop(s,MAXOP);
     }
}

void push(f)
double f;
{
     if (sp < MAXVAL)
     {
        val[sp++] = f;
     }
     else
     {
         printf("error: stack full\n");
         clear();
     }
}

double pop()
{
     double retval;

     if (sp > 0)
     {
        retval = val[--sp];
     }
     else
     { 
        printf("error: stack empty\n");
        clear();
        retval = 0;
     }
     return(retval);
}

clear()
{
   sp = 0;
}

getop(s, lim)
char s[];
int lim;

{
   int i,c,retval;

   c = getc(stdin);
   while ( c == ' ' || c == '\t' || c == '\n' )
   {
       c = getc(stdin);
   }

   if (c != '.' && (c < '0' || c > '9') )
   {
       retval = c; 
   }
   else
   {
      s[0] = c;

      c = getc(stdin);
      i = 1;
      while (c >= '0' && c <= '9')
      {
          if (i < lim)
          {
             s[i] = c;
          }
          c = getc(stdin);
          i++;
      }

      if (c == '.')
      {
          if (i < lim)
          {
             s[i] = c;
          }
          i++;
          c = getc(stdin);
          while (c >= '0' && c <= '9')
          {
              if (i<lim)
              {
                 s[i] = c;
              }
              i++;
              c = getc(stdin);
          }
      }

      if (i < lim)
      {
         ungetc(c,stdin);
         s[i] = '\0';
         retval = NUMBER;
      }
      else
      {
         while (c!= '\n' && c != DONE)
         {
           c = getc(stdin);
         }
         s[lim-1] = '\0';
         retval = TOOBIG;
      }
   }

   return (retval);
}

/*
Variable      Labels  
----------------------
 c@getop             {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 f@push              {}
 i@getop             {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 lim@getop           {}
 num1@main           {"calc.c: [23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,90,92,101,105,107,113,115,120,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 num2@main           {"calc.c: [23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,90,92,101,105,107,113,115,120,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 retval@getop        {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 retval@pop          {"calc.c: [23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,90,92,101,105,107,113,115,120,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 s@getop             {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 s@main              {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 sp                  {"calc.c: [90,92,105,107,120]"}
 type@main           {"calc.c: [23,83,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}
 val                 {"calc.c: [23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,90,92,101,105,107,113,115,120,123,130,131,133,136,138,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,179,183,185,187,188,192]"}


-- by "bwdIFDS"
 c@getop             {23,83,130,131,133,136,144,145,146,148,152,153,156,158,162,163,164,166,170,171,183,185}
 f@push              {}
 i@getop             {23,83,130,131,133,136,144,145,146,148,152,153,156,158,162,163,164,166,170,171}
 lim@getop           {}
 num1@main           {23,24,26,30,31,35,37,41,43,47,49,53,54,57,64,66,83,90,92,105,107,120,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 num2@main           {23,24,26,30,31,36,37,42,43,48,49,54,56,57,64,66,83,90,92,105,107,120,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 op2@main            {}
 retval@getop        {23,83,130,131,133,136,138,144,145,146,148,152,153,156,158,162,163,164,166,170,171,175,179,183,185,188}
 retval@pop          {23,24,26,30,31,37,43,49,54,57,64,66,83,90,92,105,107,113,120,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 s@getop             {23,83,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 s@main              {23,83,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 sp                  {90,92,105,107,120}
 type@main           {23,83,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}
 val                 {23,24,26,30,31,37,43,49,54,57,64,66,83,90,92,105,107,120,130,131,133,136,142,144,145,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,178,183,185,187}

 


Forward SliceTable:
Variable      Labels  
----------------------
 c@getop             {130,152,163,171}  -- [130,133,144,152,163,171,185]
 f@push              {57,92}
 i@getop             {153,162,170} -- [145,153,162,170]
 lim@getop           {150,152,153,160,162,163,168,170,171,175,177,178,179,187}
                  -- [146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,177,178,179,183,185,187,188]
 num1@main           {}   -- [35,41,47,53,64]
 num2@main           {56}  -- [36,42,48,56]
 op2@main            {}
 retval@getop        {138,179,188}  -- [138,179,188]
 retval@pop          {107}   -- [107,113]
 s@getop             {178,187}
 s@main              {23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,178,187}
                  -- [23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,142,150,160,168,178,187]
 sp                  {56,57,60,90,92,96,97,105,107,111,112,113,120}
                  -- [35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,90,92,96,97,105,107,111,112,113,115,120]
 type@main           {23,83}
 val                 {56,57,60,92,107} 
                   --[35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,92,107,115]

Forward SliceTable by "ifdsSlicer":
Variable      Labels  
----------------------
 c@getop             {130,133,144,152,163,171,185}
 f@push              {92}
 i@getop             {145,153,162,170}
 lim@getop           {146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,177,178,179,183,185,187,188}
 num1@main           {35,41,47,53,64}
 num2@main           {36,42,48,56}
 op2@main            {}
 retval@getop        {138,179,188}
 retval@pop          {107,113}
 s@getop             {142,150,160,168,178,187}
 s@main              {23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,142,150,160,168,178,187}
 sp                  {35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,90,92,96,97,105,107,111,112,113,115,120}
 type@main           {23,83}
 val                 {35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,92,107,115}

 c@getop             {130,133,144,152,163,171,185}
 f@push              {35,37,41,43,47,49,53,57,64,65,66,92}
 i@getop             {145,153,162,170}
 lim@getop           {23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,146,148,150,152,153,156,158,160,162,163,164,166,168,170,171,175,177,178,179,183,185,187,188}
 num1@main           {35,41,47,53,64}
 num2@main           {36,42,48,56}
 op2@main            {}
 retval@getop        {138,179,188}
 retval@pop          {107,113}
 s@getop             {23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,142,150,160,168,178,187}
 s@main              {23,24,26,30,31,35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,70,74,78,83,92,142,150,160,168,178,187}
 sp                  {35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,90,92,96,97,105,107,111,112,113,115,120}
 type@main           {23,83}
 val                 {35,36,37,41,42,43,47,48,49,53,54,56,57,60,64,65,66,92,107,115}


ProcTables:
fromList [("clear",
Variable      Labels  
----------------------
 @.str         {-3}
 @.str1        {-17}
 @.str2        {-22}
 @.str3        {-31}
 @.str4        {-41}
 @.str5        {-44}
 @sp           {221}
 @val          {-39}
),("getop",
Variable      Labels  
----------------------
 %1@getop             {-225,233}
 %2@getop             {-226,234}
 %c@getop             {-226,237,243,256,260,265,280,287,294,297,306,312,315,325,331,337,340,351,372,378,382}
 %i@getop             {-226,243,256,265,281,287,294,297,308,312,315,322,331,337,340,348}
 %lim@getop           {-226}
 %retval@getop        {-226,243,256,265,268,273,287,294,312,359,365,372,378,388}
 %s@getop             {-226,-225,243,256,265,277,287,294,297,301,312,315,318,331,337,340,344,359,364,372,378,387}
 @.str                {-3}
 @.str1               {-17}
 @.str2               {-22}
 @.str3               {-31}
 @.str4               {-41}
 @.str5               {-44}
 @sp                  {-1}
 @val                 {-39}
),("main",
Variable      Labels  
----------------------
 %1@getop             {59,166,233}
 %1@main              {57}
 %1@push              {-39,-1,67,93,99,107,115,123,130,135,150,175,179,182,184,199,203,221}
 %2                   {59,166,243,256,265,277,287,294,297,301,312,315,318,331,337,340,344,359,364,372,378,387}
 %2@getop             {59,166,234}
 %c@getop             {59,166,237,243,256,260,265,280,287,294,297,306,312,315,325,331,337,340,351,372,378,382}
 %f@push              {}
 %i@getop             {59,166,243,256,265,281,287,294,297,308,312,315,322,331,337,340,348}
 %lim@getop           {}
 %num1@main           {-39,-1,67,93,98,99,103,107,111,115,119,123,127,130,135,146,150,179,182,184,199,203,221}
 %num2@main           {-39,-1,67,93,99,105,107,113,115,121,123,130,133,135,150,179,182,184,199,203,221}
 %retval@getop        {59,166,243,256,265,268,273,287,294,312,359,365,372,378,388}
 %retval@pop          {-39,-1,67,93,99,107,115,123,130,135,150,179,182,184,199,203,206,213,221}
 %s@getop             {59,166,243,256,265,277,287,294,297,301,312,315,318,331,337,340,344,359,364,372,378,387}
 %type@main           {61,167}
 @.str                {-3}
 @.str1               {-17}
 @.str2               {-22}
 @.str3               {-31}
 @.str4               {-41}
 @.str5               {-44}
 @sp                  {-1,179,182,199,203,221}
 @val                 {-39,-1,67,93,99,107,115,123,130,135,150,179,182,184,199,203,221}
),("pop",
Variable      Labels  
----------------------
 %retval@pop        {-39,-1,199,203,206,213}
 @.str              {-3}
 @.str1             {-17}
 @.str2             {-22}
 @.str3             {-31}
 @.str4             {-41}
 @.str5             {-44}
 @sp                {-1,199,203,221}
 @val               {-39}
),("push",
Variable      Labels  
----------------------
 %1@push        {-172,175}
 %f@push        {-172}
 @.str          {-3}
 @.str1         {-17}
 @.str2         {-22}
 @.str3         {-31}
 @.str4         {-41}
 @.str5         {-44}
 @sp            {-1,179,182,221}
 @val           {-172,-39,-1,179,184}
)]



----------------------
@sp = i32 0
@.str = private constant [21 x i8] [i8 122, i8 101, i8 114, i8 111, i8 32, i8 100, i8 105, i8 118, i8 105, i8 115, i8 111, i8 114, i8 32, i8 112, i8 111, i8 112, i8 112, i8 101, i8 100, i8 10, i8 0]
@.str1 = private constant [5 x i8] [i8 9, i8 37, i8 102, i8 10, i8 0]
@.str2 = private constant [23 x i8] [i8 37, i8 46, i8 50, i8 48, i8 115, i8 32, i8 46, i8 46, i8 46, i8 32, i8 105, i8 115, i8 32, i8 116, i8 111, i8 111, i8 32, i8 108, i8 111, i8 110, i8 103, i8 10, i8 0]
@.str3 = private constant [20 x i8] [i8 117, i8 110, i8 107, i8 110, i8 111, i8 119, i8 110, i8 32, i8 99, i8 111, i8 109, i8 109, i8 97, i8 110, i8 100, i8 32, i8 37, i8 99, i8 10, i8 0]
@val = common [100 x double] zeroinitializer
@.str4 = private constant [19 x i8] [i8 101, i8 114, i8 114, i8 111, i8 114, i8 58, i8 32, i8 115, i8 116, i8 97, i8 99, i8 107, i8 32, i8 102, i8 117, i8 108, i8 108, i8 10, i8 0]
@.str5 = private constant [20 x i8] [i8 101, i8 114, i8 114, i8 111, i8 114, i8 58, i8 32, i8 115, i8 116, i8 97, i8 99, i8 107, i8 32, i8 101, i8 109, i8 112, i8 116, i8 121, i8 10, i8 0]

define i32 @main ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %type = alloca i32 , align 4
  %s = alloca [20 x i8] , align 1
  %num1 = alloca double , align 8
  %num2 = alloca double , align 8
  %op2 = alloca double , align 8
  store i32 0 , i32* %1
  %2 = getelementptr inbounds [20 x i8]* %s , i32 0, i32 0, !dbg !6
  %3 = call i32 @getop ( i8* %2, i32 20 ), !dbg !7
  store i32 %3 , i32* %type , align 4, !dbg !8
  br label %4, !dbg !9
; <label>:4
  %5 = phi i32 [ [%46, %45], [%3, %0] ]  // type
  %6 = icmp ne i32 %5 , 113, !dbg !10
  br i1 %6 , label %7 , label %47, !dbg !11
; <label>:7
  switch i32 %5 , label %42 [ [1 x <2 x i32>] [<2 x i32> [i32 48, i32 48]], label %8 [1 x <2 x i32>] [<2 x i32> [i32 43, i32 43]], label %11 [1 x <2 x i32>] [<2 x i32> [i32 42, i32 42]], label %15 [1 x <2 x i32>] [<2 x i32> [i32 45, i32 45]], label %19 [1 x <2 x i32>] [<2 x i32> [i32 47, i32 47]], label %23 [1 x <2 x i32>] [<2 x i32> [i32 61, i32 61]], label %33 [1 x <2 x i32>] [<2 x i32> [i32 99, i32 99]], label %37 [1 x <2 x i32>] [<2 x i32> [i32 57, i32 57]], label %39 ], !dbg !12
; <label>:8
  %9 = call double double(i8*)* bitcast (double(, ...) @atof to double(i8*)*) ( i8* %2 ), !dbg !13
  store double %9 , double* %num1 , align 8, !dbg !14
  call void @push ( double %9 ), !dbg !15
  br label %45, !dbg !16
; <label>:11
  %12 = call double @pop ( ), !dbg !17
  store double %12 , double* %num1 , align 8, !dbg !18
  %13 = call double @pop ( ), !dbg !19
  store double %13 , double* %num2 , align 8, !dbg !20
  %14 = add double %12 , %13, !dbg !21
  call void @push ( double %14 ), !dbg !22
  br label %45, !dbg !23
; <label>:15
  %16 = call double @pop ( ), !dbg !24
  store double %16 , double* %num1 , align 8, !dbg !25
  %17 = call double @pop ( ), !dbg !26
  store double %17 , double* %num2 , align 8, !dbg !27
  %18 = mul double %16 , %17, !dbg !28
  call void @push ( double %18 ), !dbg !29
  br label %45, !dbg !30
; <label>:19
  %20 = call double @pop ( ), !dbg !31
  store double %20 , double* %num1 , align 8, !dbg !32
  %21 = call double @pop ( ), !dbg !33
  store double %21 , double* %num2 , align 8, !dbg !34
  %22 = sub double %21 , %20, !dbg !35
  call void @push ( double %22 ), !dbg !36
  br label %45, !dbg !37
; <label>:23
  %24 = call double @pop ( ), !dbg !38
  store double %24 , double* %num1 , align 8, !dbg !39
  %25 = fcmp une double %24 , 0.0, !dbg !40
  br i1 %25 , label %26 , label %29, !dbg !41
; <label>:26
  %27 = call double @pop ( ), !dbg !42
  store double %27 , double* %num2 , align 8, !dbg !43
  %28 = div double %27 , %24, !dbg !44
  call void @push ( double %28 ), !dbg !45
  br label %32, !dbg !46
; <label>:29
  %30 = call i32 @printf ( i8* i8* getelementptr ( [21 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !47
  br label %32
; <label>:32
  br label %45, !dbg !48
; <label>:33
  %34 = call double @pop ( ), !dbg !49
  store double %34 , double* %num1 , align 8, !dbg !50
  %35 = call i32 @printf ( i8* i8* getelementptr ( [5 x i8]* @.str1 ,  i32 0, i32 0 ), double %34 ), !dbg !51
  call void @push ( double %34 ), !dbg !52
  br label %45, !dbg !53
; <label>:37
  %38 = call i32 @clear ( ), !dbg !54
  br label %45, !dbg !55
; <label>:39
  %40 = call i32 @printf ( i8* i8* getelementptr ( [23 x i8]* @.str2 ,  i32 0, i32 0 ), i8* %2 ), !dbg !56
  br label %45, !dbg !57
; <label>:42
  %43 = call i32 @printf ( i8* i8* getelementptr ( [20 x i8]* @.str3 ,  i32 0, i32 0 ), i32 %5 ), !dbg !58
  br label %45, !dbg !59
; <label>:45
  %46 = call i32 @getop ( i8* %2, i32 20 ), !dbg !60
  store i32 %46 , i32* %type , align 4, !dbg !61
  br label %4, !dbg !62
; <label>:47
  ret i32 0, !dbg !63 }

define void @push ( double %f ) {
 ; <label>:0
  %1 = alloca double , align 8
  store double %f , double* %1 , align 8
  %2 = load i32* @sp , align 4, !dbg !68
  %3 = icmp slt i32 %2 , 100, !dbg !69
  br i1 %3 , label %4 , label %7, !dbg !70
; <label>:4
  %5 = add nsw i32 %2 , 1, !dbg !71
  store i32 %5 , i32* @sp , align 4, !dbg !72
  %6 = getelementptr inbounds [100 x double]* @val , i32 0, i32 %2, !dbg !73
  store double %f , double* %6 , align 4, !dbg !74
  br label %11, !dbg !75
; <label>:7
  %8 = call i32 @printf ( i8* i8* getelementptr ( [19 x i8]* @.str4 ,  i32 0, i32 0 ) ), !dbg !76
  %10 = call i32 @clear ( ), !dbg !77
  br label %11
; <label>:11
  ret void, !dbg !78 }

define double @pop ( ) {
 ; <label>:0
  %retval = alloca double , align 8
  %1 = load i32* @sp , align 4, !dbg !82
  %2 = icmp sgt i32 %1 , 0, !dbg !83
  br i1 %2 , label %3 , label %7, !dbg !84
; <label>:3
  %4 = add nsw i32 %1 , -1, !dbg !85
  store i32 %4 , i32* @sp , align 4, !dbg !86
  %5 = getelementptr inbounds [100 x double]* @val , i32 0, i32 %4, !dbg !87
  %6 = load double* %5 , align 4, !dbg !88
  store double %6 , double* %retval , align 8, !dbg !89
  br label %11, !dbg !90
; <label>:7
  %8 = call i32 @printf ( i8* i8* getelementptr ( [20 x i8]* @.str5 ,  i32 0, i32 0 ) ), !dbg !91
  %10 = call i32 @clear ( ), !dbg !92
  store double 0.0 , double* %retval , align 8, !dbg !93
  br label %11
; <label>:11
  %12 = phi double [ [0.0, %7], [%6, %3] ]  // retval
  ret double %12, !dbg !94 }

define i32 @clear ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 0 , i32* @sp , align 4, !dbg !96
  ret i32 undef, !dbg !97 }

define i32 @getop ( i8* %s, i32 %lim ) {
 ; <label>:0
  %1 = alloca i8* , align 4
  %2 = alloca i32 , align 4
  %i = alloca i32 , align 4
  %c = alloca i32 , align 4
  %retval = alloca i32 , align 4
  store i8* %s , i8** %1 , align 4
  store i32 %lim , i32* %2 , align 4
  %3 = load %struct._IO_FILE** @stdin , align 4, !dbg !103
  %4 = call i32 @_IO_getc ( %struct._IO_FILE* %3 ), !dbg !104
  store i32 %4 , i32* %c , align 4, !dbg !105
  br label %5, !dbg !106
; <label>:5
  %6 = phi i32 [ [%17, %15], [%4, %0] ]  // c
  %7 = icmp eq i32 %6 , 32, !dbg !107
  br i1 %7 , label %12 , label %8, !dbg !108
; <label>:8
  %9 = icmp eq i32 %6 , 9, !dbg !109
  br i1 %9 , label %12 , label %10, !dbg !110
; <label>:10
  %11 = icmp eq i32 %6 , 10, !dbg !111
  br label %12, !dbg !112
; <label>:12
  %13 = phi i32 [ [9, %8], [32, %5], [%6, %10] ]  //c
  %14 = phi i1 [ [-1, %8], [-1, %5], [%11, %10] ]  // ?
  br i1 %14 , label %15 , label %18, !dbg !113
; <label>:15
  %16 = load %struct._IO_FILE** @stdin , align 4, !dbg !114
  %17 = call i32 @_IO_getc ( %struct._IO_FILE* %16 ), !dbg !115
  store i32 %17 , i32* %c , align 4, !dbg !116
  br label %5, !dbg !117
; <label>:18
  %19 = icmp ne i32 %13 , 46, !dbg !118
  br i1 %19 , label %20 , label %25, !dbg !119
; <label>:20
  %21 = icmp slt i32 %13 , 48, !dbg !120
  br i1 %21 , label %24 , label %22, !dbg !121
; <label>:22
  %23 = icmp sgt i32 %13 , 57, !dbg !122
  br i1 %23 , label %24 , label %25, !dbg !123
; <label>:24
  store i32 %13 , i32* %retval , align 4, !dbg !124
  br label %98, !dbg !125
; <label>:25
  %26 = trunc i32 %13 to i8, !dbg !126
  store i8 %26 , i8* %s , align 1, !dbg !127
  %27 = load %struct._IO_FILE** @stdin , align 4, !dbg !128
  %28 = call i32 @_IO_getc ( %struct._IO_FILE* %27 ), !dbg !129
  store i32 %28 , i32* %c , align 4, !dbg !130
  store i32 1 , i32* %i , align 4, !dbg !131
  br label %29, !dbg !132
; <label>:29
  %30 = phi i32 [ [%45, %42], [1, %25] ]   // i
  %31 = phi i32 [ [%44, %42], [%28, %25] ]  // c
  %32 = icmp sge i32 %31 , 48, !dbg !133
  br i1 %32 , label %33 , label %35, !dbg !134
; <label>:33
  %34 = icmp sle i32 %31 , 57, !dbg !135
  br label %35
; <label>:35
  %36 = phi i1 [ [0, %29], [%34, %33] ]  // ?
  br i1 %36 , label %37 , label %46
; <label>:37
  %38 = icmp slt i32 %30 , %lim, !dbg !136
  br i1 %38 , label %39 , label %42, !dbg !137
; <label>:39
  %40 = trunc i32 %31 to i8, !dbg !138
  %41 = getelementptr inbounds i8* %s , i32 %30, !dbg !139
  store i8 %40 , i8* %41 , align 1, !dbg !140
  br label %42, !dbg !141
; <label>:42
  %43 = load %struct._IO_FILE** @stdin , align 4, !dbg !142
  %44 = call i32 @_IO_getc ( %struct._IO_FILE* %43 ), !dbg !143
  store i32 %44 , i32* %c , align 4, !dbg !144
  %45 = add nsw i32 %30 , 1, !dbg !145
  store i32 %45 , i32* %i , align 4, !dbg !146
  br label %29, !dbg !147
; <label>:46
  %47 = icmp eq i32 %31 , 46, !dbg !148
  br i1 %47 , label %48 , label %74, !dbg !149
; <label>:48
  %49 = icmp slt i32 %30 , %lim, !dbg !150
  br i1 %49 , label %50 , label %52, !dbg !151
; <label>:50
  %51 = getelementptr inbounds i8* %s , i32 %30, !dbg !152
  store i8 46 , i8* %51 , align 1, !dbg !153
  br label %52, !dbg !154
; <label>:52
  %53 = add nsw i32 %30 , 1, !dbg !155
  store i32 %53 , i32* %i , align 4, !dbg !156
  %54 = load %struct._IO_FILE** @stdin , align 4, !dbg !157
  %55 = call i32 @_IO_getc ( %struct._IO_FILE* %54 ), !dbg !158
  store i32 %55 , i32* %c , align 4, !dbg !159
  br label %56, !dbg !160
; <label>:56
  %57 = phi i32 [ [%70, %69], [%53, %52] ]  // i
  %58 = phi i32 [ [%72, %69], [%55, %52] ]  // c
  %59 = icmp sge i32 %58 , 48, !dbg !161
  br i1 %59 , label %60 , label %62, !dbg !162
; <label>:60
  %61 = icmp sle i32 %58 , 57, !dbg !163
  br label %62
; <label>:62
  %63 = phi i1 [ [0, %56], [%61, %60] ]  // ?
  br i1 %63 , label %64 , label %73
; <label>:64
  %65 = icmp slt i32 %57 , %lim, !dbg !164
  br i1 %65 , label %66 , label %69, !dbg !165
; <label>:66
  %67 = trunc i32 %58 to i8, !dbg !166
  %68 = getelementptr inbounds i8* %s , i32 %57, !dbg !167
  store i8 %67 , i8* %68 , align 1, !dbg !168
  br label %69, !dbg !169
; <label>:69
  %70 = add nsw i32 %57 , 1, !dbg !170
  store i32 %70 , i32* %i , align 4, !dbg !171
  %71 = load %struct._IO_FILE** @stdin , align 4, !dbg !172
  %72 = call i32 @_IO_getc ( %struct._IO_FILE* %71 ), !dbg !173
  store i32 %72 , i32* %c , align 4, !dbg !174
  br label %56, !dbg !175
; <label>:73
  br label %74, !dbg !176
; <label>:74
  %75 = phi i32 [ [%58, %73], [%31, %46] ]  // c
  %76 = phi i32 [ [%57, %73], [%30, %46] ]  // i
  %77 = icmp slt i32 %76 , %lim, !dbg !177
  br i1 %77 , label %78 , label %82, !dbg !178
; <label>:78
  %79 = load %struct._IO_FILE** @stdin , align 4, !dbg !179
  %80 = call i32 @ungetc ( i32 %75, %struct._IO_FILE* %79 ), !dbg !180
  %81 = getelementptr inbounds i8* %s , i32 %76, !dbg !181
  store i8 0 , i8* %81 , align 1, !dbg !182
  store i32 48 , i32* %retval , align 4, !dbg !183
  br label %96, !dbg !184
; <label>:82
  br label %83, !dbg !185
; <label>:83
  %84 = phi i32 [ [%92, %90], [%75, %82] ]   // c
  %85 = icmp ne i32 %84 , 10, !dbg !186
  br i1 %85 , label %86 , label %88, !dbg !187
; <label>:86
  %87 = icmp ne i32 %84 , 113, !dbg !188
  br label %88
; <label>:88
  %89 = phi i1 [ [0, %83], [%87, %86] ]  // ?
  br i1 %89 , label %90 , label %93
; <label>:90
  %91 = load %struct._IO_FILE** @stdin , align 4, !dbg !189
  %92 = call i32 @_IO_getc ( %struct._IO_FILE* %91 ), !dbg !190
  store i32 %92 , i32* %c , align 4, !dbg !191
  br label %83, !dbg !192
; <label>:93
  %94 = sub nsw i32 %lim , 1, !dbg !193
  %95 = getelementptr inbounds i8* %s , i32 %94, !dbg !194
  store i8 0 , i8* %95 , align 1, !dbg !195
  store i32 57 , i32* %retval , align 4, !dbg !196
  br label %96
; <label>:96
  %97 = phi i32 [ [57, %93], [48, %78] ]  // retval
  br label %98
; <label>:98
  %99 = phi i32 [ [%97, %96], [%13, %24] ] // retval
  ret i32 %99, !dbg !197 }


*/

/*
SDGSlicer:

Backward Static SliceTable:
 Variable      SrcLineNumbers  
------------------------------
 c@getop             {"calc.c: [16,23,24,26,53,54,83,123,130,131,133,136,144,145,146,148,152,153,156,158,162,163,164,166,170,171,175,183,185]"}
 f@push              {"calc.c: [16,23,24,26,31,37,43,49,53,54,57,66,83,87]"}
 i@getop             {"calc.c: [16,23,24,26,53,54,83,123,130,131,133,136,144,145,146,148,152,153,156,158,162,163,164,166,170,171]"}
 lim@getop           {"calc.c: [16,23,24,26,53,54,83,123]"}
 num1@main           {"calc.c: [16,23,24,26,30,35,41,47,53,54,64,83]"}
 num2@main           {"calc.c: [16,23,24,26,36,42,48,53,54,56,83]"}
 op2@main            {"calc.c: [16]"}
 retval@getop        {"calc.c: [16,23,24,26,53,54,83,123,130,131,133,136,138,144,145,146,148,152,153,156,158,162,163,164,166,170,171,175,179,183,185,188]"}
 retval@pop          {"calc.c: [16,23,24,26,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,87,90,92,97,101,105,107,112,113]"}
 s@getop             {"calc.c: [16,23,24,26,53,54,83,123,130,131,133,136,144,145,146,148,152,153,156,158,162,163,164,166,170,171,175,178,183,185,187]"}
 s@main              {"calc.c: [16]"}
 sp                  {"calc.c: [16,23,24,26,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,87,90,92,97,101,105,107,112]"}
 type@main           {"calc.c: [16,23,24,26,53,54,83]"}
 val                 {"calc.c: [16,23,24,26,31,35,36,37,41,42,43,47,48,49,53,54,56,57,64,66,83,87,90,92,97,101,105,107,112]"}

*/
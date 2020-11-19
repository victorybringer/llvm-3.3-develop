void main(){
  int v1, v2, v3, v4, v5,n;
  v1 = 1;
  v2 = 3;
  v3 = 5;
  v4 = 6;
  v5 = 9;	
  printf("Enter a positive number: ");
  scanf("%d", &n);
	while (n <= 6) { 
		while (n <= 6) { 
			while (n <= 6) { 
				while (n <= 6) { 
					while (n <= 6) { 
						while (n <= 6) {
							while (n <= 6) { 
								while (n <= 6) {
									while (n <= 6) {
										while (n <= 6) { 
											while (n <= 6) { 
												while (n <= 6) { 
													v1  =  v2;
													v2  =  v3;
													v3  =  v4;
													v4  =  v5;
													v5  =  v1;
												}												
											  v1  =  v2;
												v2  =  v3;
												v3  =  v4;
												v4  =  v5;
												v5  =  v1;
											}

									    v1  =  v2;
											v2  =  v3;
											v3  =  v4;
											v4  =  v5;
											v5  =  v1;
										}
										
							      v1  =  v2;
										v2  =  v3;
										v3  =  v4;
										v4  =  v5;
										v5  =  v1;
									}

					        v1  =  v2;
									v2  =  v3;
									v3  =  v4;
									v4  =  v5;
									v5  =  v1;
								}

			          v1  =  v2;
								v2  =  v3;
								v3  =  v4;
								v4  =  v5;
								v5  =  v1;
							}
							
	            v1  =  v2;
							v2  =  v3;
							v3  =  v4;
							v4  =  v5;
							v5  =  v1;
						}

            v1  =  v2;
						v2  =  v3;
						v3  =  v4;
						v4  =  v5;
						v5  =  v1;
					}
					
          v1  =  v2;
					v2  =  v3;
					v3  =  v4;
					v4  =  v5;
					v5  =  v1;
				}

				v1  =  v2;
				v2  =  v3;
				v3  =  v4;
				v4  =  v5;
				v5  =  v1;
			}

			v1  =  v2;
			v2  =  v3;
			v3  =  v4;
			v4  =  v5;
			v5  =  v1;
		}

		v1  =  v2;
		v2  =  v3;
		v3  =  v4;
		v4  =  v5;
		v5  =  v1;
	}
	printf("v5=%d\n",v5);
}



/*
Variable      Labels  
----------------------
 v1@main        {3,4,5,6,7,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,29,30,31,32,36,37,38,39,43,44,45,46,50,51,52,53,57,58,59,60,64,65,66,67,71,72,73,74,78,79,80,81,85,86,87,88,92,93,94,95,98,99,100,101,102}
 v2@main        {  4,5,6,7,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,29,30,31,32,36,37,38,39,43,44,45,46,50,51,52,53,57,58,59,60,64,65,66,67,71,72,73,74,78,79,80,81,85,86,87,88,92,93,94,95,   99,100,101,102}
 v3@main        {    5,6,7,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,29,30,31,32,36,37,38,39,43,44,45,46,50,51,52,53,57,58,59,60,64,65,66,67,71,72,73,74,78,79,80,81,85,86,87,88,92,93,94,95,   99,100,101,102}
 v4@main        {5,6,7,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,29,30,31,32,36,37,38,39,43,44,45,46,50,51,52,53,57,58,59,60,64,65,66,67,71,72,73,74,78,79,80,81,85,86,87,88,92,93,94,95,99,100,101,102}
 v5@main        {5,6,7,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,29,30,31,32,36,37,38,39,43,44,45,46,50,51,52,53,57,58,59,60,64,65,66,67,71,72,73,74,78,79,80,81,85,86,87,88,92,93,94,95,99,100,101,102}

ProcTables:
fromList [("main",
Variable      Labels  
----------------------
 %v1@main        {38,40,42,44,46,62,67,71,75,79,83,87,91,95,99,103,108,110,112,113,119,121,123,124,130,132,134,135,141,143,145,146,152,154,156,157,163,165,167,168,174,176,178,179,185,187,189,190,196,198,200,201,207,209,211,212,218,220,222,223,227,229,231,233,234}
 %v2@main        {40,42,44,46,62,67,71,75,79,83,87,91,95,99,103,108,110,112,113,119,121,123,124,130,132,134,135,141,143,145,146,152,154,156,157,163,165,167,168,174,176,178,179,185,187,189,190,196,198,200,201,207,209,211,212,218,220,222,223,229,231,233,234}
 %v3@main        {42,44,46,62,67,71,75,79,83,87,91,95,99,103,108,110,112,113,119,121,123,124,130,132,134,135,141,143,145,146,152,154,156,157,163,165,167,168,174,176,178,179,185,187,189,190,196,198,200,201,207,209,211,212,218,220,222,223,229,231,233,234}
 %v4@main        {42,44,46,62,67,71,75,79,83,87,91,95,99,103,108,110,112,113,119,121,123,124,130,132,134,135,141,143,145,146,152,154,156,157,163,165,167,168,174,176,178,179,185,187,189,190,196,198,200,201,207,209,211,212,218,220,222,223,229,231,233,234}
 %v5@main        {42,44,46,62,67,71,75,79,83,87,91,95,99,103,108,110,112,113,119,121,123,124,130,132,134,135,141,143,145,146,152,154,156,157,163,165,167,168,174,176,178,179,185,187,189,190,196,198,200,201,207,209,211,212,218,220,222,223,229,231,233,234}
 @.str           {-1}
 @.str1          {-20}
 @.str2          {-24}
)]


-----------------------
@.str = private constant [26 x i8] [i8 69, i8 110, i8 116, i8 101, i8 114, i8 32, i8 97, i8 32, i8 112, i8 111, i8 115, i8 105, i8 116, i8 105, i8 118, i8 101, i8 32, i8 110, i8 117, i8 109, i8 98, i8 101, i8 114, i8 58, i8 32, i8 0]
@.str1 = private constant [3 x i8] [i8 37, i8 100, i8 0]
@.str2 = private constant [7 x i8] [i8 118, i8 53, i8 61, i8 37, i8 100, i8 10, i8 0]

define void @main ( ) {
 ; <label>:0
  %v1 = alloca i32 , align 4
  %v2 = alloca i32 , align 4
  %v3 = alloca i32 , align 4
  %v4 = alloca i32 , align 4
  %v5 = alloca i32 , align 4
  %n = alloca i32 , align 4
  store i32 1 , i32* %v1 , align 4, !dbg !5
  store i32 3 , i32* %v2 , align 4, !dbg !6
  store i32 5 , i32* %v3 , align 4, !dbg !7
  store i32 6 , i32* %v4 , align 4, !dbg !8
  store i32 9 , i32* %v5 , align 4, !dbg !9
  %1 = call i32 @printf ( i8* i8* getelementptr ( [26 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !10
  %3 = call i32 @scanf ( i8* i8* getelementptr ( [3 x i8]* @.str1 ,  i32 0, i32 0 ), i32* %n ), !dbg !11
  %.pre = load i32* %n , align 4, !dbg !12
  br label %5, !dbg !13
; <label>:5
  %6 = icmp sle i32 %.pre , 6, !dbg !14
  br i1 %6 , label %7 , label %89, !dbg !15
; <label>:7
  br label %8, !dbg !16
; <label>:8
  br i1 -1 , label %9 , label %84, !dbg !17
; <label>:9
  br label %10, !dbg !18
; <label>:10
  br i1 -1 , label %11 , label %79, !dbg !19
; <label>:11
  br label %12, !dbg !20
; <label>:12
  br i1 -1 , label %13 , label %74, !dbg !21
; <label>:13
  br label %14, !dbg !22
; <label>:14
  br i1 -1 , label %15 , label %69, !dbg !23
; <label>:15
  br label %16, !dbg !24
; <label>:16
  br i1 -1 , label %17 , label %64, !dbg !25
; <label>:17
  br label %18, !dbg !26
; <label>:18
  br i1 -1 , label %19 , label %59, !dbg !27
; <label>:19
  br label %20, !dbg !28
; <label>:20
  br i1 -1 , label %21 , label %54, !dbg !29
; <label>:21
  br label %22, !dbg !30
; <label>:22
  br i1 -1 , label %23 , label %49, !dbg !31
; <label>:23
  br label %24, !dbg !32
; <label>:24
  br i1 -1 , label %25 , label %44, !dbg !33
; <label>:25
  br label %26, !dbg !34
; <label>:26
  br i1 -1 , label %27 , label %39, !dbg !35
; <label>:27
  br label %28, !dbg !36
; <label>:28
  br i1 -1 , label %29 , label %34, !dbg !37
; <label>:29
  %30 = load i32* %v2 , align 4, !dbg !38
  store i32 %30 , i32* %v1 , align 4, !dbg !39
  %31 = load i32* %v3 , align 4, !dbg !40
  store i32 %31 , i32* %v2 , align 4, !dbg !41
  %32 = load i32* %v4 , align 4, !dbg !42
  store i32 %32 , i32* %v3 , align 4, !dbg !43
  %33 = load i32* %v5 , align 4, !dbg !44
  store i32 %33 , i32* %v4 , align 4, !dbg !45
  store i32 %30 , i32* %v5 , align 4, !dbg !46
  br label %28, !dbg !47
; <label>:34
  %35 = load i32* %v2 , align 4, !dbg !48
  store i32 %35 , i32* %v1 , align 4, !dbg !49
  %36 = load i32* %v3 , align 4, !dbg !50
  store i32 %36 , i32* %v2 , align 4, !dbg !51
  %37 = load i32* %v4 , align 4, !dbg !52
  store i32 %37 , i32* %v3 , align 4, !dbg !53
  %38 = load i32* %v5 , align 4, !dbg !54
  store i32 %38 , i32* %v4 , align 4, !dbg !55
  store i32 %35 , i32* %v5 , align 4, !dbg !56
  br label %26, !dbg !57
; <label>:39
  %40 = load i32* %v2 , align 4, !dbg !58
  store i32 %40 , i32* %v1 , align 4, !dbg !59
  %41 = load i32* %v3 , align 4, !dbg !60
  store i32 %41 , i32* %v2 , align 4, !dbg !61
  %42 = load i32* %v4 , align 4, !dbg !62
  store i32 %42 , i32* %v3 , align 4, !dbg !63
  %43 = load i32* %v5 , align 4, !dbg !64
  store i32 %43 , i32* %v4 , align 4, !dbg !65
  store i32 %40 , i32* %v5 , align 4, !dbg !66
  br label %24, !dbg !67
; <label>:44
  %45 = load i32* %v2 , align 4, !dbg !68
  store i32 %45 , i32* %v1 , align 4, !dbg !69
  %46 = load i32* %v3 , align 4, !dbg !70
  store i32 %46 , i32* %v2 , align 4, !dbg !71
  %47 = load i32* %v4 , align 4, !dbg !72
  store i32 %47 , i32* %v3 , align 4, !dbg !73
  %48 = load i32* %v5 , align 4, !dbg !74
  store i32 %48 , i32* %v4 , align 4, !dbg !75
  store i32 %45 , i32* %v5 , align 4, !dbg !76
  br label %22, !dbg !77
; <label>:49
  %50 = load i32* %v2 , align 4, !dbg !78
  store i32 %50 , i32* %v1 , align 4, !dbg !79
  %51 = load i32* %v3 , align 4, !dbg !80
  store i32 %51 , i32* %v2 , align 4, !dbg !81
  %52 = load i32* %v4 , align 4, !dbg !82
  store i32 %52 , i32* %v3 , align 4, !dbg !83
  %53 = load i32* %v5 , align 4, !dbg !84
  store i32 %53 , i32* %v4 , align 4, !dbg !85
  store i32 %50 , i32* %v5 , align 4, !dbg !86
  br label %20, !dbg !87
; <label>:54
  %55 = load i32* %v2 , align 4, !dbg !88
  store i32 %55 , i32* %v1 , align 4, !dbg !89
  %56 = load i32* %v3 , align 4, !dbg !90
  store i32 %56 , i32* %v2 , align 4, !dbg !91
  %57 = load i32* %v4 , align 4, !dbg !92
  store i32 %57 , i32* %v3 , align 4, !dbg !93
  %58 = load i32* %v5 , align 4, !dbg !94
  store i32 %58 , i32* %v4 , align 4, !dbg !95
  store i32 %55 , i32* %v5 , align 4, !dbg !96
  br label %18, !dbg !97
; <label>:59
  %60 = load i32* %v2 , align 4, !dbg !98
  store i32 %60 , i32* %v1 , align 4, !dbg !99
  %61 = load i32* %v3 , align 4, !dbg !100
  store i32 %61 , i32* %v2 , align 4, !dbg !101
  %62 = load i32* %v4 , align 4, !dbg !102
  store i32 %62 , i32* %v3 , align 4, !dbg !103
  %63 = load i32* %v5 , align 4, !dbg !104
  store i32 %63 , i32* %v4 , align 4, !dbg !105
  store i32 %60 , i32* %v5 , align 4, !dbg !106
  br label %16, !dbg !107
; <label>:64
  %65 = load i32* %v2 , align 4, !dbg !108
  store i32 %65 , i32* %v1 , align 4, !dbg !109
  %66 = load i32* %v3 , align 4, !dbg !110
  store i32 %66 , i32* %v2 , align 4, !dbg !111
  %67 = load i32* %v4 , align 4, !dbg !112
  store i32 %67 , i32* %v3 , align 4, !dbg !113
  %68 = load i32* %v5 , align 4, !dbg !114
  store i32 %68 , i32* %v4 , align 4, !dbg !115
  store i32 %65 , i32* %v5 , align 4, !dbg !116
  br label %14, !dbg !117
; <label>:69
  %70 = load i32* %v2 , align 4, !dbg !118
  store i32 %70 , i32* %v1 , align 4, !dbg !119
  %71 = load i32* %v3 , align 4, !dbg !120
  store i32 %71 , i32* %v2 , align 4, !dbg !121
  %72 = load i32* %v4 , align 4, !dbg !122
  store i32 %72 , i32* %v3 , align 4, !dbg !123
  %73 = load i32* %v5 , align 4, !dbg !124
  store i32 %73 , i32* %v4 , align 4, !dbg !125
  store i32 %70 , i32* %v5 , align 4, !dbg !126
  br label %12, !dbg !127
; <label>:74
  %75 = load i32* %v2 , align 4, !dbg !128
  store i32 %75 , i32* %v1 , align 4, !dbg !129
  %76 = load i32* %v3 , align 4, !dbg !130
  store i32 %76 , i32* %v2 , align 4, !dbg !131
  %77 = load i32* %v4 , align 4, !dbg !132
  store i32 %77 , i32* %v3 , align 4, !dbg !133
  %78 = load i32* %v5 , align 4, !dbg !134
  store i32 %78 , i32* %v4 , align 4, !dbg !135
  store i32 %75 , i32* %v5 , align 4, !dbg !136
  br label %10, !dbg !137
; <label>:79
  %80 = load i32* %v2 , align 4, !dbg !138
  store i32 %80 , i32* %v1 , align 4, !dbg !139
  %81 = load i32* %v3 , align 4, !dbg !140
  store i32 %81 , i32* %v2 , align 4, !dbg !141
  %82 = load i32* %v4 , align 4, !dbg !142
  store i32 %82 , i32* %v3 , align 4, !dbg !143
  %83 = load i32* %v5 , align 4, !dbg !144
  store i32 %83 , i32* %v4 , align 4, !dbg !145
  store i32 %80 , i32* %v5 , align 4, !dbg !146
  br label %8, !dbg !147
; <label>:84
  %85 = load i32* %v2 , align 4, !dbg !148
  store i32 %85 , i32* %v1 , align 4, !dbg !149
  %86 = load i32* %v3 , align 4, !dbg !150
  store i32 %86 , i32* %v2 , align 4, !dbg !151
  %87 = load i32* %v4 , align 4, !dbg !152
  store i32 %87 , i32* %v3 , align 4, !dbg !153
  %88 = load i32* %v5 , align 4, !dbg !154
  store i32 %88 , i32* %v4 , align 4, !dbg !155
  store i32 %85 , i32* %v5 , align 4, !dbg !156
  br label %5, !dbg !157
; <label>:89
  %90 = load i32* %v5 , align 4, !dbg !158
  %91 = call i32 @printf ( i8* i8* getelementptr ( [7 x i8]* @.str2 ,  i32 0, i32 0 ), i32 %90 ), !dbg !159
  ret void, !dbg !160 }


*/
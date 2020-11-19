#include "std_testcase.h"

#include <wchar.h>

#ifndef OMITBAD

/* bad function declaration */
void Use_After_Free_Malloc_Free_Char_case02_Bad_01(void * dataVoidPtr);

// @scene 释放后跨文件跨函数读取原指针
void Use_After_Free_Malloc_Free_Char_case02_01()
{
    char * data;
    /* Initialize data */
    data = NULL;
    data = (char *)malloc(100*sizeof(char));
    if (data == NULL) {exit(-1);}
    memset(data, 'A', 100-1);
    data[100-1] = '\0';
    /* Free data in the source - the bad sink attempts to use data */
    free(data);
    Use_After_Free_Malloc_Free_Char_case02_Bad_01(&data);
}

#endif /* OMITBAD */

#ifndef OMITGOOD

void Use_After_Free_Malloc_Free_Char_case02_Good_01(void * dataVoidPtr);

static void Use_After_Free_Malloc_Free_Char_case02_02()
{
    char * data;
    /* Initialize data */
    data = NULL;
    data = (char *)malloc(100*sizeof(char));
    if (data == NULL) {exit(-1);}
    memset(data, 'A', 100-1);
    data[100-1] = '\0';
    /* FIX: Do not free data in the source */
    Use_After_Free_Malloc_Free_Char_case02_Good_01(&data);
}

/* goodB2G uses the BadSource with the GoodSink */
void Use_After_Free_Malloc_Free_Char_case02_Good_02(void * dataVoidPtr);

// @scene 内存释放后不再读取内存指向的内存值 
static void Use_After_Free_Malloc_Free_Char_case02_03()
{
    char * data;
    /* Initialize data */
    data = NULL;
    data = (char *)malloc(100*sizeof(char));
    if (data == NULL) {exit(-1);}
    memset(data, 'A', 100-1);
    data[100-1] = '\0';
    /* Free data in the source - the bad sink attempts to use data */
    free(data);
    Use_After_Free_Malloc_Free_Char_case02_Good_02(&data);
}

void Use_After_Free_Malloc_Free_Char_case02_04()
{
    Use_After_Free_Malloc_Free_Char_case02_02();
    Use_After_Free_Malloc_Free_Char_case02_03();
}

#endif /* OMITGOOD */

/* Below is the main(). It is only used when building this testcase on
   its own for testing or for building a binary to use in testing binary
   analysis tools. It is not used when compiling all the testcases as one
   application, which is how source code analysis tools are tested. */

#ifdef INCLUDEMAIN

int main(int argc, char * argv[])
{
    /* seed randomness */
    srand( (unsigned)time(NULL) );
#ifndef OMITGOOD
    printLine("Calling good()...");
    Use_After_Free_Malloc_Free_Char_case02_04();
    printLine("Finished good()");
#endif /* OMITGOOD */
#ifndef OMITBAD
    printLine("Calling bad()...");
    Use_After_Free_Malloc_Free_Char_case02_01();
    printLine("Finished bad()");
#endif /* OMITBAD */
    return 0;
}

#endif

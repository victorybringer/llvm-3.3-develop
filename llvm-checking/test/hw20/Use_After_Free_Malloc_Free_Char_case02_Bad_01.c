/*
 * @description 释放后读取，跨文件sink。搭配Use_After_Free_Malloc_Free_Char_case02.c文件使用
 * 
 * @good 
 * 
 * @bad 
 * 
 * @label CWE:415;Coverity:USE_AFTER_FREE;TOPN
 * 
 * @author wwx564328;x00407107
 * 
 */

#include "std_testcase.h"

#include <wchar.h>

#ifndef OMITBAD

void Use_After_Free_Malloc_Free_Char_case02_Bad_01(void * dataVoidPtr)
{
    /* cast void pointer to a pointer of the appropriate type */
    char * * dataPtr = (char * *)dataVoidPtr;
    /* dereference dataPtr into data */
    char * data = (*dataPtr);
    /* POTENTIAL FLAW: Use of data that may have been freed */
    printLine(data);
}

#endif /* OMITBAD */

#ifndef OMITGOOD

/* goodG2B uses the GoodSource with the BadSink */
void Use_After_Free_Malloc_Free_Char_case02_Good_01(void * dataVoidPtr)
{
    /* cast void pointer to a pointer of the appropriate type */
    char * * dataPtr = (char * *)dataVoidPtr;
    /* dereference dataPtr into data */
    char * data = (*dataPtr);
    /* Use of data that may have been freed */
    printLine(data);
}

/* goodB2G uses the BadSource with the GoodSink */
void Use_After_Free_Malloc_Free_Char_case02_Good_02(void * dataVoidPtr)
{
    /* cast void pointer to a pointer of the appropriate type */
    char * * dataPtr = (char * *)dataVoidPtr;
    /* dereference dataPtr into data */
    char * data = (*dataPtr);
    /* FIX: Don't use data that may have been freed already */
    /* POTENTIAL INCIDENTAL - Possible memory leak here if data was not freed */
    /* do nothing */
	/* empty statement needed for some flow variants */
}

#endif /* OMITGOOD */

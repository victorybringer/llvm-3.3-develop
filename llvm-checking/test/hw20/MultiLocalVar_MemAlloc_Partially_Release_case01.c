/*
 * @description RESOURCE_LEAK 查找程序没有尽快释放系统资源的情况
 *
 * @good test_good01
 *
 * @bad test_bad01;test_bad02
 *
 * @label Coverity:RESOURCE_LEAK;topn;CWE:404;icsl
 *
 * @author r00369861;x00407107
 *
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int a;
    char *p;
} tStrc1;

typedef struct {
    int a;
    tStrc1 *ptStrc1;
} tStrc;

tStrc1 *allocMem(int num)
{
    tStrc1 *pStr1 = NULL;
    pStr1 = (tStrc1 *)malloc(num * sizeof(tStrc1));
    if (pStr1 == NULL) {
        return NULL;
    }


    pStr1[0].p = (char *)malloc(10);
    if (pStr1[0].p == NULL) {
        free(pStr1);
        return NULL;
    }

    pStr1[1].p = (char *)malloc(10);
    if (pStr1[1].p == NULL) {
        free(pStr1[0].p);
        free(pStr1);
        return NULL;
    }

    return pStr1;
}

// 没有释放成员变量内的内存
void test1_freeMem(tStrc1 *ptr)
{
    free(ptr);
    /* POTENTIAL FLAW: partial memory is leaked */
}

// 成员变量的内存没有释放全
void test2_freeMem(tStrc1 *ptr)
{
    free(ptr->p);
    free(ptr);
    /* POTENTIAL FLAW: partial memory is leaked */
}

void test3_freeMem(tStrc1 *ptr)
{
    free(ptr[0].p);
    free(ptr[1].p);
    free(ptr);
}

// @scene 跨函数释放内存，没有完全释放成员变量指向的内存
int test_bad_02(int argc, char *argv[])
{
    tStrc tSts = { 0 };
    int num = 2;
    tSts.ptStrc1 = allocMem(num);
    test2_freeMem(tSts.ptStrc1);
    system("pause");

    return 0;
}


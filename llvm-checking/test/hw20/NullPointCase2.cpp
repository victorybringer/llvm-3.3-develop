#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

struct PERSON {
    char *name;
    char *gender;
    unsigned int age;
};

struct TEACHER : public PERSON {
    unsigned int id;
    char *name;
};

struct STU : public PERSON {
    unsigned int id;
    char *name;
    struct TEACHER *teacher;
};

/*
 * @mainScene 开始事件：dynamic_cast，结束事件：指针解引用运算
 * @subScene 强转后赋值后未校验直接访问成员
 */
void forwardNullBadTestCpp(struct STU *stu)
{
    // 开始事件，dynamic_cast - 从动态转换返回了 NULL。到指针类型的动态转换可能是有意返回 NULL。
    // 中间事件，alias_transfer - 变量被赋予可能为 null 的指针
    struct PERSON *person = dynamic_cast<struct PERSON *>(stu);
    /* POTENTIAL FLAW: 结束事件，var_deref_op - null 指针解引用运算，访问成员变量 */
    person->gender;
}
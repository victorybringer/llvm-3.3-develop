
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "securec.h"
/*
* @Scene: 使用安全函数格式化输入时，变参中的缓冲区长度参数超过目的缓冲区实际大小
* @SubScene: 使用函数封装的vsscanf_s时,格式串类型大小超出实际变参类型大小
*/
void cwe120_c_203(char *buf, size_t bufSize, char *msg)
{
    char c;
    /* POTENTIAL FLAW:%d格式写入4字节数据，超出char类型的大小，char对应的格式为%hhd */
    scanf_s("%d", &c);
}
int main(int argc, char **argv)
{
    char buf[16];
    if (argc > 1) {
        cwe120_c_203(buf, sizeof(buf), argv[1]);
    }
    return 0;
}


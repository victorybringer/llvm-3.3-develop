#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "securec.h"

/*
* @Scene: 使用安全函数格式化输出时，destMax参数超过目的缓冲区实际大小
* @SubScene: 使用安全函数snprintf_s时,destMax参数超过目的缓冲区实际大小
*/
void cwe120_c_211(char *buf, size_t bufSize, char *msg)
{
    size_t len = strlen(msg);
    size_t count = (len > 0 ? len - 1 : 0);
    /* POTENTIAL FLAW:destmax的长度可能大于dest的长度 */
    snprintf_s(buf, len, count, "%s", msg);
}
int main(int argc, char **argv)
{
    char buf[16];
    if (argc > 1) {
        cwe120_c_211(buf, sizeof(buf), argv[1]);
    }
    return 0;
}


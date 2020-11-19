
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

// 固定长度数组
char cTestData[256] = {0};

/*
* @Scene: memcpy缓冲区溢出
* @SubScene: 对入参指针有判断，但长度值没有判断
*/
static void  TestCaseBad01(char *rsaKey)
{
    char testDigestBuffer[32] = {0};

    if (rsaKey == NULL) {
        return;
    }

    /* POTENTIAL FLAW: buffer_size: You might overrun the 32 byte destination string "testDigest.t.buffer" by writing the maximum 256 bytes from "c_testData"  */
    memcpy((void *)testDigestBuffer, (void *)cTestData, sizeof(cTestData));
}
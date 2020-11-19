#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>


typedef unsigned char   UINT8;
typedef unsigned char   UCHAR;
typedef unsigned short  UINT16;
typedef unsigned int    UINT32;

#define TYPE_LENGTH 2

typedef struct {
    UINT16 usType;
    UINT16 usLength;
}MY_MSG;


void GetNextLen(MY_MSG *pMsg, UINT16 *usLength)
{
    *usLength = (UINT16)(TYPE_LENGTH + pMsg->usLength); //  = 0xFFFD
}


void testCase01(MY_MSG *pMsg)
{
    UINT16 pucPos= 0;
    UINT8 usBytesLeft = 100;
    UINT16 usLength = 0;

    /*POTENTIAL FLAW: usLength 被外部输入参数参与运行 可能溢出为0 导致死循环*/
    while(usBytesLeft > 0)
    {
        GetNextLen(pMsg, &usLength);

        pucPos += usLength;

        /*POTENTIAL FLAW: usLength 被外部输入参数参与运行 可能溢出为0 导致死循环*/
        usBytesLeft = usBytesLeft - usLength;
    }
}




int main(int argc, char *argv[])
{
    MY_MSG *pMsg = (MY_MSG *)argv[2];
    testCase01(pMsg);
    return 0;
}
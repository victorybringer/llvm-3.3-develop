
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define E_XXX_BUTT 2
#define GPP_HEAD_LEN 20
#define USER_DATA_MAX_LEN 0x75e

typedef struct {
	unsigned short usHType;
	unsigned short usHLength;
}XXX_XX_STRU;

//InWhile_BufSizeUnKnown_PtrCalcNonConst_InvalidCheck_StructField_OOB_case01
//While循环体内，BufSize未知，非常数指针运算，校验不足,导致结构体域访问OOB   N  场景适配高误报

void messageLenNotCheck_01(unsigned char* pucInXxxMsg, unsigned short usXxxType)
{
    XXX_XX_STRU* pstXxxHeadPtr = NULL;
    unsigned char* pucXxxPtr = NULL;
    unsigned short usGppLen = 0;
    unsigned short usCurXxxType = E_XXX_BUTT;
    unsigned long ulDecodeMsgLen = 0;

    pucXxxPtr = pucInXxxMsg;
    pstXxxHeadPtr = (XXX_XX_STRU*)pucXxxPtr;
    usCurXxxType = pstXxxHeadPtr->usHType;
    usGppLen = pstXxxHeadPtr->usHLength;

    while ((usCurXxxType < E_XXX_BUTT) && (usCurXxxType != usXxxType))
    {
        if (usGppLen < GPP_HEAD_LEN)
        {
            return;
        }

        ulDecodeMsgLen +=  usGppLen;
		// POTENTIAL FLAW:
        if (ulDecodeMsgLen > USER_DATA_MAX_LEN)
        {
            return;
        }

        pstXxxHeadPtr = (XXX_XX_STRU*)((char*)pstXxxHeadPtr + usGppLen);
        usGppLen = pstXxxHeadPtr->usHLength;
        usCurXxxType = pstXxxHeadPtr->usHType;
    }
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        return 0;
    }
	char* pucInXxxMsg = argv[1];
	short usXxxType = atoi(argv[2]);
	messageLenNotCheck_01(pucInXxxMsg, usXxxType);
	return 0;
}
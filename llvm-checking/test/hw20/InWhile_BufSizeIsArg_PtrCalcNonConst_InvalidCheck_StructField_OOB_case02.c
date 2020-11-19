
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

typedef unsigned long ULONG;
typedef unsigned char UINT8;
typedef unsigned long UINT32;
typedef unsigned short USHORT;

typedef struct {
    UINT32 xxx;
    UINT8 ucRestartCount;
}MY_MSG1;

typedef struct {
    USHORT uwLen;
    USHORT uwExtenID;
}MY_MSG2;

#define BOOL int
#define TRUE 1
#define FALSE 0

//InWhile_BufSizeIsArg_PtrCalcWithNonConst_InvalidCheck_StructField_OOB_case1
//While循环内，BufSize为参数，非常数指针运算，校验不足,结构体域访问OOB Y
BOOL  testcase04(void *pPData, UINT32 udwPLen, UINT8 *pucValue)
{
    MY_MSG1 *psRecovery = NULL;
    UINT32 udwOffset = 0;
    UINT8  ucIeType = 0;
    MY_MSG2 *psPIe = NULL;
      
    *pucValue = 0;

    while (udwOffset < udwPLen)
    {
        ucIeType = *(UINT8 *)((UINT8 *)pPData + udwOffset);
        switch(ucIeType)
        {
            case 0xAA:
                psPIe = (MY_MSG2 *)((UINT8 *)pPData + udwOffset);
                
                /*POTENTIAL FLAW: 
                    No check to ensure that udwOffset + sizeof(MY_MSG1) <= udwPLen
                    so if udwOffset == udwPLen-1, then you can get a OOB Read
                */
                udwOffset += (sizeof(MY_MSG2) + psPIe->uwLen - sizeof(psPIe->uwExtenID));
                break;
            case 0xBB:
                psRecovery = (MY_MSG1*)((UINT8 *)pPData + udwOffset);
                /*POTENTIAL FLAW: 
                    No check to ensure that udwOffset + sizeof(MY_MSG1) <= udwPLen
                    so if udwOffset == udwPLen-1, then you can get a OOB Read
                */
                *pucValue = psRecovery->ucRestartCount;
                return TRUE;
            default: 
                return FALSE;
        }
    }

    return FALSE;  
}

int main(int argc, char *argv[])
{
    UINT8 ucValue = 0;
    if(argc < 2)
		return -1;
	testcase04((UINT8 *)argv[1], strlen(argv[1]), &ucValue);
    return 0;
}

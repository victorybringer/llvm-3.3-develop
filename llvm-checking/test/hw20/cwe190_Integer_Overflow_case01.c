#include <stdio.h>

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;

typedef enum {
	TYPE1,
	TYPE2,
	TYPE3
}TYPE_ENUM;

#define AA_CODEBIT (0xD5)
#define AA_CODE_SIZE(x)  (UINT32)(1+(UINT32)(0 == ((*(UINT8 *)(x)) & AA_CODEBIT)))

void AA_MsgID(UINT8* pInData, UINT16* ID, UINT8* IDlen, UINT32 ulRecvLen);



UINT32 Recv_Msg(UINT8* pInData, UINT32 ulRecvLen)
{
    UINT32 ulMsgLen = 0;
    UINT16 usCID = 0;
    UINT8 ucCIDLen = 0;

    AA_MsgID(pInData, &usCID, &ucCIDLen, ulRecvLen);
	// POTENTIAL FLAW: ucCIDLen is not checked, this can underflow
    ulMsgLen = ulRecvLen - ucCIDLen;  
	
	return ulMsgLen;
}


UINT32 XX_RecvParse(UINT8 *pucRecvPkt, UINT32 ulMsgLen)
{
	UINT32 ulProcLen = 0;
	UINT8 ucOptionType = 0;
	UINT8 ucOptionLen = 0;
	
	UINT8 *pData = pucRecvPkt;
	
    while(ulProcLen < ulMsgLen)    // THIS CAN POTENTIALLY LOOP MANY TIMES
    {
        ucOptionType = *pData >> 4;
        ucOptionLen = *pData & 0x0F;

        if(4 == ucOptionType) {
            return TYPE3;
        }

        if(5 == ucOptionType) {
            return TYPE3;
        }
		
        pData += (ucOptionLen + 1);    // THIS POINTER CAN BE INCREMENTED BEYOND END OF PACKET
        ulProcLen += (ucOptionLen + 1);
    }
	
	return ulProcLen;
}


UINT32 TestCase_Bad01(UINT8 *pInData)
{
	UINT32 ulMsgLen = 0;
	UINT32 ulRet = 0;
	UINT8 *pTempStr = NULL;
	
	pTempStr = pInData;
	ulMsgLen = (1 == AA_CODE_SIZE(pTempStr) ) ? (*pTempStr & AA_CODEBIT) : *(pTempStr + 1);
				 
	pTempStr += AA_CODE_SIZE(pTempStr);
	ulRet = Recv_Msg(pTempStr, ulMsgLen);
	
	XX_RecvParse(pTempStr, ulRet);
}

int main(int argc, char *argv[])
{
	TestCase_Bad01(argv[0]);
	return 0;
}

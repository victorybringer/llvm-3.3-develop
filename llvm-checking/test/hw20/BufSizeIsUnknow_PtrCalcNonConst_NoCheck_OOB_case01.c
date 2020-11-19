//------------------------------
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	unsigned long ulInfoLen;
	unsigned long ulId;
}RRE_LOAD_ACK_S;

typedef struct {
	unsigned long ulNeNum;
	char *acNeInfo;
}RRE_LOAD_FILE_ACK_S;

int *LOAD_YY(unsigned long ulId);

//BufSizeIsUnknow_PtrCalcNonConst_NoCheck_OOB_case01
//BufSize未知，非常数指针运算，未校验OOB导致  Y
void LOAD_XXX(RRE_LOAD_FILE_ACK_S *pstMsg)
{
	unsigned int i;
	RRE_LOAD_ACK_S *pstOxxN;
	int *pstLoc;
	pstOxxN = (RRE_LOAD_ACK_S *)pstMsg->acNeInfo;

	// Potential Flaw: In the for loop, external data 'pstOxxN->ulInfoLen' is used for pointer offset,
	// it would result in a out-of-bounds read
	for (i = 0; i < pstMsg->ulNeNum; i++, pstOxxN = (RRE_LOAD_ACK_S*)((char*) pstOxxN + pstOxxN->ulInfoLen)) {
		pstLoc = LOAD_YY(pstOxxN->ulId);
		// do something
	}
}

int main(int argc, char *argv[])
{
    if(argc < 2)
		return -1;
	LOAD_XXX((RRE_LOAD_FILE_ACK_S *)argv[1]);
	return 0;
}
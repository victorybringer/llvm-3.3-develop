
#include "String_Overflow_Strcpy_UseExLen_case01.h"
#include "securec.h"
/* 该头文件用于main函数 */
#include "getmsg.h"


#define MAX_UTL_FILE_NAME_LEN 128
/* B4 - 函数入参为void* char*变长报文 */
/*
* @Scene:  函数入参为void* char*变长报文
*/
INT32  UtlFileCopyNameBad01(UINT8 *dst, UINT8 *src)
{
    UINT8 file_temp[MAX_UTL_FILE_NAME_LEN] = {0};
    if ((NULL  == dst || NULL == src)) {
        return  -1; 
    }
    /* C6 - 系统函数的返回值 */
    /* C0 - if(函数返回值) */
    if (strlen((char *)src) < MAX_UTL_FILE_NAME_LEN) {
        /* POTENTIAL FLAW: 逐个字符按外部报文提供的长度拷贝导致的缓冲区写溢出 */
        strncpy((char *)file_temp, (char *)dst, strlen((char *)dst)); 
        return 0;
    }
    return   -1;   
}

UINT32 FileFcopy(TLV *file, UINT32 len)
{
    UINT8 dest_filename[256];
    if (len < (sizeof(TLV) + 1) || len - sizeof(TLV) < file->length || file->length > 256 || file->length == 0) {
        return 1;
    }
    memcpy_s(dest_filename, sizeof(dest_filename), file->data, strlen((char *)(file->data)));
    if (0 != UtlFileCopyNameBad01(dest_filename, file->data)) {
        return 1;
    }
    return 0;
}

int main(int argc, char* argv[])
{
    INT32 result;
    INT32 fd = 0;
    UNIONPARAM params[MAX_PARAM_NUMBER];
    result = readbytes(fd, (UINT8 *)params, MAX_PARAM_NUMBER * sizeof(UNIONPARAM));
    if (result < 0) {
        return result;
    }
    (void)FileFcopy((TLV*)params[0].tlv.data, params[0].tlv.length);
    return 0;
}
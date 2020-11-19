/*
 * @description 整型溢出导致死循环
 *
 * @bad send_ipc_msg_to_sru
 *
 */

#include <stdio.h>
#include "common_structures.h"

/* B4 - 函数入参为void*char*变长报文 */
VOID send_ipc_msg_to_sru(UINT8* pData, UINT32 len)
{
    UINT32 ulMsgDataLen = 0;

    UINT32 ulRecordIDNum = 0;
    UINT32 ulVCpuNum = 0;

    UINT32* pMsgDataCur = NULL;
    UINT32* pulDataTemp = NULL;

    if (len < sizeof(TLV)) {
        return;
    }
    ulMsgDataLen = len - sizeof(TLV);

    pulDataTemp = (UINT32*)(pData + sizeof(TLV));
    pMsgDataCur = pulDataTemp;

    while (pMsgDataCur < (pulDataTemp + (ulMsgDataLen / sizeof(UINT32)))) {
        /* B5 - 获取的特定位置数据给局部变量(如指针偏移取值) */
        ulRecordIDNum = *(pMsgDataCur + 2);
        ulVCpuNum = *(pMsgDataCur + 3);

        /* POTENTIAL FLAW: 此处溢出导致死循环 */
        pMsgDataCur = pMsgDataCur + 1 + 1 + 1 + 1 + ulRecordIDNum * (1 + 1) + ulVCpuNum * (1 + 1);
    }
}

/* TAINTED SOURCE */
int main(int argc, char** argv)
{
    UNIONPARAM *params = (UNIONPARAM *)argv[1];
    (void)send_ipc_msg_to_sru((UINT8*)params->tlv.data, params->tlv.length);
}

/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2018-2020. All rights reserved.
 *
 * @description buffer from external is converted to struct type and with no check, which could cause buffer overflow
 *
 * @bad test17_c
 *
 * @label wukong:Buffer_OverFlow_Checker;SecBrella:SecL_BufferOverFlow;CWE:119;TopN:BufferOverFlow:2.1.2.1;csec;cwe-119
 *
 * @author z00500735
 */

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "securec.h"


typedef struct
{
    unsigned long ulSrcAddr;
    unsigned long ulDstAddr;
    unsigned char ucHeaderLength;
}PPUB_SCTP_IPV4HDR_S;
typedef struct
{
    unsigned int i;
}PPUB_SCTP_PACKET_HEADER_ST;
extern int recv(int, char *, int, int);

/*
* @Scene: 结构体域值访问越界
* @SubScene: 外部数据的长度未校验或校验不足，强转为结构体并访问其域值 
*/
void test17_c()
{
    int len;
    unsigned int ulIpHeaderOffset;
    unsigned char pucPacket[256] = {0};
    PPUB_SCTP_PACKET_HEADER_ST *pstSctpHeader = NULL;
    PPUB_SCTP_IPV4HDR_S *pstIpV4Header = NULL;
    unsigned long ulFromAddrs = 0;
    unsigned long ulToAddrs = 0;
    
    len = recv(0, pucPacket, 256, 0); //VOS_Recv
    if (len <= 0) {
        return;
    } else {
      pstIpV4Header = (PPUB_SCTP_IPV4HDR_S*) pucPacket;

      /* POTENTIAL FLAW: 通过VOS_Recv获取到的数据，未校验数据长度，直接进行结构体强转并访问成员，可能造成读越界  */
      ulIpHeaderOffset = sizeof(unsigned int) * pstIpV4Header->ucHeaderLength;  //error

      /* POTENTIAL FLAW: 通过VOS_Recv获取到的数据，未校验数据长度，直接进行结构体强转并访问成员，可能造成读越界  */
      ulFromAddrs = pstIpV4Header->ulSrcAddr;  //error

      /* POTENTIAL FLAW: 通过VOS_Recv获取到的数据，未校验数据长度，直接进行结构体强转并访问成员，可能造成读越界  */
      ulToAddrs = pstIpV4Header->ulDstAddr;  //error

      /* POTENTIAL FLAW: 通过VOS_Recv获取到的数据，未校验数据长度，直接进行结构体强转并访问成员，可能造成读越界  */
      pstSctpHeader = (PPUB_SCTP_PACKET_HEADER_ST*)(pucPacket + ulIpHeaderOffset);  //error
    
    }

}



int main(int argc, char **argv)
{
    test17_c();
	return 0;
}



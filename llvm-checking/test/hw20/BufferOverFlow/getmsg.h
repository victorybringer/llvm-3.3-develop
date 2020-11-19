#ifndef GETMSG_H
#define GETMSG_H
#include "base_type.h"
#include "common_structures.h"
INT32 readbytes(INT32 fd, unsigned char *buf, INT32 len);
INT32 readuint8(INT32 fd, UINT8 *p_u8);
INT32 readuint16(INT32 fd, UINT16 *p_u16);
INT32 readuint32(INT32 fd, UINT32 *p_u32);
#endif// GETMSG_H
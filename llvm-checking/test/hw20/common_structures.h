#ifndef __TEST_CASE_STRUCT_DEFINE_
#define __TEST_CASE_STRUCT_DEFINE_

#ifdef __cplusplus
extern "C" {
#endif

#include "base_type.h"
#define ARRSIZ 16
#define BASICTYPES \
    char   *p_ch;   \
    INT8   *p_i8;   \
    UINT8  *p_u8;   \
    INT16  *p_i16;  \
    UINT16 *p_u16;  \
    INT32  *p_i32;  \
    UINT32 *p_u32;  \
    LONG   *p_lon;  \
    ULONG  *p_ulo;  \
    INT64  *p_i64;  \
    UINT64 *p_u64;  \
    VOID   *p_void; \
    char   ch;   \
    INT8   i8;   \
    UINT8  u8;   \
    INT16  i16;  \
    UINT16 u16;  \
    INT32  i32;  \
    UINT32 u32;  \
    LONG   lon;  \
    ULONG  ulo;  \
    INT64  i64;  \
    UINT64 u64;  \
    char   *pa_ch[ARRSIZ];\
    INT8   *pa_i8[ARRSIZ];\
    UINT8  *pa_u8[ARRSIZ];\
    INT16  *pa_i16[ARRSIZ];\
    UINT16 *pa_u16[ARRSIZ];\
    INT32  *pa_i32[ARRSIZ];\
    UINT32 *pa_u32[ARRSIZ];\
    LONG   *pa_lon[ARRSIZ];\
    ULONG  *pa_ulo[ARRSIZ];\
    INT64  *pa_i64[ARRSIZ];\
    UINT64 *pa_u64[ARRSIZ];\
    VOID   *pa_void[ARRSIZ];\
    char   a_ch[ARRSIZ];\
    INT8   a_i8[ARRSIZ];\
    UINT8  a_u8[ARRSIZ];\
    INT16  a_i16[ARRSIZ];\
    UINT16 a_u16[ARRSIZ];\
    INT32  a_i32[ARRSIZ];\
    UINT32 a_u32[ARRSIZ];\
    LONG   a_lon[ARRSIZ];\
    ULONG  a_ulo[ARRSIZ];\
    INT64  a_i64[ARRSIZ];\
    UINT64 a_u64[ARRSIZ];
// һ��
// ��msg[0] - STRUCT_ML1, *PSTRUCT_ML1
typedef struct stSTRUCT_ML1
{
    BASICTYPES; \
    struct stSTRUCT_ML1 *p_prev;\
    struct stSTRUCT_ML1 *p_next;\
    unsigned char msg[0]; \
}STRUCT_ML1, *PSTRUCT_ML1;
//����msg[0]-STRUCT_L1, *PSTRUCT_L1
typedef struct stSTRUCT_L1
{
    BASICTYPES; \
    struct stSTRUCT_L1 *p_prev; \
    struct stSTRUCT_L1 *p_next; \
}STRUCT_L1, *PSTRUCT_L1;

//*
//����
// ��msg[0] - STRUCT_ML2, *PSTRUCT_ML2
typedef struct stSTRUCT_ML2
{
    BASICTYPES; \
    STRUCT_L1 sub_1; \
    PSTRUCT_L1 p_sub_1; \
    STRUCT_L1 a_sub_1[ARRSIZ]; \
    PSTRUCT_L1 pa_sub_1[ARRSIZ]; \
    struct stSTRUCT_ML2 *p_prev; \
    struct stSTRUCT_ML2 *p_next; \
    unsigned char msg[0]; \
}STRUCT_ML2, *PSTRUCT_ML2;
//����msg[0]-STRUCT_L2, *PSTRUCT_L2
typedef struct stSTRUCT_L2
{
    BASICTYPES; \
    STRUCT_L1 sub_1; \
    PSTRUCT_L1 p_sub_1; \
    STRUCT_L1 a_sub_1[ARRSIZ]; \
    PSTRUCT_L1 pa_sub_1[ARRSIZ]; \
    struct stSTRUCT_L2 *p_prev; \
    struct stSTRUCT_L2 *p_next; \
}STRUCT_L2, *PSTRUCT_L2;
//*/


#define CON(a, b) a##b
#define DEFINE_STRUCT_LN(parent, child)\
typedef struct CON(stSTRUCT_L, parent)\
{\
BASICTYPES;\
CON(STRUCT_L, child) CON(sub_, child);\
CON(PSTRUCT_L, child) CON(p_sub_,child);\
CON(STRUCT_L, child) CON(a_sub_,child)[ARRSIZ];\
CON(PSTRUCT_L, child) CON(pa_sub_,child)[ARRSIZ];\
struct CON(stSTRUCT_L, parent) *p_prev;\
struct CON(stSTRUCT_L, parent) *p_next;\
}CON(STRUCT_L, parent), *CON(PSTRUCT_L, parent);


#define STRUHEAD(parent) typedef struct CON(stSTRUCT_MLN, parent)   {
#define DEFINE_STRUCTMLN_END(parent) }CON(STRUCT_ML, parent), *CON(PSTRUCT_ML, parent);

#define DEFINE_STRUCTMLN_BEGIN(parent, child)\
STRUHEAD(parent);\
BASICTYPES;\
CON(STRUCT_L, child) CON(sub_,child);\
CON(PSTRUCT_L, child) CON(p_sub_,child);\
CON(STRUCT_L, child) CON(a_sub_,child)[ARRSIZ];\
CON(PSTRUCT_L, child) CON(pa_sub_,child)[ARRSIZ];\
struct CON(stSTRUCT_MLN, parent) *p_prev;\
struct CON(stSTRUCT_MLN, parent) *p_next;\
/*
// ����
// ����msg[0] - STRUCT_L2, *PSTRUCT_L2
DEFINE_STRUCT_LN(2, 1)
// ��msg[0] - STRUCT_LM2, *PSTRUCT_LM2
DEFINE_STRUCTMLN_BEGIN(2, 1)
unsigned char msg[0];
DEFINE_STRUCTMLN_END(2)
*/
// ����
// ����msg[0] - STRUCT_L3, *PSTRUCT_L3
DEFINE_STRUCT_LN(3, 2)
// ��msg[0] - STRUCT_LM3, *PSTRUCT_LM3
DEFINE_STRUCTMLN_BEGIN(3, 2)
unsigned char msg[0];
DEFINE_STRUCTMLN_END(3)
/*
// �Ĳ�
// ����msg[0] - STRUCT_L4, *PSTRUCT_L4
DEFINE_STRUCT_LN(4, 3)
// ��msg[0] - STRUCT_LM4, *PSTRUCT_LM4
DEFINE_STRUCTMLN_BEGIN(4, 3)
unsigned char msg[0];
DEFINE_STRUCTMLN_END(4)

// ���
// ����msg[0] - STRUCT_L5, *PSTRUCT_L5
DEFINE_STRUCT_LN(5, 4)
// ��msg[0] - STRUCT_LM5, *PSTRUCT_LM5
DEFINE_STRUCTMLN_BEGIN(5, 4)
unsigned char msg[0];
DEFINE_STRUCTMLN_END(5)

// ����
// ����msg[0] - STRUCT_L6, *PSTRUCT_L6
DEFINE_STRUCT_LN(6, 5)
// ��msg[0] - STRUCT_LM6, *PSTRUCT_LM6
DEFINE_STRUCTMLN_BEGIN(6, 5)
unsigned char msg[0];
DEFINE_STRUCTMLN_END(6)
//*/

#define MAX_PARAM_NUMBER 10
#define MAX_PTR_PARA_LENGTH 2048
typedef union UPARAMS
{
    INT8 i8;
    UINT8 u8;
    INT16 i16;
    UINT16 u16;
    INT32 i32;
    UINT32 u32;
    LONG lon;
    ULONG ulo;
    SIZE_T sizet;
    SSIZE_T ssizet;
    STRUCT_L1 sl1;
    STRUCT_L2 sl2;
    ULONG *p_ul;
    UINT8 *p_u8;
    void *ptr;
    STRUCT_ML1 sml1;
    STRUCT_ML2 sml2;
    UINT8 a_u8[MAX_PTR_PARA_LENGTH];
    TLV tlv;
}UNIONPARAM;

#ifdef __cplusplus
}
#endif
#endif //__TEST_CASE_STRUCT_DEFINE_

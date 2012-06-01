#ifndef rr_c_typesH
#define rr_c_typesH

#if defined(__cplusplus)
extern "C"
{
#else
#include <stdio.h>
#include "rr_support/stdbool.h"
#endif
                                    /*Hand the client a HANDLE, i.e. ptr to structure */
typedef void*                       RRHandle;

C_DECL_SPEC typedef struct RRDoubleVector
{
    int             Size;
    double*         Data;
} *RRDoubleVectorHandle ;


C_DECL_SPEC typedef struct RRSymbolList
{

} *RRSymbolListHandle ;

C_DECL_SPEC typedef struct RRDataMatrix
{
    int             RSize;
    int             CSize;
    double*         Data;
} *RRDataMatrixHandle ;

C_DECL_SPEC typedef struct RRResult
{
    int             RSize;
    int             CSize;
    double*         Data;
    char**          ColumnHeaders;
                   ~RRResult();
} *RRResultHandle ;

C_DECL_SPEC typedef struct RRStringList
{
    int             Count;
    char**          String;
} *RRStringListHandle ;


#if defined( __cplusplus)
}
#endif


#endif



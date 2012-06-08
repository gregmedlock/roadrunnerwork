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

typedef struct RRDoubleVector
{
    int             Size;
    double*         Data;
} *RRDoubleVectorHandle ;


typedef struct RRStringList
{
    int             Count;
    char*           Label;
    char**          String;
} *RRStringListHandle ;

typedef struct RRSymbolLists
{
    int             NumberOfLists;
    RRStringList*   List;
}*RRSymbolListsHandle ;

typedef struct RRDataMatrix
{
    int             RSize;
    int             CSize;
    double*         Data;
} *RRDataMatrixHandle ;

typedef struct RRResult
{
    int             RSize;
    int             CSize;
    double*         Data;
    char**          ColumnHeaders;
} *RRResultHandle ;

#if defined( __cplusplus)
}
#endif


#endif

#ifndef rrC_APIH
#define rrC_APIH

//Export/Import
#if defined(EXPORT_RR_C_API)
#define RR_DECL_SPEC __declspec(dllexport)
#else
#define RR_DECL_SPEC __declspec(dllimport)
#endif


#if defined(__cplusplus)
extern "C"
{
#else
#include <stdio.h>
#include "../../src/c_src/stdbool.h"
#endif

typedef void*                   RRHandle;
typedef struct RRDataMatrix*    RRDataMatrixHandle;
typedef struct RRResult*        RRResultHandle;

RR_DECL_SPEC struct RRDataMatrix
{
    int             mNrOfRows;
    int             mNrOfCols;
    double*         mData;
};

RR_DECL_SPEC struct RRResult
{
    int             mNrOfRows;
    int             mNrOfCols;
    double*         mData;
    char**          mColumnHeaders;
};

RR_DECL_SPEC RRHandle              getRRInstance(void);
RR_DECL_SPEC void                  deleteRRInstance(RRHandle *handle);
RR_DECL_SPEC char*                 getCopyright(void);

RR_DECL_SPEC bool                  loadSBML(const char* filePath);
RR_DECL_SPEC bool                  setTimeStart(double timeStart);
RR_DECL_SPEC bool                  setTimeEnd(double timeEnd);
RR_DECL_SPEC bool                  setNumPoints(int nrPoints);
RR_DECL_SPEC RRResultHandle        simulate(void);
RR_DECL_SPEC bool                  FreeRRResult(RRResultHandle rrResult);

RR_DECL_SPEC bool                  setSelectionList(const char* list);
RR_DECL_SPEC char*                 getReactionNames(void);

RR_DECL_SPEC double                getValue(void);
RR_DECL_SPEC bool                  setValue(double val);
RR_DECL_SPEC RRDataMatrixHandle    getStoichiometryMatrix(void);
RR_DECL_SPEC bool                  FreeRRDataMatrixHandle(RRDataMatrixHandle matrix);
RR_DECL_SPEC int                   GetNumber();
#if defined( __cplusplus)
}
#endif

#endif

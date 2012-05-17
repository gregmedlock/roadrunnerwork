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

RR_DECL_SPEC RRHandle           __stdcall     getRRInstance(void);
RR_DECL_SPEC void               __stdcall      deleteRRInstance(RRHandle *handle);
RR_DECL_SPEC char*              __stdcall      getCopyright(void);

RR_DECL_SPEC bool               __stdcall      loadSBML(const char* filePath);
RR_DECL_SPEC bool               __stdcall      setTimeStart(double timeStart);
RR_DECL_SPEC bool               __stdcall      setTimeEnd(double timeEnd);
RR_DECL_SPEC bool               __stdcall      setNumPoints(int nrPoints);
RR_DECL_SPEC RRResultHandle     __stdcall      simulate(void);
RR_DECL_SPEC bool               __stdcall      FreeRRResult(RRResultHandle rrResult);

RR_DECL_SPEC bool               __stdcall      setSelectionList(const char* list);
RR_DECL_SPEC char*              __stdcall      getReactionNames(void);

RR_DECL_SPEC double             __stdcall      getValue(void);
RR_DECL_SPEC bool               __stdcall      setValue(double val);
RR_DECL_SPEC RRDataMatrixHandle __stdcall   getStoichiometryMatrix(void);
RR_DECL_SPEC bool               __stdcall   FreeRRDataMatrixHandle(RRDataMatrixHandle matrix);
RR_DECL_SPEC int                __stdcall   GetNumber();
#if defined( __cplusplus)
}
#endif

#endif

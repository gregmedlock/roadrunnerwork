#ifndef rrC_APIH
#define rrC_APIH

//Export/Import
#if defined(EXPORT_RR_C_API)
#define C_DECL_SPEC __declspec(dllexport)
#else
#define C_DECL_SPEC __declspec(dllimport)
#endif

#if defined(__cplusplus)
extern "C"
{
#else
#include <stdio.h>
#include "rr_support/stdbool.h"
#endif

typedef void*                   RRHandle;
typedef struct RRDataMatrix*    RRDataMatrixHandle;
typedef struct RRResult*        RRResultHandle;
typedef struct RRStringList*    RRStringListHandle;

C_DECL_SPEC struct RRDataMatrix
{
    int             RSize;
    int             CSize;
    double*         Data;
};

C_DECL_SPEC struct RRResult
{
    int             RSize;
    int             CSize;
    double*         Data;
    char**          ColumnHeaders;
                   ~RRResult();     //Is this OK???
};

C_DECL_SPEC struct RRStringList
{
    int             Count;
    char**          String;
};

/////////////////////  API FUNCTIONS ///////////////////////////////////////////////
C_DECL_SPEC RRHandle                __stdcall   getRRInstance(void);
C_DECL_SPEC char*                   __stdcall   getBuildDate(void);
C_DECL_SPEC void                    __stdcall   deleteRRInstance(RRHandle handle);
C_DECL_SPEC char*                   __stdcall   getCopyright(void);
C_DECL_SPEC bool                    __stdcall   setTempFolder(const char* folder);
C_DECL_SPEC bool                    __stdcall   loadSBML(const char* sbml);
C_DECL_SPEC bool                    __stdcall   setTimeStart(double timeStart);
C_DECL_SPEC bool                    __stdcall   setTimeEnd(double timeEnd);
C_DECL_SPEC bool                    __stdcall   setNumPoints(int nrPoints);
C_DECL_SPEC RRResultHandle          __stdcall   simulate(void);
C_DECL_SPEC bool                    __stdcall   freeRRResult();
C_DECL_SPEC bool                    __stdcall   setSelectionList(const char* list);
C_DECL_SPEC RRStringListHandle      __stdcall   getReactionNames(void);
C_DECL_SPEC double                  __stdcall   getValue(const char* specie);
C_DECL_SPEC bool                    __stdcall   setValue(const char* speciesId, const double& val);
C_DECL_SPEC RRDataMatrixHandle      __stdcall   getStoichiometryMatrix(void);
C_DECL_SPEC bool                    __stdcall   freeRRDataMatrixHandle(RRDataMatrixHandle matrix);
C_DECL_SPEC void                    __stdcall   printMatrix(RRDataMatrixHandle mat);
C_DECL_SPEC char*                   __stdcall   getLastError();
C_DECL_SPEC char*                   __stdcall   freeText(char* text);


// Free functions
C_DECL_SPEC bool                    __stdcall   freeStringList(RRStringListHandle sl);


#if defined( __cplusplus)
}
#endif

#endif

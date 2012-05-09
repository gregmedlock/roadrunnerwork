#ifndef rrC_APIH
#define rrC_APIH

//Export/Import
#if defined(EXPORT_RR)
#define RR_DECL_SPEC __declspec(dllexport)
#else
#define RR_DECL_SPEC __declspec(dllimport)
#endif

#include <stdio.h>
#if defined(__cplusplus)
extern "C"
{
#endif
typedef void* RRHandle;


RR_DECL_SPEC RRHandle 	getRRInstance();
RR_DECL_SPEC void  		deleteRRInstance(RRHandle *handle);
RR_DECL_SPEC char* 		getCopyright();

RR_DECL_SPEC bool loadSBML(const char* filePath);
RR_DECL_SPEC bool setTimeStart(const double& timeStart);
RR_DECL_SPEC bool setTimeEnd(const double& timeEnd);
RR_DECL_SPEC bool setNumPoints(const int& nrPoints);
RR_DECL_SPEC RRResult* simulate();
RR_DELC_SPEC bool FreeRRResult();

RR_DECL_SPEC bool setSelectionList(const char* list);
RR_DECL_SPEC char* getReactionNames();

RR_DECL_SPEC double getValue();
RR_DECL_SPEC bool setValue(const double& val);
RR_DECL_SPEC getStoichiometryMatrix

#if defined( __cplusplus)
}
#endif

#endif

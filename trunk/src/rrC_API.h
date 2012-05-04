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

#if defined( __cplusplus)
}
#endif

#endif

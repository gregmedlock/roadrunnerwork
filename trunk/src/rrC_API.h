#ifndef rrC_APIH
#define rrC_APIH
//---------------------------------------------------------------------------

//Export/Import
#if defined(EXPORT_RR)
#define DECL_SPEC __declspec(dllexport)
#else
#define DECL_SPEC __declspec(dllimport)
#endif

#include <stdio.h>
#if defined(__cplusplus)
extern "C"
{
#endif
typedef void *RRHandle;


DECL_SPEC RRHandle getRRInstance();
DECL_SPEC void 	deleteRRInstance(RRHandle *handle);
DECL_SPEC char* getCopyright();

#if defined( __cplusplus)
}
#endif

#endif

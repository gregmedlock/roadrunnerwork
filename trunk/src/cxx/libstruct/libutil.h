#ifndef LIB_LA_LIB_UTIL_H
#define LIB_LA_LIB_UTIL_H

#ifdef WIN32
#if defined(LIB_EXPORTS)
#  define LIB_EXTERN __declspec(dllexport)
#elif defined(NO_LIBSTRUCT_DLL)
#  define LIB_EXTERN
#else
#  define LIB_EXTERN __declspec(dllimport)
#endif
#else
#  define LIB_EXTERN
#endif

#if defined(__cplusplus)
#  define BEGIN_C_DECLS extern "C" { //
#  define END_C_DECLS   } //
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif

#endif



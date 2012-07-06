#ifndef LIB_LA_LIB_UTIL_H
#define LIB_LA_LIB_UTIL_H

#if defined( WIN32 )

#if defined(STATIC_LIB_LA)
#  define LIB_EXTERN
#else

#if defined(LIB_EXPORTS)
    #define LIB_EXTERN __declspec(dllexport)
#else
    #define LIB_EXTERN __declspec(dllimport)
#endif

#endif

#else //WIN32
    #define LIB_EXTERN
#endif

#if defined(__cplusplus)
#  define BEGIN_C_DECLS extern "C" { //
#  define END_C_DECLS   } //
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif

#endif



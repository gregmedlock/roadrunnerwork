#ifndef lsLibUtilH
#define lsLibUtilH

#if defined( WIN32 )

#if defined(LIBSTRUCT_STATIC)          //Libstruct static
#  define LIB_EXTERN
#else

#if defined(LIBSTRUCT_SHARED)       //Export libstruct
    #define LIB_EXTERN __declspec(dllexport)
#else
    #define LIB_EXTERN __declspec(dllimport)
#endif

#endif

#else //WIN32
    #define LIB_EXTERN
#endif

#endif



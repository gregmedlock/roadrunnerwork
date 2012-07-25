#ifndef rrExporterH
#define rrExporterH

#if defined(STATIC_RR)
    #define RR_DECLSPEC
    #define STATIC_LIB_LA
#else

#if defined(EXPORT_RR)
    #define RR_DECLSPEC __declspec(dllexport)
#else
    #define RR_DECLSPEC __declspec(dllimport)
#endif

#endif


#if defined (__MINGW32__)
#undef RR_DECLSPEC
#endif

#if defined(_MSC_VER)
#pragma warning(disable : 4996) // _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4018) // int to unsigned int comparison
#pragma warning(disable : 4482) // prefixing enums...
#pragma warning(disable : 4251) // _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4221) // empty cpp file

#define __FUNC__ "not defined in VS"
#endif

#endif

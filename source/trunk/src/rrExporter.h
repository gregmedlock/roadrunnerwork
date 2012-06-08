#ifndef rrExporterH
#define rrExporterH

#if defined(EXPORT_RR) //DLL
    #define RR_DECLSPEC __declspec(dllexport)

#elif defined(NO_RR_DLL)
    #define RR_DECLSPEC

#else
    #define RR_DECLSPEC __declspec(dllimport)
#endif


#if defined (__MINGW32__)
#undef RR_DECLSPEC2
#endif

#if defined(_MSC_VER)
#pragma warning(disable : 4996) // _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4018) // int to unsigned int comparison
#pragma warning(disable : 4482) // prefixing enums...
#pragma warning(disable : 4251) // _CRT_SECURE_NO_WARNINGS

#define __FUNC__ "not defined in VS"
#endif

#endif

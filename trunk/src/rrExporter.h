#ifndef rrExporterH
#define rrExporterH

#if defined(EXPORT_RR) //DLL
	#define RR_DECLSPEC __declspec(dllexport)

#elif defined(IMPORT_RR)
	#define RR_DECLSPEC __declspec(dllimport)

#else
	#define RR_DECLSPEC

#endif


#if defined(_MSC_VER)

#pragma warning(disable : 4996) // _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4018) // int to unsigned int comparison
#pragma warning(disable : 4482) // prefixing enums...

#define __FUNC__ "not defined in VS"
#endif

#endif

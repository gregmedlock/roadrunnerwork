#ifndef rrExporterH
#define rrExporterH

#if defined(EXPORT_RR) //DLL
	#define RR_DECLSPEC __declspec(dllexport)

#elif defined(EXPORT_RR_PKG) //Package
	#define RR_DECLSPEC __declspec(package)

#elif defined(IMPORT_RR_PKG)
	#define RR_DECLSPEC __declspec(package)

#elif defined(IMPORT_RR)
	#define RR_DECLSPEC __declspec(dllimport)

#else
	#define RR_DECLSPEC

#endif
#endif

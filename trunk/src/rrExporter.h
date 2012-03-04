#ifndef rrExporterH
#define rrExporterH

#if defined(EXPORT_RR) //DLL
	#define RR_DECLSPEC __declspec(dllexport)

#elif defined(IMPORT_RR)
	#define RR_DECLSPEC __declspec(dllimport)

#else
	#define RR_DECLSPEC

#endif
#endif

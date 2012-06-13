#ifndef rr_c_api_exporterH
#define rr_c_api_exporterH
//---------------------------------------------------------------------------

//Export/Import API functions
#if defined (STATIC_RR_C_API)
#define C_DECL_SPEC
#else

#if defined(EXPORT_RR_C_API)
#define C_DECL_SPEC __declspec(dllexport)
#else
#define C_DECL_SPEC __declspec(dllimport)
#endif

#endif	//STATIC_RR_C_API

#endif //hdr_guard

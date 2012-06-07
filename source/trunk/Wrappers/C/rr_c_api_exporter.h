#ifndef rr_c_api_exporterH
#define rr_c_api_exporterH
//---------------------------------------------------------------------------

//Export/Import API functions
#if defined(EXPORT_RR_C_API)
#define C_DECL_SPEC __declspec(dllexport)
#else
#define C_DECL_SPEC __declspec(dllimport)
#endif


#endif

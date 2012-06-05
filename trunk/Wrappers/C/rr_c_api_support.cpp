#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rr_c_api.h"
#include "rr_c_api_support.h"
//---------------------------------------------------------------------------
#if defined (__CODEGEAR__)
#pragma package(smart_init)
#endif
extern char* gLastError;

//static const char* ALLOCATE_API_ERROR_MSG = {"Please allocate a handle to the roadrunner API before calling any API function"};
void SetAPIError(const string& err)
{
    if(gLastError)
    {
        delete gLastError;
    }
    gLastError = new char[err.size() + 1];
    strcpy(gLastError, err.c_str());
}




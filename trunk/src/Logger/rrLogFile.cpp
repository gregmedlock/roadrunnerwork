#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
//#include <fstream>
#include <stdio.h>
#include "rrLogFile.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif

namespace rr
{
using std::fstream;
LogFile::LogFile(const char* name)
:
mFileName(name),
mFILEHandle(fopen(name, "w"))
{
    if (!mFILEHandle)
    {
        throw std::runtime_error("File Open failure");
    }
}

LogFile::~LogFile()
{
    fclose(mFILEHandle);
}

}

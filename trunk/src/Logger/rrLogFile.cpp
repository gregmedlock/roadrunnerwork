#ifdef MTK_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
//#include <fstream>
#include <stdio.h>
#include "rrLogFile.h"
#ifdef __CODEGEARC__
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

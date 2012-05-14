#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
//#include <fstream>
#include <stdio.h>
#include "rrLogFile.h"
//---------------------------------------------------------------------------


namespace rr
{
using std::fstream;
LogFile::LogFile(const string& name)
:
mFileName(name),
mFILEHandle(fopen(name.c_str(), "w"))
{
    if (!mFILEHandle)
    {
        throw std::runtime_error("File Open failure");
    }
}

//LogFile::LogFile(const char* name)
//:
//mFileName(name),
//mFILEHandle(fopen(name, "w"))
//{
//    if (!mFILEHandle)
//    {
//        throw std::runtime_error("File Open failure");
//    }
//}

LogFile::~LogFile()
{
    fclose(mFILEHandle);
}

}

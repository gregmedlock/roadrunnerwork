#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
//#include <fstream>
#include <stdio.h>
#include "mtkLogFile.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

using std::fstream;
mtkLogFile::mtkLogFile(const char* name)
:
mFILEHandle(fopen(name, "w"))
{
    if (!mFILEHandle)
    {
        throw std::runtime_error("File Open failure");
    }
}

mtkLogFile::~mtkLogFile()
{
    fclose(mFILEHandle);
}


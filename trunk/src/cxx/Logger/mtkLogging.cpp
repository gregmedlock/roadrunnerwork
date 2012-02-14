#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
#include "mtkLogging.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

mtkLogging gLogging;

int mtkLogging::mNrOfInstances = 0;

mtkLogging::mtkLogging()
:
//mLogFile(),
mLogPrefix("none"),
mLogLevel(lDebug5),
mLogToServer(false)
{
	mNrOfInstances++;
}

bool mtkLogging::Init(const string& logPrefix, const mtkLogLevel& level, unique_ptr<mtkLogFile> logFile)
{
    mLogPrefix = logPrefix;
    mLogLevel = level;
    mLogFile = move(logFile);
 	return mLogFile.get() ? true : false;
}

mtkLogging::~mtkLogging()
{
	mNrOfInstances--;
}

mtkLogLevel mtkLogging::GetLogLevel()
{
	return mLogLevel;
}

void mtkLogging::SetLogLevel(const mtkLogLevel& lvl)
{
	mLogLevel = lvl;
}

void mtkLogging::SetLogPrefix(const string& prefix)
{
	mLogPrefix = prefix;
}

string mtkLogging::GetLogPrefix()
{
	return mLogPrefix;
}

void mtkLogging::write(const char* str)
{
	if(!mLogFile.get())
    {
		return;
    }
	fprintf(mLogFile->mFILEHandle, "%s", str);

    if (EOF == fflush(mLogFile->mFILEHandle))
    {
        throw std::runtime_error("file write failure");
    }
}


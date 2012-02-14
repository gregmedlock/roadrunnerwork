#ifdef MTK_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrLogging.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

rrLogging gLogging;

int rrLogging::mNrOfInstances = 0;

rrLogging::rrLogging()
:
//mLogFile(),
mLogPrefix("none"),
mLogLevel(lDebug5),
mLogToServer(false)
{
	mNrOfInstances++;
}

bool rrLogging::Init(const string& logPrefix, const rrLogLevel& level, unique_ptr<rrLogFile> logFile)
{
    mLogPrefix = logPrefix;
    mLogLevel = level;
    mLogFile = move(logFile);
 	return mLogFile.get() ? true : false;
}

string rrLogging::GetLogFileName()
{
	if(mLogFile)
    {
    	return mLogFile->GetFileName();
    }
    return string("<none>");
}

rrLogging::~rrLogging()
{
	mNrOfInstances--;
}

rrLogLevel rrLogging::GetLogLevel()
{
	return mLogLevel;
}

void rrLogging::SetCutOffLogLevel(const rrLogLevel& lvl)
{
	mLogLevel = lvl;
}

void rrLogging::SetLogPrefix(const string& prefix)
{
	mLogPrefix = prefix;
}

string rrLogging::GetLogPrefix()
{
	return mLogPrefix;
}

void rrLogging::write(const char* str)
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


#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrFileLog.h"
//---------------------------------------------------------------------------


namespace rr
{

FileLog gLog;

int FileLog::mNrOfInstances = 0;

FileLog::FileLog()
:
mLogFile(),
mLogPrefix("none"),
mLogLevel(lDebug5),
mLogToServer(false)
{
	mNrOfInstances++;
}

FileLog::~FileLog()
{
	mNrOfInstances--;
}


bool FileLog::Init(const string& logPrefix, const LogLevel& level, unique_ptr<LogFile> logFile)
{
    mLogPrefix = logPrefix;
    mLogLevel = level;
    mLogFile = move(logFile);
 	return mLogFile.get() ? true : false;
}

string FileLog::GetLogFileName()
{
	if(mLogFile)
    {
    	return mLogFile->GetFileName();
    }
    return string("<none>");
}

LogLevel FileLog::GetLogLevel()
{
	return mLogLevel;
}

void FileLog::SetCutOffLogLevel(const LogLevel& lvl)
{
	mLogLevel = lvl;
}

void FileLog::SetLogPrefix(const string& prefix)
{
	mLogPrefix = prefix;
}

string FileLog::GetLogPrefix()
{
	return mLogPrefix;
}

void FileLog::write(const char* str)
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

}

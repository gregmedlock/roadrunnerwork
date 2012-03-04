#ifdef MTK_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <string>
#include "rrFileLog.h"
#include "rrLogOutput.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
#ifdef __CODEGEARC__

#endif

namespace rr
{

bool          		LogOutput::mDoLogging		= true;
bool          		LogOutput::mLogToConsole 	= false;
bool          		LogOutput::mShowLogTime 	= false;
bool          		LogOutput::mShowLogPrefix	= false;
bool          		LogOutput::mShowLogLevel	= false;
bool          		LogOutput::mUseLogTabs		= false;

LogOutput::LogOutput(){}

void LogOutput::StopLogging()
{
	mDoLogging = false;
}

void LogOutput::StartLogging()
{
	mDoLogging = true;
}

void LogOutput::Output(const string& msg, const LogLevel& /*lvl*/)
{
    if(!mDoLogging)
    {
    	return;
    }

    //Log to file, always..
	gLog.write(msg.c_str());

    if(mLogToConsole == true) 	//Default is standard error
    {
		fprintf(stderr, "%s", msg.c_str());
	    fflush(stderr);
    }
}

}//namespace

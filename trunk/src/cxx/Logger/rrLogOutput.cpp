#ifdef MTK_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <string>
#include "rrLogging.h"
#include "rrLogOutput.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

namespace rr
{

bool          		rrLogOutput::mLogToConsole 	= false;
bool          		rrLogOutput::mShowLogTime 	= false;
bool          		rrLogOutput::mShowLogPrefix	= false;
bool          		rrLogOutput::mShowLogLevel	= false;
bool          		rrLogOutput::mUseLogTabs		= false;
bool          		rrLogOutput::mDoLogging		= true;


rrLogOutput::rrLogOutput(){}

void rrLogOutput::StopLogging()
{
	mDoLogging = false;
}

void rrLogOutput::StartLogging()
{
	mDoLogging = true;
}

void rrLogOutput::Output(const string& msg, const rrLogLevel& /*lvl*/)
{
    if(!mDoLogging)
    {
    	return;
    }

    //Log to file, always..
	gLogging.write(msg.c_str());

    if(mLogToConsole == true) 	//Default is standard error
    {
		fprintf(stderr, "%s", msg.c_str());
	    fflush(stderr);
    }
}

}//namespace rr

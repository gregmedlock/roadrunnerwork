#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
#include <string>
#include "mtkLogging.h"
#include "mtkLogOutput.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

namespace rr
{

bool          		mtkLogOutput::mLogToConsole 	= false;
bool          		mtkLogOutput::mShowLogTime 	= false;
bool          		mtkLogOutput::mShowLogPrefix	= false;
bool          		mtkLogOutput::mShowLogLevel	= false;
bool          		mtkLogOutput::mUseLogTabs		= false;
bool          		mtkLogOutput::mDoLogging		= true;


mtkLogOutput::mtkLogOutput(){}

void mtkLogOutput::StopLogging()
{
	mDoLogging = false;
}

void mtkLogOutput::StartLogging()
{
	mDoLogging = true;
}

void mtkLogOutput::Output(const string& msg, const mtkLogLevel& /*lvl*/)
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

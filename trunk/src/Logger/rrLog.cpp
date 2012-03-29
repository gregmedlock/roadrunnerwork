#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <string>
#include "rrLog.h"
//---------------------------------------------------------------------------


namespace rr
{

template class LogContainer<LogOutput>;

template <>
LogContainer<LogOutput>::LogContainer()
{}

template <>
LogContainer<LogOutput>::~LogContainer()
{
    mOutputStream << std::endl;
    LogOutput::Output(mOutputStream.str(), mCurrentLogLevel);
}

template <>
std::ostringstream& LogContainer<LogOutput>::Get(const LogLevel& level)
{
    mCurrentLogLevel = level;

    if(LogOutput::mShowLogPrefix)
    {
		mOutputStream << gLog.GetLogPrefix() <<" ";
    }

    if(LogOutput::mShowLogTime)
    {
        mOutputStream << GetLogTime(true);
    }

    if(LogOutput::mUseLogTabs)
    {
		//Output tabs
		mOutputStream << string(level > lInfo ? level - lInfo : 0, '\t');
		mOutputStream << "\t";
    }

    if(LogOutput::mShowLogLevel)
    {
         mOutputStream << LogLevelToString(level) << ": "; //Next comes the log message
    }
	return mOutputStream;
}

}//namespace

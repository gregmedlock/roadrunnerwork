#ifdef MTK_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <string>
#include "rrLog.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

namespace rr
{

template class rrLog<rrLogOutput>;

template <>
rrLog<rrLogOutput>::rrLog()
{}

template <>
rrLog<rrLogOutput>::~rrLog()
{
    mOutputStream << std::endl;
    rrLogOutput::Output(mOutputStream.str(), mCurrentLogLevel);
}

template <>
std::ostringstream& rrLog<rrLogOutput>::Get(const rrLogLevel& level)
{
    mCurrentLogLevel = level;

    if(rrLogOutput::mShowLogPrefix)
    {
		mOutputStream << gLogging.GetLogPrefix() <<" ";
    }

    if(rrLogOutput::mShowLogTime)
    {
        mOutputStream << GetLogTime(true);
    }

    if(rrLogOutput::mUseLogTabs)
    {
		//Output tabs
		mOutputStream << string(level > lInfo ? level - lInfo : 0, '\t');
		mOutputStream << "\t";
    }

    if(rrLogOutput::mShowLogLevel)
    {
         mOutputStream << LogLevelToString(level) << ": "; //Next comes the log message
    }
	return mOutputStream;
}


}//namespace rr

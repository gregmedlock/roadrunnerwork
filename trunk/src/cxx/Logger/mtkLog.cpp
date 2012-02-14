#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
#include <string>
#include "mtkLog.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

namespace mtk
{

template class mtkLog<mtkLogOutput>;

template <>
mtkLog<mtkLogOutput>::mtkLog()
{}

template <>
mtkLog<mtk::mtkLogOutput>::~mtkLog()
{
    mOutputStream << std::endl;
    mtkLogOutput::Output(mOutputStream.str(), mCurrentLogLevel);
}

template <>
std::ostringstream& mtkLog<mtkLogOutput>::Get(const mtkLogLevel& level)
{
    mCurrentLogLevel = level;

    if(mtkLogOutput::mShowLogPrefix)
    {
		mOutputStream << gLogging.GetLogPrefix() <<" ";
    }

    if(mtkLogOutput::mShowLogTime)
    {
        mOutputStream << GetLogTime(true);
    }

    if(mtkLogOutput::mUseLogTabs)
    {
		//Output tabs
		mOutputStream << string(level > lInfo ? level - lInfo : 0, '\t');
		mOutputStream << "\t";
    }

    if(mtkLogOutput::mShowLogLevel)
    {
         mOutputStream << LogLevelToString(level) << ": "; //Next comes the log message
    }
	return mOutputStream;
}


}//namespace mtk

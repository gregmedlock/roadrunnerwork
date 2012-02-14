#ifndef mtkLogOutputH
#define mtkLogOutputH
#include <sstream>
#include <string>
#include <stdio.h>
#include "rrObject.h"
#include "mtkLogLevel.h"
using std::string;
using std::ostringstream;

namespace rr
{

class RR_DECLSPEC mtkLogOutput : public rrObject
{
	public:
        static bool             mShowLogTime;
        static bool             mShowLogPrefix;
		static bool             mShowLogLevel;
		static bool             mUseLogTabs;
        static bool             mLogToMemo;
        static bool             mLogToConsole;
        static bool             mDoLogging;
        						mtkLogOutput();
	    static void             Output(const string& msg, const mtkLogLevel& lvl);
        static void				StopLogging();
        static void				StartLogging();
};

}
#endif




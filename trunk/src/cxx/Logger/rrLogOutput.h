#ifndef rrLogOutputH
#define rrLogOutputH
#include <sstream>
#include <string>
#include <stdio.h>
#include "rrObject.h"
#include "rrLogLevel.h"
using std::string;
using std::ostringstream;

namespace rr
{

class RR_DECLSPEC rrLogOutput : public rrObject
{
	public:
        static bool             mShowLogTime;
        static bool             mShowLogPrefix;
		static bool             mShowLogLevel;
		static bool             mUseLogTabs;
        static bool             mLogToMemo;
        static bool             mLogToConsole;
        static bool             mDoLogging;
        						rrLogOutput();
	    static void             Output(const string& msg, const rrLogLevel& lvl);
        static void				StopLogging();
        static void				StartLogging();
};

}
#endif




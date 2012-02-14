#ifndef mtkLogOutputH
#define mtkLogOutputH
#include <sstream>
#include <string>
#include <stdio.h>
#include "CommonExporter.h"
#include "mtkLogLevel.h"
using std::string;
using std::ostringstream;

namespace mtk
{

class MTK_COMMON mtkLogOutput : public mtkObject
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




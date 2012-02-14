#ifndef mtkLogLevelH
#define mtkLogLevelH
//---------------------------------------------------------------------------
#include <string>
//#include "CommonExporter.h"

using std::string;
namespace mtk
{
MTK_COMMON enum  mtkLogLevel
    {
    	lUndef      = -1,
        lError      = 0,
        lWarning    = 1,
        lInfo       = 2,
        lDebug      = 3,
        lDebug1     = 4,
        lDebug2     = 5,
        lDebug3     = 6,
        lDebug4     = 7,
        lDebug5     = 8,
        lAny        = 9,
        lUser
    };


string 			MTK_COMMON ToUpperCase(const string& inStr);
int 		    MTK_COMMON GetHighestLogLevel();
mtkLogLevel     MTK_COMMON StringToLogLevel(const string& level);
string          MTK_COMMON LogLevelToString(const mtkLogLevel& level);
mtkLogLevel     MTK_COMMON IntToLogLevel(const int& lvl);

}
#endif

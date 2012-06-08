#ifndef rrLogLevelH
#define rrLogLevelH
//---------------------------------------------------------------------------
#include <string>
#include "rrExporter.h"

using std::string;
namespace rr
{

enum  LogLevel
    {
        lShowAlways = -1,
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

string         RR_DECLSPEC ToUpperCase(const string& inStr);
int            RR_DECLSPEC GetHighestLogLevel();
LogLevel       RR_DECLSPEC StringToLogLevel(const string& level);
string         RR_DECLSPEC LogLevelToString(const LogLevel& level);
LogLevel       RR_DECLSPEC IntToLogLevel(const int& lvl);

}
#endif

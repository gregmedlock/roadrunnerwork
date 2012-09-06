#ifndef rrLogH
#define rrLogH
#include <sstream>
#include <string>
#include <stdio.h>
#include "rrExporter.h"
#include "rrLoggerUtils.h"
#include "rrLogLevel.h"
#include "rrFileLog.h"
#include "rrLogOutput.h"

using std::string;
using std::ostringstream;

namespace rr
{

template <class T>
class RR_DECLSPEC LogContainer : public rrObject
{
    private:
        LogLevel                    mCurrentLogLevel;
                                    LogContainer(const LogContainer&);    //Don't copy this one..
    protected:
        std::ostringstream          mOutputStream;

    public:
                                    LogContainer();
        virtual                    ~LogContainer();
        std::ostringstream&         Get(const LogLevel& level);
        string						GetCurrentLogLevel();
};

class RR_DECLSPEC Logger : public LogContainer<LogOutput>
{
};

#ifndef NO_LOGGER
#define Log(level) \
    if (level > rr::GetHighestLogLevel()) { ; }\
    else if (level > gLog.GetLogLevel()) { ; } \
    else Logger().Get(level)
#else
#define Log(level) \
    if (true) {  }\
    else \
    Logger().Get(level)
#endif


}//Namespace rr

#endif

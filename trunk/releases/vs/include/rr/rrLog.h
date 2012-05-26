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
};

class RR_DECLSPEC Logger : public LogContainer<LogOutput>
{
};



#define Log(level) \
    if (level > GetHighestLogLevel()) ;\
    else if (level > gLog.GetLogLevel()) ; \
    else Logger().Get(level)

}//Namespace rr

#endif

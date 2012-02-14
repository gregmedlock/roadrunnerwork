#ifndef rrLogH
#define rrLogH
#include <sstream>
#include <string>
#include <stdio.h>
#include "rrExporter.h"
#include "rrLoggerUtils.h"
#include "rrLogLevel.h"
#include "rrLogging.h"
#include "rrLogOutput.h"

using std::string;
using std::ostringstream;

namespace rr
{

template <class T>
class RR_DECLSPEC rrLog : public rrObject
{
	private:
        rrLogLevel            	mCurrentLogLevel;
    							rrLog(const rrLog&);
    protected:
	    std::ostringstream 		mOutputStream;

    public:
        					   	rrLog();
        virtual    		   	   ~rrLog();
        std::ostringstream&    	Get(const rrLogLevel& level);
};

class RR_DECLSPEC Logger : public rrLog<rrLogOutput>
{
};

}//Namespace rr

#define Log(level) \
    if (level > GetHighestLogLevel()) ;\
	else if (level > gLogging.GetLogLevel()) ; \
    else Logger().Get(level)

#endif

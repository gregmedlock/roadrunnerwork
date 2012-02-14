#ifndef mtkLogH
#define mtkLogH
#include <sstream>
#include <string>
#include <stdio.h>
#include "rrExporter.h"
#include "mtkLoggerUtils.h"
#include "mtkLogLevel.h"
#include "mtkLogging.h"
#include "mtkLogOutput.h"

using std::string;
using std::ostringstream;

namespace rr
{

template <class T>
class RR_DECLSPEC mtkLog : public rrObject
{
	private:
        mtkLogLevel            	mCurrentLogLevel;
    							mtkLog(const mtkLog&);
    protected:
	    std::ostringstream 		mOutputStream;

    public:
        					   	mtkLog();
        virtual    		   	   ~mtkLog();
        std::ostringstream&    	Get(const mtkLogLevel& level);
};

class RR_DECLSPEC Logger : public mtkLog<mtkLogOutput>
{
};

}//Namespace mtk

#define Log(level) \
    if (level > GetHighestLogLevel()) ;\
	else if (level > gLogging.GetLogLevel()) ; \
    else Logger().Get(level)

#endif

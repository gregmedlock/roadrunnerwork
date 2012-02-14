#ifndef mtkLogH
#define mtkLogH
#include <sstream>
#include <string>
#include <stdio.h>
#include "CommonExporter.h"
#include "mtkLoggerUtils.h"
#include "mtkLogLevel.h"
#include "mtkLogging.h"
#include "mtkLogOutput.h"

using std::string;
using std::ostringstream;

namespace mtk
{

template <class T>
class MTK_COMMON mtkLog : public mtkObject
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

class MTK_COMMON Logger : public mtkLog<mtkLogOutput>
{
};

}//Namespace mtk

#define Log(level) \
    if (level > GetHighestLogLevel()) ;\
	else if (level > gLogging.GetLogLevel()) ; \
    else Logger().Get(level)

#endif

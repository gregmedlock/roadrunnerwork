//---------------------------------------------------------------------------
#ifndef mtkLoggingH
#define mtkLoggingH
//---------------------------------------------------------------------------
#include <memory>
#include "stdio.h"
#include "CommonExporter.h"
#include "mtkLogLevel.h"
//#include "mtkObject.h"
#include "mtkLogFile.h"
using std::unique_ptr;
//class mtkLogFile;

//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII
using namespace mtk;
class MTK_COMMON mtkLogging //: public mtkObject
{
    private:
        unique_ptr<mtkLogFile>  mLogFile;
        string		           	mLogPrefix;
     	mtkLogLevel	           	mLogLevel;
		static int				mNrOfInstances;

		        	           	// prevent copying and assignment
        			           	mtkLogging(const mtkLogging& logFile);
        			           	mtkLogging& operator=(const mtkLogging &);

    public:
        		   		       	mtkLogging();
       		   		   		   ~mtkLogging();
        string			        GetLogPrefix();
        void			        SetLogPrefix(const string& prefix);
        mtkLogLevel		        GetLogLevel();
        void			        SetLogLevel(const mtkLogLevel& lvl);
        bool			        Init(const string& logPrefix = "none", const mtkLogLevel& level = lDebug5, unique_ptr<mtkLogFile> logFile = unique_ptr<mtkLogFile>());
        void 	   				write(const char* str);
        bool					mLogToServer;
};



extern MTK_COMMON mtkLogging  gLogging;
#endif

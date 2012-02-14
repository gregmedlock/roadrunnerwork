//---------------------------------------------------------------------------
#ifndef mtkLoggingH
#define mtkLoggingH
//---------------------------------------------------------------------------
#include <memory>
#include "stdio.h"
#include "rrObject.h"
#include "mtkLogLevel.h"
#include "mtkLogFile.h"
using std::unique_ptr;
//class mtkLogFile;

//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII
using namespace rr;
class RR_DECLSPEC mtkLogging : public rrObject
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
        void			        SetCutOffLogLevel(const mtkLogLevel& lvl);
        bool			        Init(const string& logPrefix = "none", const mtkLogLevel& level = lDebug5, unique_ptr<mtkLogFile> logFile = unique_ptr<mtkLogFile>());
        void 	   				write(const char* str);
        bool					mLogToServer;
        string					GetLogFileName();
};



extern RR_DECLSPEC mtkLogging  gLogging;
#endif

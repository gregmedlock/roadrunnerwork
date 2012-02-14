//---------------------------------------------------------------------------
#ifndef rrLoggingH
#define rrLoggingH
//---------------------------------------------------------------------------
#include <memory>
#include "stdio.h"
#include "rrObject.h"
#include "rrLogLevel.h"
#include "rrLogFile.h"
using std::unique_ptr;
//class rrLogFile;

//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII
using namespace rr;
class RR_DECLSPEC rrLogging : public rrObject
{
    private:
        unique_ptr<rrLogFile>  mLogFile;
        string		           	mLogPrefix;
     	rrLogLevel	           	mLogLevel;
		static int				mNrOfInstances;

		        	           	// prevent copying and assignment
        			           	rrLogging(const rrLogging& logFile);
        			           	rrLogging& operator=(const rrLogging &);

    public:
        		   		       	rrLogging();
       		   		   		   ~rrLogging();
        string			        GetLogPrefix();
        void			        SetLogPrefix(const string& prefix);
        rrLogLevel		        GetLogLevel();
        void			        SetCutOffLogLevel(const rrLogLevel& lvl);
        bool			        Init(const string& logPrefix = "none", const rrLogLevel& level = lDebug5, unique_ptr<rrLogFile> logFile = unique_ptr<rrLogFile>());
        void 	   				write(const char* str);
        bool					mLogToServer;
        string					GetLogFileName();
};



extern RR_DECLSPEC rrLogging  gLogging;
#endif

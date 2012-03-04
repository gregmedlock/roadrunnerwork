//---------------------------------------------------------------------------
#ifndef rrFileLogH
#define rrFileLogH
//---------------------------------------------------------------------------
#include <memory>
#include "stdio.h"
#include "rrObject.h"
#include "rrLogLevel.h"
#include "rrLogFile.h"
using std::unique_ptr;
//Global class holding logfile and other settings. Should Persist trougout the life of the application that is using it. Based on RAII
namespace rr
{

class RR_DECLSPEC FileLog : public rrObject
{
    private:
        unique_ptr<LogFile>  	mLogFile;
        string		           	mLogPrefix;
     	LogLevel	           	mLogLevel;
		static int				mNrOfInstances;

		        	           	// prevent copying and assignment
        			           	FileLog(const FileLog& logFile);
        FileLog& 				operator=(const FileLog &);

    public:
        		   		       	FileLog();
       		   		   		   ~FileLog();
        string			        GetLogPrefix();
        void			        SetLogPrefix(const string& prefix);
        LogLevel		        GetLogLevel();
        void			        SetCutOffLogLevel(const LogLevel& lvl);
        bool			        Init(const string& logPrefix = "none", const LogLevel& level = lDebug5, unique_ptr<LogFile> logFile = unique_ptr<LogFile>());
        void 	   				write(const char* str);
        bool					mLogToServer;
        string					GetLogFileName();
        int						GetNrOfInstances(){return mNrOfInstances;}
};

extern RR_DECLSPEC FileLog gLog;
}

#endif

#ifndef rrLogFileH
#define rrLogFileH
#include <memory>
#include <string>
#include "rrLogLevel.h"
#include "rrObject.h"

using std::unique_ptr;
using std::FILE;
using std::string;
//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII

using namespace rr;
class RR_DECLSPEC rrLogFile : public rrObject
{
	private:
                    			// prevent copying and assignment
                				rrLogFile(const rrLogFile& logFile);
                				rrLogFile& operator=(const rrLogFile&);
		string					mFileName;

	public:
	  							rrLogFile(const char* fName);
			   				   ~rrLogFile();
		FILE* 					mFILEHandle;
        string					GetFileName(){return mFileName;}
};

#endif

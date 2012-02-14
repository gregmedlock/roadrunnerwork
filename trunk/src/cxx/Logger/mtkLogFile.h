#ifndef mtkLogFileH
#define mtkLogFileH
#include <memory>
//#include <fstream>
#include <string>
#include "rrExporter.h"
#include "mtkLogLevel.h"
//#include "rrObject.h"

using std::unique_ptr;
using std::FILE;
using std::string;
//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII

using namespace rr;
class RR_DECLSPEC mtkLogFile //: public rrObject
{
	private:
                    			// prevent copying and assignment
                				mtkLogFile(const mtkLogFile& logFile);
                				mtkLogFile& operator=(const mtkLogFile&);
		string					mFileName;

	public:
	  							mtkLogFile(const char* fName);
			   				   ~mtkLogFile();
		FILE* 					mFILEHandle;
        string					GetFileName(){return mFileName;}
};

#endif

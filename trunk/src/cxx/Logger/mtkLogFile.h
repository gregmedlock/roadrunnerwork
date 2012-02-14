#ifndef mtkLogFileH
#define mtkLogFileH
#include <memory>
#include <fstream>
#include "CommonExporter.h"
#include "mtkLogLevel.h"
//#include "mtkObject.h"

using std::unique_ptr;
using std::FILE;
//Global class holding logfile and other settings. Persist trougout the life of the application that is using it. Based on RAII

using namespace mtk;
class MTK_COMMON mtkLogFile //: public mtkObject
{
	private:
                    			// prevent copying and assignment
                				mtkLogFile(const mtkLogFile& logFile);
                				mtkLogFile& operator=(const mtkLogFile&);
	public:
	  							mtkLogFile(const char* fName);
			   				   ~mtkLogFile();
		FILE* 					mFILEHandle;
};

#endif

#ifndef rrLogFileReaderH
#define rrLogFileReaderH
//---------------------------------------------------------------------------
#include <string>
#include <fstream>
#include "mtkThread.h"

class TMForm;
namespace rr
{
//Class that starts wtail process and read its output.
//The output is forwarded to an anonomous pipe by a pipe client,
//owned by the TailReaderProcess


using namespace std;
//using namespace mtk;
class LogFileReader : public mtkThread
{
    protected:
        ifstream                mFS;
        string                  mFileName;
        TMForm*                 mMainForm;
    public:
                                LogFileReader(const string& fName = "", TMForm* mainForm = NULL);
        void                    Worker();
        void                    SetFileName(const string& fName);
};


}
#endif

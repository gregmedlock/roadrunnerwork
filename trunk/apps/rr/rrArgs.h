//---------------------------------------------------------------------------
#ifndef CommandLineParametersH
#define CommandLineParametersH
#include <windows.h>
#include <shlobj.h>
#include "rrUtils.h"
#include "rrLogger.h"
//---------------------------------------------------------------------------
using namespace rr;
class Args
{
    public:
                                        Args();
        virtual                        ~Args(){}
        bool                            UseOSTempFolder;    //option u
        bool                            OnlyCompile;
        bool                            Pause;
        LogLevel                        LogLevel;
        string                          DataOutputFolder;   //option d:
        string                          TempDataFolder;     //option t:
        string                          ModelFileName;      //option m:
};


#endif

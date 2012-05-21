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
        bool                            OnlyCompile;        //option c
        bool                            Pause;              //option p
        double                          StartTime;
        double                          Duration;
        double                          EndTime;
        int                             Steps;
        LogLevel                        LogLevel;           //option v:
        string                          DataOutputFolder;   //option d:
        string                          TempDataFolder;     //option t:
        string                          ModelFileName;      //option m:
        string                          SelectionList;      //option l:
};


#endif

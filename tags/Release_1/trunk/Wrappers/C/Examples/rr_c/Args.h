//---------------------------------------------------------------------------
#ifndef CommandLineParametersH
#define CommandLineParametersH
#include <windows.h>
#include <shlobj.h>
#include <string>
#include "rrLogger.h"
//---------------------------------------------------------------------------
using std::string;
string Usage(const string& prg);
using namespace rr;
class Args
{
    public:
                                        Args();
        virtual                        ~Args(){}
        LogLevel                        LogLevel;           //option v:
        string                          ModelFileName;      //option m:
        string                          DataOutputFolder;   //option d:
        string                          TempDataFolder;     //option t:
        bool                            Pause;              //option p
        bool                            OnlyCompile;        //option c
        bool                            UseOSTempFolder;    //option u
        double                          StartTime;          //option s
        double                          Duration;
        double                          EndTime;            //option e
        int                             Steps;              //option z
        string                          SelectionList;      //option l:
};

#endif

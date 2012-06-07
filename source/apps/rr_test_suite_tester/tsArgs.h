//---------------------------------------------------------------------------
#ifndef tsArgsH
#define tsArgsH
//#include <windows.h>
//#include <shlobj.h>
#include <string>
#include "rrLogger.h"
//---------------------------------------------------------------------------
namespace rr
{

using std::string;
string tsUsage(const string& prg);

class tsArgs
{
    public:
                                        tsArgs();
        virtual                        ~tsArgs(){}
        int                             CaseNumber;
        double                          ErrorThreshold;
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
}

#endif

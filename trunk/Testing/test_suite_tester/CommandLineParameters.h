//---------------------------------------------------------------------------
#ifndef CommandLineParametersH
#define CommandLineParametersH
#include <windows.h>
#include <shlobj.h>
#include "rrUtils.h"
#include "rrLogger.h"
//---------------------------------------------------------------------------
using namespace rr;
class Paras
{
    public:
                                        Paras();
        virtual                        ~Paras(){}
        int                             CaseNumber;
        double                          ErrorThreshold;
        bool                            OnlyCompile;
        bool                            Pause;
        LogLevel                        LogLevel;
        string                          DataOutputFolder; //option d:
        string                          TempDataFolder;   //option t:
};


#endif

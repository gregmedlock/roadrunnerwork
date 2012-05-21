#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include <iomanip>
#include "rrUsage.h"
//---------------------------------------------------------------------------
using namespace std;
#pragma package(smart_init)

//                case ('v'): Args.LogLevel                      = StringToLogLevel(optarg);     break;
//                case ('c'): Args.OnlyCompile                   = true;                         break;
//                case ('p'): Args.Pause                         = true;                         break;
//                case ('t'): Args.TempDataFolder                = optarg;                       break;
//                case ('d'): Args.DataOutputFolder              = optarg;                       break;
//                case ('m'): Args.ModelFileName                 = optarg;                       break;

string Usage(const string& prg)
{
    stringstream usage;
    usage << "\nUSAGE for "<<prg<<"\n\n";
    usage<<left;
    usage<<setfill('.');
    usage<<setw(25)<<"-v<debug level>"              <<" Debug levels: Error, Warning, Info, Debug, Debug'n', where n is 1-7\n";
    usage<<setw(25)<<"-m<FileName>"                 <<" SBML Model File Name (with path)\n";
    usage<<setw(25)<<"-d<FilePath>"                 <<" Data output folder. If not given, data is output to console\n";
    usage<<setw(25)<<"-t<FilePath>"                 <<" Temporary data output folder. If not given, temp files are output to current directory\n";
    usage<<setw(25)<<"-p"                           <<" Pause before exiting.\n";
    usage<<setw(25)<<"-c"                           <<" Stop execution after compiling model\n";
    usage<<setw(25)<<"-u"                           <<" Use users OS designated temporary folder\n";
    usage<<setw(25)<<"-? "                          <<" Shows the help screen.\n\n";

    usage<<"\nSystems Biology, UW 2012\n";
    return usage.str();
}



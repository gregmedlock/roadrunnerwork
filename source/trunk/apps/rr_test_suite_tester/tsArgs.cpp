#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <strstream>
#include <iomanip>
#include "tsArgs.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEAR__)
#pragma package(smart_init)
#endif

Args::Args()
:
UseOSTempFolder(false),
OnlyCompile(false),
Pause(false),
CurrentLogLevel(lInfo),
ModelFileName(""),
DataOutputFolder(""),
TempDataFolder("."),
StartTime(0),
Duration(5),
EndTime(StartTime + Duration),
CaseNumber(1),
ErrorThreshold(1.e-6),
Steps(50)
{
    //Default for data output is in the same dir as where the executable is

    //CSIDL_MYDOCUMENTS
//    TCHAR myDocs[MAX_PATH];
//    HRESULT result = SHGetFolderPath(NULL, CSIDL_PERSONAL, NULL, SHGFP_TYPE_CURRENT, myDocs);
//
//    if(result == S_OK)
//    {
//        //Make Sure MyDocuments/RoadRunner exists..
//        TCHAR rrDocs[MAX_PATH];
//        strcpy(rrDocs, strcat(myDocs,"\\RoadRunner"));
//        if(FolderExists(rrDocs))
//        {
//            DataOutputFolder = rrDocs;
//        }
//        else
//        {
//            if(!CreateFolder(rrDocs))
//            {
//                Log(lError)<<"No data output folder..";
//                DataOutputFolder = "";
//            }
//            else
//            {
//                DataOutputFolder = rrDocs;
//            }
//        }
//
//    }
}
//        LogLevel                        LogLevel;           //option v:
//        string                          ModelFileName;      //option m:
//        string                          DataOutputFile;     //option d:
//        string                          TempDataFolder;     //option t:
//        bool                            Pause;              //option p
//        bool                            OnlyCompile;        //option c
//        bool                            UseOSTempFolder;    //option u
//        double                          StartTime;          //option s
//        double                          Duration;
//        double                          EndTime;            //option e
//        int                             Steps;              //option z
//        string                          SelectionList;      //option l:


string Usage(const string& prg)
{
    stringstream usage;
    usage << "\nUSAGE for "<<prg<<"\n\n";
    usage<<left;
    usage<<setfill('.');
    usage<<setw(25)<<"-v<debug level>"              <<" Debug levels: Error, Warning, Info, Debug, Debug'n', where n is 1-7. Defualt: Info\n";
    usage<<setw(25)<<"-m<FileName>"                 <<" SBML Model File Name (with path)\n";
    usage<<setw(25)<<"-d<FilePath>"                 <<" Data output folder. If not given, data is output to console\n";
    usage<<setw(25)<<"-t<FilePath>"                 <<" Temporary data output folder. If not given, temp files are output to current directory\n";
    usage<<setw(25)<<"-p"                           <<" Pause before exiting.\n";
    usage<<setw(25)<<"-c"                           <<" Stop execution after compiling model\n";
    usage<<setw(25)<<"-u"                           <<" Use users OS designated temporary folder\n";
    usage<<setw(25)<<"-s<#>"                        <<" Set the start time for simulation. Default: 0\n";
    usage<<setw(25)<<"-e<#>"                        <<" Set the end time for simulation. Default: 5\n";
    usage<<setw(25)<<"-z<#>"                        <<" Set number of steps in the simulation. Default: 50\n";
    usage<<setw(25)<<"-l<List>"                     <<" Set selection list. Separate variables using ',' or space\n";
    usage<<setw(25)<<"-? "                          <<" Shows the help screen.\n\n";

    usage<<"\nSystems Biology, UW 2012\n";
    return usage.str();
}


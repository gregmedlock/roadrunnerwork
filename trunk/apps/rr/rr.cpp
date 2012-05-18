#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <tchar.h>

#if defined(__CODEGEARC__)
#include <dir.h>
#else
#include <direct.h>
#endif

#include <iomanip>
#include "rrLog.h"
#include "rrRoadRunner.h"
#include "rrCGenerator.h"
#include "rrException.h"
#include "rrUtils.h"
#include "rrStringUtils.h"
#include "rrSBMLModelSimulation.h"
#include "rrGetOptions.h"
#include "CommandLineParameters.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;
string Usage(const string& prg);

int main(int argc, char * argv[])
{
    try
    {

    LogOutput::mLogToConsole = true;

    if(argc < 2)
    {
        cout<<Usage(argv[0])<<endl;
        exit(0);
    }

    Paras paras;
    char c;
    while ((c = GetOptions(argc, argv, ("cpv:n:d:t:m:"))) != -1)
    {
        switch (c)
        {
            case ('v'): paras.LogLevel                      = StringToLogLevel(optarg);     break;
            case ('c'): paras.OnlyCompile                   = true;                         break;
            case ('p'): paras.Pause                         = true;                         break;
            case ('t'): paras.TempDataFolder                = optarg;                       break;
            case ('d'): paras.DataOutputFolder              = optarg;                       break;
            case ('m'): paras.ModelFileName                 = optarg;                       break;
            case ('?'):
            {
                    cout<<Usage(argv[0])<<endl;
            }
            default:
            {
                string str = argv[optind-1];
                if(str != "-?")
                {
                    cout<<"*** Illegal option:\t"<<argv[optind-1]<<" ***\n"<<endl;
                }
                exit(-1);
            }
        }
    }


    gLog.SetCutOffLogLevel(paras.LogLevel);
    string logFileName;

    RoadRunner *rr = NULL;
    if(!(paras.TempDataFolder.size()))
    {
        throw(rr::Exception("RoadRunner failed to assign a temporary data folder"));
    }


    gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile("RoadRunner.log")));
    Log(lShowAlways)<<"Logs are going to "<<gLog.GetLogFileName();

    Log(lShowAlways)<<"Log level is:" <<LogLevelToString(gLog.GetLogLevel());
    SBMLModelSimulation simulation(paras.DataOutputFolder);

    rr = new RoadRunner();
    rr->Reset();
    simulation.UseEngine(rr);

    //The following will load and compile and simulate the sbml model in the file



    if(!paras.ModelFileName.size())
    {
        Log(lInfo)<<"Please supply a sbml model file name, using option -m<modelfilename>";
    }
    else
    {
        simulation.SetModelFileName(paras.ModelFileName);
        simulation.CompileIfDllExists(true);
        if(!simulation.LoadSBMLFromFile())
        {
            Log(lError)<<"Failed loading SBML model";
            goto end;
        }

        if(!simulation.GenerateModelCode())
        {
            Log(lError)<<"Failed loading SBML model";
            goto end;
        }

        if(!simulation.CompileModel())
        {
                Log(lError)<<"Failed compiling SBML model:" <<paras.ModelFileName;
            goto end;
        }

        if(paras.OnlyCompile)
        {
            goto end;
        }

        if(!simulation.CreateModel())
        {
            Log(lError)<<"Failed creating Model";
            goto end;
        }

        //First load the model
        if(!simulation.InitializeModel())
        {
            Log(lError)<<"Failed initializing SBML model";
            goto end;
        }

        //Then read settings file if it exists..
        string settingsOveride("");//C:\\rrw\\Models\\settings_override.txt");
        if(!simulation.LoadSettings(settingsOveride))    //set selection list here!
        {
            Log(lError)<<"Failed loading SBML model settings";
        }

        //        rr->ComputeAndAssignConservationLaws(true);
        //Then Simulate model
        if(!simulation.Run())
        {
            Log(lError)<<"Failed running simulation";
            throw("Failed running simulation");
        }

        //Write result
        if(!simulation.SaveResult())
        {
            //Failed to save data
        }

        simulation.SaveAllData();

        }

        end:

        Log(lInfo)<<"Done";
        if(paras.Pause)
        {
            Pause();
        }

        delete rr;
    }
    catch(rr::Exception& ex)
    {
        Log(lError)<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }


    return 0;
}

string Usage(const string& prg)
{
    stringstream usage;
    usage << "\nUSAGE for "<<prg<<": (options and parameters)\n\n";
    usage<<left;
    usage<<setfill('.');
    usage<<setw(25)<<"-v<debug level>"                <<" Debug level Error, Warning, Info, Debugn, where n is 1-7\n";
    usage<<setw(25)<<"-? "                              <<" Shows the help screen.\n\n";

    usage<<"\nSystems Biology, UW 2012\n";
    return usage.str();
}



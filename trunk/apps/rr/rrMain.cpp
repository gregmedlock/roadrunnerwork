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
#include "rrArgs.h"
#include "rrUsage.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

void ProcessCommandLineArguments(int argc, char* argv[], Args& args);
int main(int argc, char * argv[])
{
    string settingsOveride;
    try
    {
        LogOutput::mLogToConsole = true;

        if(argc < 2)
        {
            cout<<Usage(argv[0])<<endl;
            exit(0);
        }

        Args args;
        ProcessCommandLineArguments(argc, argv, args);

        gLog.SetCutOffLogLevel(args.LogLevel);
        string logFileName;

        RoadRunner *rr = NULL;

        if(args.UseOSTempFolder)
        {
            args.TempDataFolder = GetUsersTempDataFolder();
        }

        gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile("RoadRunner.log")));
        Log(lShowAlways)<<"Logs are going to "<<gLog.GetLogFileName();

        Log(lShowAlways)<<"Log level is:" <<LogLevelToString(gLog.GetLogLevel());
        SBMLModelSimulation simulation(args.DataOutputFolder, args.TempDataFolder);

        rr = new RoadRunner();
        rr->Reset();
        simulation.UseEngine(rr);

        //The following will load and compile and simulate the sbml model in the file
        if(!args.ModelFileName.size())
        {
            Log(lInfo)<<"Please supply a sbml model file name, using option -m<modelfilename>";
            goto end;
        }

        if(!simulation.SetModelFileName(args.ModelFileName))
        {
            Log(lInfo)<<"Bad model file";
            goto end;
        }

        simulation.CompileIfDllExists(true);
        if(!simulation.LoadSBMLFromFile())
        {
            Log(lError)<<"Failed loading SBML model";
            goto end;
        }
        Log(lInfo)<<"SBML semantics was loaded from file: "<<simulation.GetModelsFullFilePath();
        if(!simulation.GenerateModelCode())
        {
            Log(lError)<<"Failed loading SBML model";
            goto end;
        }

        if(!simulation.CompileModel())
        {
            Log(lError)<<"Failed compiling SBML model:" <<args.ModelFileName;
            goto end;
        }

        if(args.OnlyCompile)
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
        settingsOveride = ("");//C:\\rrw\\Models\\settings_override.txt");
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

        if(args.DataOutputFolder.size())
        {
            //Write result
            if(!simulation.SaveResult())
            {
                //Failed to save data
            }
        }
        else
        {
            //Write to std out
        }

        //All paths leads to end..
        end:

        Log(lInfo)<<"RoadRunner is exiting...";
        if(args.Pause)
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

void ProcessCommandLineArguments(int argc, char* argv[], Args& args)
{
    char c;
    while ((c = GetOptions(argc, argv, ("cpuv:n:d:t:m:"))) != -1)
    {
        switch (c)
        {
            case ('v'): args.LogLevel                      = StringToLogLevel(optarg);     break;
            case ('c'): args.OnlyCompile                   = true;                         break;
            case ('p'): args.Pause                         = true;                         break;
            case ('t'): args.TempDataFolder                = optarg;                       break;
            case ('d'): args.DataOutputFolder              = optarg;                       break;
            case ('m'): args.ModelFileName                 = optarg;                       break;
            case ('u'): args.UseOSTempFolder               = true;                         break;
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
}

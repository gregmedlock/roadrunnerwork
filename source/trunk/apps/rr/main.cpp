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
#include "Args.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

void ProcessCommandLineArguments(int argc, char* argv[], Args& args);
int main(int argc, char * argv[])
{
    string settingsFile;
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

        gLog.SetCutOffLogLevel(args.CurrentLogLevel);
        string logFileName;

        RoadRunner *rr = NULL;

        if(args.UseOSTempFolder)
        {
            args.TempDataFolder = GetUsersTempDataFolder();
        }

        if(args.ModelFileName.size())
        {
            string logName = ExtractFileName(args.ModelFileName);
            logName = ChangeFileExtensionTo(logName, ".log");
            gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(JoinPath(args.TempDataFolder, logName) )));
        }
        else
        {
            gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(JoinPath(args.TempDataFolder, "RoadRunner.log") )));
        }
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
        if(settingsFile.size())
        {
            if(!simulation.LoadSettings(settingsFile))    //set selection list here!
            {
                Log(lError)<<"Failed loading SBML model settings";
            }
        }
        else //Read from commandline
        {
            simulation.SetTimeStart(args.StartTime);
            simulation.SetTimeEnd(args.EndTime);
            simulation.SetNumberOfPoints(args.Steps);
            simulation.SetSelectionList(args.SelectionList);
        }

        //rr->ComputeAndAssignConservationLaws(true);
        //Then Simulate model
        if(!simulation.Simulate())
        {
            Log(lError)<<"Failed running simulation";
            throw("Failed running simulation");
        }

        if(args.SaveResultToFile)
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
            SimulationData result = simulation.GetResult();
            Log(lShowAlways)<<result;

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
    while ((c = GetOptions(argc, argv, ("cpufv:n:d:t:l:m:s:e:z:"))) != -1)
    {
        switch (c)
        {
            case ('v'): args.CurrentLogLevel                       = StringToLogLevel(optarg);     break;
            case ('c'): args.OnlyCompile                    = true;                         break;
            case ('p'): args.Pause                          = true;                         break;
            case ('t'): args.TempDataFolder                 = optarg;                       break;
            case ('d'): args.DataOutputFolder               = optarg;                       break;
            case ('m'): args.ModelFileName                  = optarg;                       break;
            case ('u'): args.UseOSTempFolder                = true;                         break;
            case ('l'): args.SelectionList                  = optarg;                       break;
            case ('s'): args.StartTime                      = ToDouble(optarg);             break;
            case ('e'): args.EndTime                        = ToDouble(optarg);             break;
            case ('z'): args.Steps                          = ToInt(optarg);                break;
            case ('f'): args.SaveResultToFile               = true;                         break;
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

    //Check arguments, and choose to bail here if something is not right...

}

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
#include "TestSuiteModelSimulation.h"
#include "rrGetOptions.h"
#include "tsArgs.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;
string tsUsage(const string& prg);

int main(int argc, char * argv[])
{

    LogOutput::mLogToConsole = true;

    if(argc < 2)
    {
        cout<<tsUsage(argv[0])<<endl;
        exit(0);
    }

    tsArgs paras;
    char c;
    while ((c = GetOptions(argc, argv, ("cpv:n:d:t:"))) != -1)
    {
        switch (c)
        {
            case ('n'): paras.CaseNumber                    = ToInt(optarg);                break;
            case ('v'): paras.CurrentLogLevel               = StringToLogLevel(optarg);     break;
            case ('c'): paras.OnlyCompile                   = true;                         break;
            case ('p'): paras.Pause                         = true;                         break;
            case ('t'): paras.TempDataFolder                = optarg;                       break;
            case ('d'): paras.DataOutputFolder              = optarg;                       break;
            case ('?'):
            {
                    cout<<tsUsage(argv[0])<<endl;
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

    vector<int> exceptions;
    exceptions.push_back(374);
    exceptions.push_back(748);

    if(std::find(exceptions.begin(), exceptions.end(), paras.CaseNumber) != exceptions.end())
    {
        //Don't simulate this one..
        throw(rr::Exception("Bad model"));
    }

    gLog.SetCutOffLogLevel(paras.CurrentLogLevel);
    string dataOutputFolder("C:\\DataOutput");
    string dummy;
    string logFileName;

    CreateTestSuiteFileNameParts(paras.CaseNumber, ".log", dummy, logFileName);
    RoadRunner *rr = NULL;
    try
    {
        //Create subfolder for data output
        dataOutputFolder = JoinPath(dataOutputFolder, GetTestSuiteSubFolderName(paras.CaseNumber));

        if(!CreateFolder(dataOutputFolder))
        {
            throw(rr::Exception("Failed creating output folder for data output"));
        }

        gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(JoinPath(dataOutputFolder, logFileName))));
        Log(lShowAlways)<<"Logs are going to "<<gLog.GetLogFileName();

        Log(lShowAlways)<<"Log level is:" <<LogLevelToString(gLog.GetLogLevel());
        TestSuiteModelSimulation simulation(dataOutputFolder);

        //dataOutputFolder += dummy;
        rr = new RoadRunner();
        rr->Reset();
        simulation.UseEngine(rr);

        //Read SBML models.....
        string modelFilePath("C:\\SBMLTestCases\\all");
        string modelFileName;

        simulation.SetCaseNumber(paras.CaseNumber);
        CreateTestSuiteFileNameParts(paras.CaseNumber, "-sbml-l2v4.xml", modelFilePath, modelFileName);

        //The following will load and compile and simulate the sbml model in the file
        simulation.SetModelFilePath(modelFilePath);
        simulation.SetModelFileName(modelFileName);
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
                Log(lError)<<"Failed compiling SBML model:" <<paras.CaseNumber;
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
        if(!simulation.Simulate())
        {
            Log(lError)<<"Failed running simulation";
            throw("Failed running simulation");
        }

        //Write result
        if(!simulation.SaveResult())
        {
            //Failed to save data
        }

        if(!simulation.LoadReferenceData())
        {
            Log(lError)<<"Failed loading reference data";
        }

        simulation.CreateErrorData();

        //Check error data.. if an error in the set is larger than threshold, signal an error
        if(simulation.GetSimulationError() > paras.ErrorThreshold)
        {
            Log(lError)<<"********** Error larger than "<<paras.ErrorThreshold;
        }
        else
        {
            Log(lError)<<"Passed Test: "<<paras.CaseNumber<<" Largest error was: "<<simulation.GetSimulationError();
        }

        simulation.SaveAllData();
        simulation.SaveModelAsXML(dataOutputFolder);

    }
    catch(rr::Exception& ex)
    {
        Log(lError)<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }

    end:    //I have not used a label in 15 years!
    delete rr;
    Log(lInfo)<<"Done";
    if(paras.Pause)
    {
        Pause();
    }

    return 0;
}

string Usage(const string& prg)
{
    stringstream usage;
    usage << "\nUSAGE for "<<prg<<": (options and parameters)\n\n";
    usage<<left;
    usage<<setfill('.');
    usage<<setw(25)<<"-n\"number\" "    <<" TestSuite number\n";
    usage<<setw(25)<<"-v"                <<" Verbose mode. Ouputs information during program execution\n";
    usage<<setw(25)<<"-? "               <<" Shows the help screen.\n\n";

    usage<<"\nSystems Biology, UW 2012\n";
    return usage.str();
}


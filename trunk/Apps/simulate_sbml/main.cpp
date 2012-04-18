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

	if(argc < 2)
	{
		cout<<Usage(argv[0])<<endl;
		exit(0);
	}

    Paras paras;
    int c;
	while ((c = GetOptions(argc, argv, ("n:"))) != -1)
	{
		switch (c)
		{
			case ('n'): paras.CaseNumber  				= ToInt(optarg);       	break;
//			case ('v'): paras.mVerboseMode 				= true; 				break;
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
            break;
		}
	}

    string dataOutputFolder("C:\\rrw\\DataOutput\\XE");
    string dummy;
    string logFileName;

    CreateTestSuiteFileNameParts(paras.CaseNumber, ".log", dummy, logFileName);
    LogOutput::mLogToConsole = true;
    try
    {
        //Create subfolder for data output
        dataOutputFolder = JoinPath(dataOutputFolder, GetTestSuiteSubFolderName(paras.CaseNumber));

        if(!CreateFolder(dataOutputFolder))
        {
            throw(Exception("Failed creating output folder for data output"));
        }

        gLog.Init("", lDebug5, unique_ptr<LogFile>(new LogFile(JoinPath(dataOutputFolder, logFileName))));
	    Log(lInfo)<<"Logs are going to "<<gLog.GetLogFileName();
	    gLog.SetCutOffLogLevel(lInfo);

        SBMLModelSimulation simulation(dataOutputFolder);
        //dataOutputFolder += dummy;
        RoadRunner *roadRunner = NULL;
        roadRunner = new RoadRunner;
        roadRunner->Reset();
        simulation.UseEngine(roadRunner);

        //Read SBML models.....
        string modelFilePath("C:\\rrw\\Models\\sbml-test-cases-2.0.2\\cases\\semantic");
        string modelFileName;

        simulation.SetCaseNumber(paras.CaseNumber);
        CreateTestSuiteFileNameParts(paras.CaseNumber, "-sbml-l2v4.xml", modelFilePath, modelFileName);

        //The following will load and compile and simulate the sbml model in the file
        simulation.SetModelFilePath(modelFilePath);
        simulation.SetModelFileName(modelFileName);
        simulation.CompileIfDllExists(true);

        //First load the model
        if(!simulation.LoadModel())
        {
            Log(lError)<<"Failed loading SBML model";
        }

        //Then read settings file if it exists..
        string settingsOveride("");//C:\\rrw\\Models\\settings_override.txt");
        if(!simulation.LoadSettings(settingsOveride))
        {
            Log(lError)<<"Failed loading SBML model settings";
        }

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

        if(!simulation.LoadReferenceData())
        {
            Log(lError)<<"Failed loading reference data";
        }


        simulation.CreateErrorData();
        simulation.SaveAllData();
	    delete roadRunner;
    }
	catch(Exception& ex)
	{
    	Log(lError)<<"RoadRunner exception occured: "<<ex.what()<<endl;
	}


    Pause();
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



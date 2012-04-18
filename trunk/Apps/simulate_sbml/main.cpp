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
//---------------------------------------------------------------------------

using namespace std;
using namespace rr;

int main()
{
    string dataOutputFolder("C:\\rrw\\DataOutput\\XE");
    string dummy;
    string logFileName;
    int caseNumber = 3;
    CreateTestSuiteFileNameParts(caseNumber, ".log", dummy, logFileName);
    LogOutput::mLogToConsole = true;
    try
    {
        //Create subfolder for data output
        dataOutputFolder = JoinPath(dataOutputFolder, GetTestSuiteSubFolderName(caseNumber));

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

        simulation.SetCaseNumber(caseNumber);
        CreateTestSuiteFileNameParts(caseNumber, "-sbml-l2v4.xml", modelFilePath, modelFileName);

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



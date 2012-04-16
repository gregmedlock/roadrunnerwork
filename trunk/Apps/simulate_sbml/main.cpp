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

	SBMLModelSimulation simulation(dataOutputFolder);

	string dummy;
    string logFileName;
    int caseNumber = 1;
    CreateTestSuiteFileNameParts(caseNumber, ".log", dummy, logFileName);

    //dataOutputFolder += dummy;
    gLog.Init("", lDebug5, unique_ptr<LogFile>(new LogFile(JoinPath(dataOutputFolder, logFileName))));
    LogOutput::mLogToConsole = true;

    gLog.SetCutOffLogLevel(lDebug3);

    Log(lDebug4)<<"Logs are going to "<<gLog.GetLogFileName();
	RoadRunner *roadRunner = NULL;
    try
    {
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
		simulation.CompileIfDllExists(false);
        //First load the model
		if(!simulation.LoadModel())
        {
        	Log(lError)<<"Failed loading SBML model";
        }

        //Then read settings file if it exists..
        string settingsOveride("C:\\rrw\\Models\\settings_override.txt");
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
    }
    catch(Exception& ex)
    {
        Log(lError)<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }

    delete roadRunner;

  	//-------------------------------------
    Pause();
	return 0;
}



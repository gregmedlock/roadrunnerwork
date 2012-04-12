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
    string dataOutputFolder("C:\\rrw\\DataOutput");

	SBMLModelSimulation simulation(dataOutputFolder);

	string dummy;
    string logFileName;
    int caseNumber = 19;
    CreateTestSuiteFileNameParts(caseNumber, ".log", dummy, logFileName);
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
        string modelFilePath("C:\\rrw\\Models\\l2v4_full");
        string modelFileName;

        simulation.SetCaseNumber(caseNumber);

        CreateTestSuiteFileNameParts(caseNumber, "-sbml-l2v4.xml", modelFilePath, modelFileName);

        //The following will load and compile and simulate the sbml model in the file
        simulation.SetModelFilePath(modelFilePath);
        simulation.SetModelFileName(modelFileName);

        //First load the model
		if(!simulation.LoadModel())
        {
        	Log(lError)<<"Failed loading SBML model";
        }

        //Then read settings file if it exists..
		if(!simulation.LoadSettings())
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



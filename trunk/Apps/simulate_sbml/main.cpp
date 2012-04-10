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
	SBMLModelSimulation simulation;

    gLog.Init("", lDebug5, unique_ptr<LogFile>(new LogFile("simulate_sbml.log")));
    LogOutput::mLogToConsole = true;

    gLog.SetCutOffLogLevel(lDebug4);
	//gLog.SetCutOffLogLevel(lInfo);

    Log(lDebug4)<<"Logs are going to "<<gLog.GetLogFileName();
    RoadRunner *roadRunner = NULL;

    try
    {
        roadRunner = new RoadRunner;
        roadRunner->Reset();
        simulation.UseEngine(roadRunner);

        string modelFilePath("C:\\rrw\\Models");

        string subFolder("l2v4");
        //string subFolder("");
        stringstream modelSubPath;
        stringstream modelFileName;

        int caseNr = 1;
        //int caseNr = 41;
        modelSubPath<<setfill('0')<<setw(5)<<caseNr;		//create the "00023" subfolder format
		modelFileName<<setfill('0')<<setw(5)<<caseNr<<"-sbml-l2v4.xml";

        if(subFolder.size())
        {
            modelFilePath = modelFilePath + "\\" + subFolder + "\\" + modelSubPath.str();
        }

		//modelFileName<<"simple.xml";
        string fullFilePath(modelFilePath +   "\\" + modelFileName.str());

        //The following will load and compile and simulate the sbml model in the file

        simulation.SetModelFilePath(modelFilePath);
        simulation.SetModelFileName(modelFileName.str());

        //First load the model
		if(!simulation.LoadModel())
        {
        	Log(lError)<<"Failed loading SBML model";
        }

        //Then Read settings file if it exists..
		if(!simulation.LoadSettings())
        {
        	Log(lError)<<"Failed loading SBML model settings";
        }

        //Then Simulate model
		if(!simulation.Run())
        {
        	Log(lError)<<"Failed running simulation";
        }

        //Write result
        if(!simulation.SaveResult())
        {
        	//Failed to simulate the model...

        }

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


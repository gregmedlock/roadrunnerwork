#pragma hdrstop
#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <conio.h>
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
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#pragma argsused

using namespace std;
using namespace rr;

void PauseBeforeExit(bool doIt = true);

int main()
{
	_control87(MCW_EM,MCW_EM);
    bool generateCSharp = false;
    char exePath[MAX_PATH];
    getcwd(exePath, MAX_PATH);
    string appPath(exePath);
    gLog.Init("simulateModel", lDebug5, unique_ptr<LogFile>(new LogFile("SimulateModel.log")));
    LogOutput::mLogToConsole = true;
    gLog.SetCutOffLogLevel(lDebug2);
//       	gLog.SetCutOffLogLevel(lInfo);

    Log(lDebug4)<<"Logs are going to "<<exePath<<"\\"<<gLog.GetLogFileName()<< " (and cout)";
    RoadRunner *roadRunner = NULL;

    try
    {
        roadRunner = new RoadRunner;
        roadRunner->Reset();
        string modelsRootPath("C:\\rrw\\Models");
//        string subFolder("l2v4");
//
//        stringstream modelSubPath;
        stringstream modelFName;
//
//        int caseNr = 1;
//        //int caseNr = 41;
//
//        modelSubPath<<setfill('0')<<setw(5)<<caseNr;		//create the "00023" subfolder format
//        modelFName<<setfill('0')<<setw(5)<<caseNr<<"-sbml-l2v4.xml";
//
//        if(subFolder.size())
//        {
//            modelsRootPath = modelsRootPath + "\\" + subFolder + "\\" + modelSubPath.str();
//        }
//
//        string fullFilePath(modelsRootPath +   "\\" + modelFName.str());

		modelFName<<"simple.xml";
        string fullFilePath(modelsRootPath +   "\\" + modelFName.str());

        //The following will load and compile and simulate the sbml model in the file
        roadRunner->SimulateSBMLFile(fullFilePath, true);
    }
    catch(Exception& ex)
    {
        Log(lError)<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }

    delete roadRunner;

  	//-------------------------------------
    PauseBeforeExit();
	return 0;
}

void PauseBeforeExit(bool doIt)
{
	if(!doIt)
    {
    	return;
    }

    cout<<"Hit any key to exit...";
    cin.ignore(0,'\n');
    getch();
    cout<<"\nExiting....\n";
}

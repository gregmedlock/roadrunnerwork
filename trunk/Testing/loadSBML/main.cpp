#pragma hdrstop
#include <iostream>
#include <fstream>
#include <string>
#include <conio.h>
#include <tchar.h>
#include <dir.h>
#include <iomanip>
#include "Logger/rrLog.h"
#include "rrException.h"
#include "rrRoadRunner.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
    try
    {

        char exePath[MAXPATH];
        getcwd(exePath, MAXPATH);
        gLog.Init("loadSBML", lDebug5, unique_ptr<LogFile>(new LogFile("LoadSBML.log")));
        LogOutput::mLogToConsole = true;
        gLog.SetCutOffLogLevel(lDebug5);
        Log(lDebug4)<<"Logs are going to "<<exePath<<"\\"<<gLog.GetLogFileName()<< " (and cout)";


        //Loading models
        string modelsPath("C:\\RRW\\Testing\\models\\test_cases_l2v4");
        int caseNr = 1;
        stringstream model;
        model <<setfill('0')<<setw(5)<<caseNr<<"\\"<<setw(5)<<caseNr<<"-sbml-l2v4.xml";
        string fullFilePath(modelsPath +   "\\\\" + model.str());

//        string model("feedback.xml");
//    	string fullFilePath(modelsPath + "\\\\" + model);


        ifstream ifs(fullFilePath.c_str());
        if(!ifs)
        {
            throw(Exception("Failed to read file:" + fullFilePath));
        }
   		Log(lDebug5)<<"Loaded mode file:"<<fullFilePath;

        std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());


		RoadRunner *rr = new RoadRunner;
		Log(lDebug5)<<"Before loading SBML..SBML string size: "<<sbml.size();
    	rr->loadSBML(sbml);

        //Save source code
        string srcCodeFileName(modelsPath + "//model_code//" + model.str() + "_C++.txt");
        string code = rr->GetModelSourceCode();
        if(code.size())
        {
        	ofstream outFile(srcCodeFileName.c_str());
            outFile<<code;
        }

        cout<<"Copyright: "<<rr->getCopyright()<<endl;
    }
    catch(const Exception& ex)
    {
		cout<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }

  	//-------------------------------------
	cout<<"Hit any key to exit...";
	cin.ignore(0,'\n');
    getch();
	return 0;
}


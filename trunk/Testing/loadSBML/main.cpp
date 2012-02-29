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

//        gLog.SetCutOffLogLevel(lDebug5);
        gLog.SetCutOffLogLevel(lWarning);

        Log(lDebug4)<<"Logs are going to "<<exePath<<"\\"<<gLog.GetLogFileName()<< " (and cout)";

	    RoadRunner *roadRunner = NULL;

        //Loading models
		for(int caseNr = 1; caseNr < 100; caseNr++)
        {
        	//int caseNr = 41;
			if(roadRunner)
            {
            	delete roadRunner;
            }

            roadRunner = new RoadRunner;
            roadRunner->Reset();
	        string modelsRootPath("C:\\RRW\\Testing\\models");
            //int caseNr = 1;
            stringstream modelSubPath;
            stringstream modelFName;

            string subFolder("test_cases_l2v4");
            modelSubPath <<setfill('0')<<setw(5)<<caseNr;

            modelFName<<setfill('0')<<setw(5)<<caseNr<<"-sbml-l2v4.xml";

            //string subFolder("");
            //model<<"feedback.xml";
            if(subFolder.size())
            {
                modelsRootPath = modelsRootPath + "\\" + subFolder + "\\" + modelSubPath.str();
            }

            string fullFilePath(modelsRootPath +   "\\\\" + modelFName.str());

            ifstream ifs(fullFilePath.c_str());
            if(!ifs)
            {
                throw(Exception("Failed to read file:" + fullFilePath));
            }
            Log(lDebug5)<<"Trying to load model file:"<<fullFilePath;
            std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
            ifs.close();
            Log(lDebug5)<<"Before loading SBML..SBML string size: "<<sbml.size();
            roadRunner->loadSBML(sbml);

            //Save source code
            string srcCodeFileName("C:\\RRW\\Testing\\models\\model_code//C++//" + modelFName.str() + "_C++.txt");
            string code = roadRunner->GetModelSourceCode();
            if(code.size())
            {
                ofstream outFile(srcCodeFileName.c_str());
                if(!outFile)
                {
                    throw(Exception("Failed to write file:" + srcCodeFileName));
                }
                outFile<<code;
            }
        }//test cases loop

        cout<<"Copyright: "<<roadRunner->getCopyright()<<endl;
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


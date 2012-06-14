#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <conio.h>
#include <tchar.h>
#include <direct.h>
#include <iomanip>
#include "rrRoadRunner.h"
#include "rrLog.h"
#include "rrException.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

#define MAXPATH 1024

int main(int argc, _TCHAR* argv[])
{
    try
    {
        char exePath[MAXPATH];
        _getcwd(exePath, MAXPATH);
        gLog.Init("loadSBML", lDebug5, unique_ptr<LogFile>(new LogFile("LoadSBML.log")));
        LogOutput::mLogToConsole = true;

    	gLog.SetCutOffLogLevel(lDebug4);
       	//gLog.SetCutOffLogLevel(lInfo);

        Log(lDebug4)<<"Logs are going to "<<exePath<<"\\"<<gLog.GetLogFileName()<< " (and cout)";

	    RoadRunner *roadRunner = NULL;

        //Loading models (max is 459)
		for(int caseNr = 19; caseNr < 20; caseNr++)
        {
        	//int caseNr = 41;
			if(roadRunner)
            {
            	delete roadRunner;	//Hav to do this because some initialization problems(?) in libs
            }

            roadRunner = new RoadRunner;
            roadRunner->Reset();
	        string modelsRootPath("C:\\rrw\\Models");
            string subFolder("l2v4");

            //int caseNr = 1;
            stringstream modelSubPath;
            stringstream modelFName;
            modelSubPath<<setfill('0')<<setw(5)<<caseNr;		//create the "00023" subfolder format
            modelFName<<setfill('0')<<setw(5)<<caseNr<<"-sbml-l2v4.xml";

            if(subFolder.size())
            {
                modelsRootPath = modelsRootPath + "\\" + subFolder + "\\" + modelSubPath.str();
            }

            string fullFilePath(modelsRootPath +   "\\" + modelFName.str());
            ifstream inFileStream(fullFilePath.c_str());
            if(!inFileStream)
            {
                throw(Exception("Failed to open the model file:" + fullFilePath));
            }
            Log(lInfo)<<"\n\n ===== Reading model file:"<<fullFilePath<<" ==============";
            std::string sbml((std::istreambuf_iterator<char>(inFileStream)), std::istreambuf_iterator<char>());
            inFileStream.close();
            Log(lDebug5)<<"Before loading SBML. SBML model code size: "<<sbml.size();

            //////////////////
	        roadRunner->loadSBML(sbml);
            //////////////////

            //Save source code
            string code = roadRunner->GetModelSourceCode();
            if(code.size())
            {
	            string srcCodeFileName("C:\\RRW\\Testing\\rr_code_output\\cs_from_rr++\\" + modelFName.str());
            	srcCodeFileName = ChangeFileNameExtensionTo(srcCodeFileName, ".cs");
                ofstream outFile(srcCodeFileName.c_str());
                if(!outFile)
                {
                    throw(Exception("Failed to write file:" + srcCodeFileName));
                }
                outFile<<code;
				Log(lInfo)<<"Wrote c# code to file: "<<srcCodeFileName;
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
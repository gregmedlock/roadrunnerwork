#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <io.h>
#include <algorithm>
#include <iostream>
#include <conio.h>
#include <fstream>
#include <iomanip>
#include "rrStringUtils.h"
#include "rrUtils.h"
#include "rrLogger.h"
//---------------------------------------------------------------------------
using namespace std;
namespace rr
{

vector<string> GetLinesInFile(const string& fName)
{
	vector<string> lines;

	ifstream ifs(fName.c_str());
	if(!ifs)
	{
		Log(lWarning)<<"Failed opening file: "<<fName;
        return lines;
	}

	std::string oneLine((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

    lines = SplitString(oneLine, "\n");

	return lines;

}

std::size_t IndexOf(std::vector<std::string>& vec, const std::string& elem )
{
	int index = distance(vec.begin(), find(vec.begin(), vec.end(), elem));
	return index;
}

bool IsNaN(const double& aNum)
{
	return _isnan(aNum) > 0 ? true : false;
}

bool IsNullOrEmpty(const string& str)
{
	return !str.size();
}

void Pause(bool doIt)
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

bool FileExists(const string& fName)
{
  	if (!fName.size())
 	{
		return false;
	}
    bool res = (access(fName.c_str(), 0) == 0);
    return res;
}

void CreateTestSuiteFileNameParts(int caseNr, const string& postFixPart, string& modelFilePath, string& modelName)
{
	stringstream modelSubPath;
    stringstream modelFileName;
    modelSubPath<<setfill('0')<<setw(5)<<caseNr;		//create the "00023" subfolder format
    modelFileName<<setfill('0')<<setw(5)<<caseNr<<postFixPart;
    modelFilePath = modelFilePath + "\\" + modelSubPath.str();
    modelName =  modelFileName.str();
}
}

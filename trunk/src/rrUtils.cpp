#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <io.h>
#include <dir.h>
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
    modelSubPath<<setfill('0')<<setw(5)<<caseNr;        //create the "00023" subfolder format
    modelFileName<<setfill('0')<<setw(5)<<caseNr<<postFixPart;
    modelFilePath = modelFilePath + "\\" + modelSubPath.str();
    modelName =  modelFileName.str();
}

string GetTestSuiteSubFolderName(int caseNr)
{
    stringstream modelSubPath;
    modelSubPath<<setfill('0')<<setw(5)<<caseNr;        //create the "00023" subfolder format
    return modelSubPath.str();
}

bool CreateFolder(const string& folder)
{
    if(FileExists(folder))
    {
        return true;
    }

    int res = mkdir(folder.c_str());
    return (res==0) ? true : false;
}


bool CopyStdVectorToCArray(vector<double>& src, double* dest,  int size)
{
    if(!dest)
    {
        Log(lError)<<"Tried to copy to NULL vector";
        return false;
    }

    for(int i = 0; i < size; i++)
    {
        dest[i] = src[i];
    }
    return true;

}

bool CopyStdVectorToCArray(vector<bool>&   src,  bool*  dest,  int size)
{
    if(!dest)
    {
        Log(lError)<<"Tried to copy to NULL vector";
        return false;
    }

    for(int i = 0; i < size; i++)
    {
        dest[i] = src[i];
    }
    return true;

}

bool CopyCArrayToStdVector(int* src, vector<int>& dest, int size)
{
    if(!src)
    {
        Log(lError)<<"Tried to copy from NULL vector";
        return false;
    }

    dest.resize(size);
    for(int i = 0; i < size; i++)
    {
        dest[i] = src[i];
    }
    return true;
}

bool CopyCArrayToStdVector(double* src, vector<double>& dest, int size)
{
    if(!src)
    {
        Log(lError)<<"Tried to copy from NULL vector";
        return false;
    }

    dest.resize(size);
    for(int i = 0; i < size; i++)
    {
        dest[i] = src[i];
    }
    return true;
}

bool CopyCArrayToStdVector(bool* src, vector<bool>& dest, int size)
{
    if(!src)
    {
        Log(lError)<<"Tried to copy from NULL vector";
        return false;
    }

    dest.resize(size);
    for(int i = 0; i < size; i++)
    {
        dest[i] = src[i];
    }
    return true;
}

double*    CreateCVectorFromStdVector(const vector<double>& vec)
{
    double* avec = new double[vec.size()];
    if(!avec)
    {
        Log(lError)<<"Failed to allocate c vector";
        return NULL;
    }

    for(int i = 0; i < vec.size(); i++)
    {
          avec[i] = vec[i];
    }
    return avec;
}


}//end of namespace
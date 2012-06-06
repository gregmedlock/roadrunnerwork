#ifndef rrUtilsH
#define rrUtilsH
//---------------------------------------------------------------------------
#include <windows.h>
#include <float.h>    //ms compatible IEEE functions, e.g. _isnan
#include <vector>
#include <string>
#include "rrExporter.h"

using std::vector;
using std::string;
namespace rr
{

RR_DECLSPEC std::size_t     IndexOf(std::vector<std::string>& vec, const std::string& elem );
RR_DECLSPEC bool            IsNaN(const double& aNum);
RR_DECLSPEC bool            IsNullOrEmpty(const string& str);    //Can't be null, but empty
RR_DECLSPEC void            Pause(bool doIt = true);
RR_DECLSPEC bool            FileExists(const string& fileN);
RR_DECLSPEC bool            FolderExists(const string& folderN);
RR_DECLSPEC bool            CreateFolder(const string& path);
RR_DECLSPEC string          GetUsersTempDataFolder();
RR_DECLSPEC vector<string>  GetLinesInFile(const string& fName);
RR_DECLSPEC void            CreateTestSuiteFileNameParts(int caseNr, const string& postFixPart, string& FilePath, string& modelFileName);
RR_DECLSPEC string          GetTestSuiteSubFolderName(int caseNr);
RR_DECLSPEC bool            CopyCArrayToStdVector(int* src, vector<int>& dest, int size);
RR_DECLSPEC bool            CopyCArrayToStdVector(double* src, vector<double>& dest, int size);
RR_DECLSPEC bool            CopyCArrayToStdVector(bool* src, vector<bool>& dest, int size);
RR_DECLSPEC bool            CopyStdVectorToCArray(vector<double>& src, double* dest,  int size);
RR_DECLSPEC bool            CopyStdVectorToCArray(vector<bool>&   src,  bool*  dest,  int size);
RR_DECLSPEC double*         CreateCVectorFromStdVector(const vector<double>& vec);
RR_DECLSPEC HINSTANCE       LoadDLL(const string& dll);

}

#endif

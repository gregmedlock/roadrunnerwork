#ifndef rrUtilsH
#define rrUtilsH
//---------------------------------------------------------------------------
#include <float.h>    //ms compatible IEEE functions, e.g. _isnan
#include <vector>
#include <string>
#include "rrExporter.h"

using std::vector;
using std::string;
namespace rr
{

std::size_t     RR_DECLSPEC IndexOf(std::vector<std::string>& vec, const std::string& elem );
bool            RR_DECLSPEC IsNaN(const double& aNum);
bool            RR_DECLSPEC IsNullOrEmpty(const string& str);    //Can't be null, but empty
void            RR_DECLSPEC Pause(bool doIt = true);

bool            RR_DECLSPEC FileExists(const string& fileN);
bool            RR_DECLSPEC FolderExists(const string& folderN);
bool            RR_DECLSPEC CreateFolder(const string& path);
string          RR_DECLSPEC GetUsersTempDataFolder();
vector<string>  RR_DECLSPEC GetLinesInFile(const string& fName);
void            RR_DECLSPEC CreateTestSuiteFileNameParts(int caseNr, const string& postFixPart, string& FilePath, string& modelFileName);
string          RR_DECLSPEC GetTestSuiteSubFolderName(int caseNr);

bool            RR_DECLSPEC CopyCArrayToStdVector(int* src, vector<int>& dest, int size);
bool            RR_DECLSPEC CopyCArrayToStdVector(double* src, vector<double>& dest, int size);
bool            RR_DECLSPEC CopyCArrayToStdVector(bool* src, vector<bool>& dest, int size);

bool            RR_DECLSPEC CopyStdVectorToCArray(vector<double>& src, double* dest,  int size);
bool            RR_DECLSPEC CopyStdVectorToCArray(vector<bool>&   src,  bool*  dest,  int size);

double*         RR_DECLSPEC CreateCVectorFromStdVector(const vector<double>& vec);

}

#endif

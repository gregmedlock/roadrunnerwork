#ifndef rrUtilsH
#define rrUtilsH
//---------------------------------------------------------------------------
#include <float.h>	//ms compatible IEEE functions, e.g. _isnan
#include <vector>
#include <string>
#include "rrExporter.h"

using std::vector;
using std::string;
namespace rr
{

std::size_t     RR_DECLSPEC IndexOf(std::vector<std::string>& vec, const std::string& elem );
bool 		    RR_DECLSPEC IsNaN(const double& aNum);
bool 		    RR_DECLSPEC IsNullOrEmpty(const string& str);	//Can't be null, but empty
void 		    RR_DECLSPEC Pause(bool doIt = true);
bool            RR_DECLSPEC	FileExists(const string& fileN);
vector<string> 	RR_DECLSPEC	GetLinesInFile(const string& fName);
void 			RR_DECLSPEC CreateTestSuiteFileNameParts(int caseNr, const string& postFixPart, string& FilePath, string& modelFileName);
}

#endif

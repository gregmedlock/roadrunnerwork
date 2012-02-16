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

std::size_t RR_DECLSPEC IndexOf(std::vector<std::string>& vec, const std::string& elem );

bool RR_DECLSPEC IsNaN(const double& aNum);
bool RR_DECLSPEC IsNullOrEmpty(const string& str);	//Can't be null, but empty

}

#endif

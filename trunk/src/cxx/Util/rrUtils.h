#ifndef rrUtilsH
#define rrUtilsH
//---------------------------------------------------------------------------
#include <float.h>	//ms compatible IEEE functions, e.g. _isnan
#include <vector>
#include <string>

namespace rr
{

std::size_t IndexOf(std::vector<std::string>& vec, const std::string& elem );

bool IsNaN(const double& aNum)
{
	return _isnan(aNum);
}


}

#endif

#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include "rrUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{

std::size_t IndexOf(std::vector<std::string>& vec, const std::string& elem )
{
	int index = distance(vec.begin(), find(vec.begin(), vec.end(), elem));
	return index;
}

bool IsNaN(const double& aNum)
{
	return _isnan(aNum);
}

bool IsNullOrEmpty(const string& str)
{
	return !str.size();
}

}

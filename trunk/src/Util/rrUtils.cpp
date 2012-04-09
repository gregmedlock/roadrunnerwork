#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <conio.h>
#include "rrUtils.h"
//---------------------------------------------------------------------------
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

}

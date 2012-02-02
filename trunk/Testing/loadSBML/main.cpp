//---------------------------------------------------------------------------

#pragma hdrstop
#include <iostream>
#include <fstream>
#include <string>
#include "rrRoadRunner.h"
#include <tchar.h>
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	RoadRunner *rr = new RoadRunner;
    ifstream ifs("feedback.xml");

    if(!ifs)
    {
    	cout<<"Failed opening file";
    }
    std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

    rr->loadSBML(sbml);
    delete rr;
	return 0;
}
//---------------------------------------------------------------------------

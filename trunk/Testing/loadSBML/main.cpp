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
	string fName = "feedback.xml";
	RoadRunner *rr = new RoadRunner;

    rr->loadSBMLFromFile(fName);


    delete rr;
	return 0;
}


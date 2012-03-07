//---------------------------------------------------------------------------

#pragma hdrstop
#include <tchar.h>
#include <iostream>
//---------------------------------------------------------------------------
extern "C"
{
#include "../../rr_code_output/c_from_rr++/00001-sbml-l2v4.h"
}

#pragma argsused
using namespace std;

int _tmain(int argc, _TCHAR* argv[])
{
	cout<<"Hello ....\n";

    gTheModel._gp[0] = 2;
	InitModel();
    cout<<"Global parameter is now: "<<gTheModel._gp[0]<<"\n";
	return 0;
}


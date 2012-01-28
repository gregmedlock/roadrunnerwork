//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include <tchar.h>
//---------------------------------------------------------------------------
#include "rrException.h"
#pragma argsused
using namespace rr;
using namespace std;

int main ()
{
  	try
  	{
  		throw SBWApplicationException("Testing an exception");
  	}
  	catch (exception& e)
  	{
    	cout << e.what() << endl;
  	}

  return 0;
}



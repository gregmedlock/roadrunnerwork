#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <math.h>
#include "rrNOMWrapper.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


namespace rr
{
NOMWrapper::NOMWrapper()
{

}

NOMWrapper::~NOMWrapper()
{
}

string NOMWrapper::getNthCompartmentId(const int& i)
{
    //NOM
	//DLL_EXPORT int getNthCompartmentId (int nIndex, char **Id)


	char *ID[1];
    if( ::getNthCompartmentId(i, ID) )
    {
		return "";
    }

	return string(ID[0]);
}

double NOMWrapper::getValue(const string& id)
{
	double val;
	if(::getValue(id.c_str(), &val))
    {
    	//How to signal error..?
    	return -1;
    }
	return val;
}
}//namespace rr

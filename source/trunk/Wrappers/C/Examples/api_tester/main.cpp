//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "rr_c_api.h"
//---------------------------------------------------------------------------
#if defined(_MSC_VER)
  #include <direct.h>
  #define getcwd _getcwd
  #define chdir  _chrdir
#elif defined(__BORLANDC__)
  	#include <dir.h>
#else
#include <unistd.h>
#endif

using namespace std;
int main(int argc, char* argv[])
{
	string modelsPath(".\\..\\Models");
	if(argc > 1)
	{
		modelsPath = argv[1];
	}

	char* buffer = new char[MAXPATH];
	// Get the current working directory:
	if( (buffer = _getcwd( buffer, MAXPATH )) == NULL )
	{
		perror( "getcwd error" );
	}
	else
	{
		printf( "%s \nLength: %d\n", buffer, strlen(buffer) );
		delete [] buffer;
	}

	RRHandle rrHandle = NULL;
    rrHandle =  getRRInstance();

    if(!rrHandle)
    {
        cout<<"No handle...";
    }

	char* text = getBuildDate();

	if(text)
	{
		cout<<"Build date: "<<text<<endl;
		freeText(text);
	}

    setTempFolder("c:\\rrTemp");
	string fileName = modelsPath + "\\ss_TurnOnConservationAnalysis.xml";
	ifstream ifs(fileName.c_str());
	if(!ifs)
	{
		stringstream msg;
		msg<<"Failed opening file: "<<fileName;
		cerr<<msg.str();
		return false;
	}

	setComputeAndAssignConservationLaws(true);
	std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

	if(!loadSBML(sbml.c_str()))
	{
		cerr<<"Failed loading SBML from file:"<<fileName<<endl;
		cerr<<"Last error was"<<getLastError()<<endl;
		return -1;
	}

	double ssVal;
    bool success = steadyState(ssVal);
    if(!success)
    {
		cerr<<"Steady State call failed. Error was: "<<getLastError()<<endl;
    }
    else
    {
	    cout<<"Steady state number: "<<ssVal<<endl;
    }

    RRStringListHandle list = getRatesOfChangeNames();

//    cout<<getBoundarySpeciesByIndex (0)<<endl;
	double value;
    getFloatingSpeciesByIndex (0, value);
    cout<<value<<endl;
    getGlobalParameterByIndex (0, value);

    cout<<value<<endl;
    getGlobalParameterByIndex (2, value);
    cout<<value<<endl;

    cout<<getNumberOfDependentSpecies()<<endl;
    cout<<getNumberOfIndependentSpecies()<<endl;

    RRVector* vec = getRatesOfChange();
    cout<<getParamPromotedSBML(sbml.c_str());
	evalModel();

	///////////////////
    text = getCopyright();
    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
    }

    cout<<text<<endl;
    freeText(text);
    freeRRInstance(rrHandle);
    return 0;
}


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

	char* buffer;
	// Get the current working directory:
	if( (buffer = _getcwd( NULL, 0 )) == NULL )
	{
		perror( "getcwd error" );
	}
	else
	{
		printf( "%s \nLength: %d\n", buffer, strlen(buffer) );
		free(buffer);
	}
	
	RRHandle rrHandle = NULL;
    rrHandle =  getRRInstance();

    if(!rrHandle)
    {
        cout<<"No handle...";
    }

	char* text;
	text = getBuildDate();

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


	RRSymbolLists* symbols = getAvailableSymbols();
	if(symbols)
	{
        for(int i = 0; i < symbols->NumberOfLists; i++)
        {

            cout<<"========= ";
            if( symbols->List[i].Label != NULL && strlen(symbols->List[i].Label) > 0)
            {
                cout<<symbols->List[i].Label;
            }
            else
            {
                cout<<"no name";
            }
            cout<<"  ==============="<<endl;

            for(int j = 0; j < symbols->List[i].Count; j++)
            {
                cout<<symbols->List[i].String[j]<<endl;
            }
        }
    }

	double ssVal = steadyState();
    if(ssVal == -1)
    {
		cerr<<"Steady State call failed. Error was: "<<getLastError()<<endl;
    }
    else
    {
	    cout<<"This is steady state number: "<<ssVal<<endl;
    }

	RRDoubleVectorHandle concs = getFloatingSpeciesInitialConcentrations();
    printVector(concs);
    freeRRDoubleVector(concs);

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


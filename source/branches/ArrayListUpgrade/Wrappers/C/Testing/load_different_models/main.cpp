//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "rr_c_api.h"
#include "rrUtils.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

int main()
{
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

    setTempFolder("C:\\rrTemp");
	string fileName = "..\\Models\\feedback.xml";
	string sbml = GetFileContent(fileName.c_str());

    //To get the CCode, the CCode needs to be generated
    if(!loadSBML(sbml.c_str()))
    {
    	cerr<<"Failed loading SBML.\n";
        cerr<<"Last error: "<<getLastError()<<endl;
    }

    RRResult* result1 = simulate();

    string str = getResultAsString(result1);
    cout<<str;

  	fileName = "..\\Models\\test_1.xml";
	sbml = GetFileContent(fileName.c_str());

    //To get the CCode, the CCode needs to be generated
    if(!loadSBML(sbml.c_str()))
    {
    	cerr<<"Failed loading SBML.\n";
        cerr<<"Last error: "<<getLastError()<<endl;
    }

    RRResult* result2 = simulate();

    cout<<getResultAsString(result2);


	///// Cleanup


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


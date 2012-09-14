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
#include "rrException.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

int main()
{
    RRHandle rrHandle = NULL;

    try
    {
    rrHandle =  getRRInstance();

    if(!rrHandle)
    {
        cout<<"No handle...";
    }

    setTempFolder("R:\\rrTemp");
	setLogLevelFromString("Debug1");
    enableLogging();

	char* text;
	text = getBuildDate();

	if(text)
	{
		cout<<"Build date: "<<text<<endl;
		freeText(text);
	}

    string fileName;
	fileName = "..\\Models\\ss_feedback.xml";

	string sbml = GetFileContent(fileName.c_str());

    //To get the CCode, the CCode needs to be generated
    if(!loadSBML(sbml.c_str()))
    {
    	cerr<<"Failed loading SBML.\n";
        cerr<<"Last error: "<<getLastError()<<endl;
    }

    setSelectionList("time,S1 Time,S2");
    RRResult* result1 = simulate();
    string str = printResult(result1);
    cout<<str;

  	fileName = "..\\Models\\squareWaveModel.xml";
	sbml = GetFileContent(fileName.c_str());

    //To get the CCode, the CCode needs to be generated
    if(!loadSBML(sbml.c_str()))
    {
    	cerr<<"Failed loading SBML.\n";
        cerr<<"Last error: "<<getLastError()<<endl;
    }

    RRResult* result2 = simulate();
    if(result2)
    {
    	cout<<printResult(result2);
    }
    else
    {
    	cout<<"There was a problem";
    }

	///// Cleanup
    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
    }

    text = getCopyright();
    cout<<text<<endl;

    freeText(text);
    freeRRInstance(rrHandle);

    }
   	catch(rr::Exception& ex)
	{
        cerr<<"RoadRunner exception occurred: "<<ex.what()<<endl;
    }

    return 0;
}


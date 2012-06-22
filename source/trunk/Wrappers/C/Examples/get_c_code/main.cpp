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

	string fileName = "..\\Models\\test_1.xml";
	string sbml = GetFileContent(fileName.c_str());

    //To get the CCode, the CCode needs to be generated
    if(!loadSBML(sbml.c_str()))
    {
    	cerr<<"Failed loading SBML.\n";
        cerr<<"Last error: "<<getLastError()<<endl;
    }

	RRCCode* code = getCCode();
    if(!code)
    {
	  	cerr<<"Failed to get CCode from RoadRunner";
    }

    cout<<"START OF CODE ==========\n";
	if(code->Header)
	{
		cout<<"C Header =========== \n"<<code->Header<<"\n";
	}
	else
	{
		cout<<"C Header =========== \n"<<" is NULL"<<"\n";
	}

	if(code->Source)
	{
		cout<<"C Source =========== \n"<<code->Source<<"\n";
	}
	else
	{
		cout<<"C Source =========== \n"<<" is NULL"<<"\n";
	}
	
    cout<<"END OF CODE ==========\n";
	///// Cleanup
    freeCCode(code);

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


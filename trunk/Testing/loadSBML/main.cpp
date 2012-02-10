#pragma hdrstop
#include <iostream>
#include <fstream>
#include <string>
#include <conio.h>
#include <tchar.h>

#include "rrException.h"
#include "rrRoadRunner.h"

//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{

	string modelsPath("C:\\RRW\\Testing\\models");
    string model(modelsPath + "\\feedback.xml");
    ifstream ifs(model.c_str());
    if(!ifs)
    {
    	cout<<"Failed opening file";
    }

    std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

    try
    {
		RoadRunner *rr = new RoadRunner;
    	rr->loadSBML(sbml);


        cout<<"Copyright: "<<rr->getCopyright()<<endl;

//        list<string> compartments  = rr->getCompartmentNames();
//
//        list<string>::iterator iter;
//        for(iter = compartments.begin(); iter != compartments.end(); iter++)
//        {
//            cout<<"Compartment: "<<*(iter)<<endl;
//        }
//        delete rr;

    }
    catch(const RRException& ex)
    {
		cout<<"RoadRunner exception occured: "<<ex.what()<<endl;
    }

  	//-------------------------------------
	cout<<"Hit any key to exit...";
	cin.ignore(0,'\n');
    getch();

	return 0;
}


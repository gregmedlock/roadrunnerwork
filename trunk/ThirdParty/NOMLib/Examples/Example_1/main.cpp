#include <conio.h>
#include <iostream>
#include <fstream>
#include "NOMLib.h"

using namespace std;
int main()
{
	cout<<"Testing some functions in the NOM library\n";

	string modelsPath("C:\\RRW\\Testing\\models");
    string model(modelsPath + "\\feedback.xml");
    ifstream ifs(model.c_str());
    if(!ifs)
    {
    	cout<<"Failed opening file";
    }

    std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

	if(loadSBML(sbml.c_str()))
    {
		cout<<"The loadSBML function failed!"<<endl;
    }

	cout<<"GetModelName"<<endl;
    //if loadSBML was succesful, a model is allocated in the loadSBML call, that can
    //be queried, as below.
    string modelName = GetModelName();
	if(modelName.size())
    {
		cout<<"Model name is: "<<modelName<<endl;
    }

    cout<<"Validating sbml:\n";
	if(!validateSBML((char*) sbml.c_str()))
    {
		cout<<"sbml model \""<<modelName<<"\" was validated succesfully!"<<endl;
    }

	cout<<"Last error was: "<<getError()<<endl;

	//-------------------------------------
	cout<<"Hit any key to exit...";
	cin.ignore(0,'\n');
    getch();
    return 0;
}

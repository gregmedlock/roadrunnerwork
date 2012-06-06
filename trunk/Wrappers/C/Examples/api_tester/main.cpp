//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include <string>
#include "rr_c_api.h"
//---------------------------------------------------------------------------

using namespace std;

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

    text = getCopyright();
    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
    }


    string xmlFileName = "..\\Models\\feedback.xml";
    if(!loadSBMLFromFile(xmlFileName.c_str()))
    {
        cout<<"Failed loading SBML from file:"<<xmlFileName;
    }

    RRDataMatrixHandle matrix = getStoichiometryMatrix();
    printMatrix(matrix);
    freeRRDataMatrix(matrix);

    cout<<text;
    freeText(text);

    reset();

    freeRRInstance(rrHandle);
    return 0;
}


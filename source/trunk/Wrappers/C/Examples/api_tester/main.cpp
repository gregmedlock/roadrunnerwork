//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include <string>
#include <string.h>
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

    RRStringList* names = getReactionNames();

    if(names)
    {
        for(int i = 0; i < names->Count; i++)
        {
            cout<<names->String[i]<<endl;
        }
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

    cout<<text;
    freeText(text);

    reset();

    freeRRInstance(rrHandle);

    return 0;
}


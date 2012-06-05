//---------------------------------------------------------------------------
#pragma hdrstop
#include <iostream>
#include "rr_c_api.h"
//---------------------------------------------------------------------------

using namespace std;

int main()
{
    RRHandle rrHandle;

//    rrHandle =  getRRInstance();

    if(!rrHandle)
    {
        cout<<"No handle...";
    }

    char* copyright = getCopyright();

    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
        freeText(error);
    }

    cout<<copyright;

    freeText(copyright);


    return 0;
}


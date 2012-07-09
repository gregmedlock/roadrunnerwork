//---------------------------------------------------------------------------

#pragma hdrstop
#include <iostream>
#include "rrArrayList.h"
//---------------------------------------------------------------------------
#pragma argsused

using namespace rr;
using namespace std;
int main()
{
    ArrayList<string> list1;
    ArrayList<string> list2;
    ArrayList<string> list3;

    list1.Add("item1");
    list1.Add("item2");

    list2.Add("item1a");
    list2.Add("item1b");

    list2.Add(list3);
    list1.Add(list2);
    list1.Add("3");
    list1.Add("test");

    cout<<endl<<list1;
    cout<<endl<<"=========="<<endl;
    for(int i = 0; i < list1.Count(); i++)
    {
        string item = (list1)[i];
        cout<<"Item "<<i<<": "<<item<<endl;
    }


    return 0;
}


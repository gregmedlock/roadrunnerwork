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
    ArrayList<int> list1;
    ArrayList<int> list2;

    list1.Add(1);
    list1.Add(2);

    list2.Add(10);
    list2.Add(11);

    list1.Add(list2);
    list1.Add(3);
    cout<<list1;
//    list1.Add("item1");
//    list2.Add("1a");
//    list2.Add("1b");
//    list2.Add("1c");
//    list1.Add(list2);

//    list2.Clear();
//
//    list1.Add("item2");
//    list2.Add("2a");
//    list2.Add("2b");
//    list2.Add("2c");
//    list1.Add(list2);
//
//    list3.Add(list1);
    return 0;
}


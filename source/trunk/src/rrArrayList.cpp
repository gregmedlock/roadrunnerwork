#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrArrayList.h"
//---------------------------------------------------------------------------



namespace rr
{

ArrayList::ArrayList()
{
}

ArrayList::ArrayList(const ArrayList& cp)
:
mContainer(cp.mContainer)
{}

ArrayList::ArrayList(const string& lbl, const ArrayList& cp)
:
mLabel(lbl),
mContainer(cp.mContainer)
{}

void ArrayList::Clear()
{
    mContainer.empty();
}

ArrayList::ArrayList(const StringList& cp)
{
    Add(cp.mLabel, cp);
}

//void ArrayList::operator=(const ArrayList& rhs)
//{
//
//}

int ArrayList::TotalCount() const
{
    //Returns the total count of all list items..
    int cnt = 0;
    for(int i = 0; i < Count(); i++)
    {
        cnt += mContainer[i].Count();
    }
    return cnt;
}

int ArrayList::ListCount() const
{
    return mContainer.size();
}

int ArrayList::Count() const
{
    return mContainer.size();
}

StringList& ArrayList::operator[](const int& index)
{
    return mContainer[index];
}

const StringList& ArrayList::operator[](const int& index) const
{
    return mContainer[index];
}

vector<StringList>::iterator ArrayList::begin()
{
    return mContainer.begin();
}

vector<StringList>::iterator ArrayList::end()
{
    return mContainer.end();
}

void ArrayList::Add(const string& lbl, const ArrayList& lists)
{
    mLabel = lbl;
    Add(lists);
}

void ArrayList::Add(const ArrayList& lists)
{
    for(int i = 0; i < lists.Count(); i++)
    {
        StringList aList;
        aList = lists.mContainer[i];    //Todo: lists[i] should work...
        Add(aList);
    }
}

void ArrayList::Add(const StringList& list)
{
    mContainer.push_back(list);
}

void ArrayList::Add(const string& listName, const StringList& aList)
{
    StringList list(aList);
    list.Label(listName);
    mContainer.push_back(list);
}

void ArrayList::Add(const string& item)
{
    StringList list;
    list.push_back(item);
    Add(list);

}

void ArrayList::Add(const int& atPos)
{

}

ostream& operator<<(ostream& stream, const ArrayList& list)
{
    vector<StringList>::iterator iter;
    for(int  i = 0; i < list.Count(); i++)
    {
        string item = list[i].AsString();
        stream<<"List Item "<<i+1<<" : "<<item<<endl;
    }
    return stream;
}

}


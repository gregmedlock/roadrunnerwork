#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#include "rrStringListContainer.h"
//---------------------------------------------------------------------------



namespace rr
{

StringListContainer::StringListContainer()
{
}

StringListContainer::StringListContainer(const StringListContainer& cp)
:
mContainer(cp.mContainer)
{}

StringListContainer::StringListContainer(const string& lbl, const StringListContainer& cp)
:
mLabel(lbl),
mContainer(cp.mContainer)
{}

StringListContainer::StringListContainer(const StringList& cp)
{
    Add(cp.mLabel, cp);
}

//void StringListContainer::operator=(const StringListContainer& rhs)
//{
//
//}

int StringListContainer::size() const
{
    return mContainer.size();
}

int StringListContainer::Count() const
{
    return mContainer.size();
}

StringList& StringListContainer::operator[](const int& index)
{
    return mContainer[index];
}

vector<StringList>::iterator StringListContainer::begin()
{
    return mContainer.begin();
}

vector<StringList>::iterator StringListContainer::end()
{
    return mContainer.end();
}

void StringListContainer::Add(const string& lbl, const StringListContainer& lists)
{
    mLabel = lbl;
    Add(lists);
}

void StringListContainer::Add(const StringListContainer& lists)
{
    for(int i = 0; i < lists.size(); i++)
    {
        StringList aList;
        aList = lists.mContainer[i];    //Todo: lists[i] should work...
        Add(aList);
    }
}

void StringListContainer::Add(const StringList& list)
{
    mContainer.push_back(list);
}

void StringListContainer::Add(const string& listName, const StringList& aList)
{
    StringList list(aList);
    list.Label(listName);
    mContainer.push_back(list);
}

void StringListContainer::Add(const string& item)
{
    StringList list;
    list.push_back(item);
    Add(list);

}

void StringListContainer::Add(const int& atPos)
{

}

//ostream& operator<<(ostream& stream, StringListContainer& list)
//{
//    vector<string>::iterator iter;
//    int count = 0;
//    for(iter = list.begin(); iter != list.end(); iter++)
//    {
//        stream<<"List Item "<<++count<<" : "<<(*iter)<<std::endl;
//    }
//    return stream;
//}

ostringstream& operator<<(ostringstream& stream, StringListContainer& list)
{
    vector<StringList>::iterator iter;
    int count = 0;
    for(iter = list.begin(); iter != list.end(); iter++)
    {
        stream<<"Label: " <<(*iter).mLabel<<std::endl;
    }
    return stream;
}


}


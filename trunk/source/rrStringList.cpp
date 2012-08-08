#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <iostream>
#include <sstream>
#include "rrStringUtils.h"
#include "rrStringList.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------

namespace rr
{

StringList::StringList()
{}

StringList::StringList(const vector<string>& strings)
:
mStrings(strings)
{}

StringList::~StringList()
{}

void StringList::Label(const string& lbl)
{
    mLabel = lbl;
}

StringList::StringList(const string& str, const string& delimiter)
{
    mStrings = SplitString(str, delimiter);
}

StringList::StringList(const StringList& cp)
{
    mLabel = cp.mLabel;
    mStrings = cp.mStrings;
}

vector<string>::iterator StringList::begin()
{
    return mStrings.begin();
}

vector<string>::iterator StringList::end()
{
    return mStrings.end();
}

string& StringList::operator[](const int& index)
{
    return mStrings[index];
}

string  StringList::operator[](const int& index) const
{
    return mStrings[index];
}

unsigned int StringList::Count() const
{
    return mStrings.size();
}

string StringList::AsString(const string& delimiter) const
{
    stringstream names;
    for(int i = 0; i < mStrings.size(); i++)
    {
        names<<mStrings[i];
        if( i < mStrings.size() - 1)
        {
            names<<delimiter;
        }
    }
    return names.str();
}

void StringList::PreFix(const string& fix)
{
     for(mLI = mStrings.begin(); mLI != mStrings.end(); mLI++)
    {
        (*mLI) = fix + (*mLI) ;
    }
}

void StringList::PostFix(const string& fix)
{
    for(mLI = mStrings.begin(); mLI != mStrings.end(); mLI++)
    {
        (*mLI) = (*mLI) + fix;
    }
}

StringList StringList::operator-(const StringList& rhs)
{
    StringList newList;

    for(int i = 0; i < Count(); i++)
    {
        string item = mStrings[i] + "-" + rhs[i];
        newList.Add(item);
    }

    return newList;
}

void StringList::InsertAt(const int& index, const string& item)
{
    mLI = mStrings.begin() + index;
    if(mLI != mStrings.end())
    {
        mStrings.insert(mLI, item);
    }
}

void StringList::Add(const string& str)
{
    mStrings.push_back(str);
}

void StringList::push_back(const string& item)
{
    Add(item);
}

int StringList::find(const string& item)
{
    return rr::IndexOf(mStrings, item);
}

int StringList::IndexOf(const string& item)
{
    return rr::IndexOf(mStrings, item);
}

bool StringList::Contains(const string& item)
{
    return std::find(mStrings.begin(), mStrings.end(), item) != mStrings.end() ? true : false;
}

void StringList::operator=(const StringList& rhs)
{
    mStrings = rhs.mStrings;
}

ostream& operator<<(ostream& stream, const StringList& list)
{
    for(int i = 0; i < list.Count(); i++)
    {
        stream<<list[i]<<std::endl;
    }
    return stream;
}

} //namespace rr


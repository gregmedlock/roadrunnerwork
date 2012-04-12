#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <iostream>
#include <sstream>
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

StringList::StringList(const StringList& cp)
{
	mStrings = cp.mStrings;
}

string StringList::AsString(const string& delimiter)
{
	stringstream names;
	for(int i = 0; i < mStrings.size(); i++)
	{
		names<<mStrings[i];
		if( i < mStrings.size() + 1)
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

    for(int i = 0; i < size(); i++)
    {
    	string item = mStrings[i] + "-" + rhs[i];
        newList.Add(item);
    }

	return newList;
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
	return IndexOf(mStrings, item);
}

bool StringList::Contains(const string& item)
{
	return std::find(mStrings.begin(), mStrings.end(), item) != mStrings.end() ? true : false;
}

void StringList::operator=(const StringList& rhs)
{
	mStrings = rhs.mStrings;
}

ostream& operator<<(ostream& stream, StringList& list)
{
	vector<string>::iterator iter;
    int count = 0;
    for(iter = list.begin(); iter != list.end(); iter++)
    {
		stream<<"List Item "<<++count<<" : "<<(*iter)<<std::endl;
    }
	return stream;
}
} //namespace rr


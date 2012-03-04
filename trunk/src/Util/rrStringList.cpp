#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <iostream>
#include "rrStringList.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
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


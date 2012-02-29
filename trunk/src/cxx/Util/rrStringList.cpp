#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include "rrStringList.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

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

} //namespace rr


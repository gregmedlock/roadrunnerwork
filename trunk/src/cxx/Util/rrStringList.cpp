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
{

}

StringList::StringList(const vector<string>& strings)
:
mStrings(strings)
{

}

StringList::~StringList()
{

}

StringList::StringList(const StringList& cp)
{
	mStrings = cp.mStrings;
}


void StringList::Add(const string& str)
{
	mStrings.push_back(str);
}

int StringList::find(const string& item)
{
	return IndexOf(mStrings, item);
}

} //namespace rr


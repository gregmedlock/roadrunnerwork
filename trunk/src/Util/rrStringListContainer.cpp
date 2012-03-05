#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop

#include "rrStringListContainer.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{

void StringListContainer::Add(const StringList& list)
{
	mContainer.push_back(list);
}

void StringListContainer::Add(const string& item)
{
	StringList list;
    list.push_back(item);
	Add(list);

}

void StringListContainer::Add(const int& coll)
{

}

}

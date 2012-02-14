#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop

#include "rrStringListContainer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

void StringListContainer::Add(const StringList& list)
{
	mContainer.push_back(list);
}

void StringListContainer::Add(const string& coll)
{

}

void StringListContainer::Add(const int& coll)
{

}

}


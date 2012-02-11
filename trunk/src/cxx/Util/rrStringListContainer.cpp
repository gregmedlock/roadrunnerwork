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
	mCollection.push_back(list);
}


}


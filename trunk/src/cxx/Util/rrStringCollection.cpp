#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop

#include "rrStringCollection.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

void StringCollection::Add(const string& str)
{
	mStrings.push_back(str);

}

void StringCollections::Add(const StringCollection& coll)
{
	mCollection.push_back(coll);
}


}


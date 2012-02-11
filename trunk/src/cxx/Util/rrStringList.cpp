#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop

#include "rrStringList.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

void StringList::Add(const string& str)
{
	mStrings.push_back(str);

}
} //namespace rr


#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrHashTable.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


namespace rr
{

bool StringSymbolHashTable::ContainsKey(const string& aKey)
{
	return (this->find( aKey ) != this->end()) ? true : false;
}

}

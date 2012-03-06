#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrHashTable.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------



namespace rr
{


bool StringSymbolHashTable::ContainsKey(const string& aKey)
{
	return (this->find( aKey ) != this->end()) ? true : false;
}

ostream& operator<<(ostream& stream, StringSymbolHashTable& hash)
{
	map<string, SBMLSymbol>::iterator iter;

	for(iter = hash.begin(); iter != hash.end(); iter++)
    {
    	stream<<"Key: "<<(*iter).first<<"\tValue:"<<(*iter).second<<"\n";
    }
    return stream;
}
}

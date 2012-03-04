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

ostream& operator<<(ostream& stream, StringSymbolHashTable& hash)
{
	hash_map<string, SBMLSymbol>::iterator iter;

	for(iter = hash.begin(); iter != hash.end(); iter++)
    {
    	stream<<"Key: "<<(*iter).first<<"\tValue:"<<(*iter).second<<"\n";
    }
    return stream;
}
}

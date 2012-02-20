#ifndef rrHashTableH
#define rrHashTableH
#include <hash_map>
#include "rrObject.h"
#include "rrSBMLSymbol.h"
//---------------------------------------------------------------------------

using std::hash_map;

namespace rr
{

class RR_DECLSPEC HashTable : public rrObject, public hash_map<string, SBMLSymbol>
{
	protected:
//		hash_map<string, SBMLSymbol> mHash;

    public:
		HashTable(){}

};

typedef HashTable Hashtable;
} //namespace rr
#endif

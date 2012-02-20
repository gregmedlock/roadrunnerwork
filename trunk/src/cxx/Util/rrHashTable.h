#ifndef rrHashTableH
#define rrHashTableH
#include <hash_map>
#include "rrObject.h"
#include "rrSBMLSymbol.h"
//---------------------------------------------------------------------------

using std::hash_map;

namespace rr
{

class RR_DECLSPEC StringSymbolHashTable : public rrObject, public hash_map<string, SBMLSymbol>
{
	protected:

    public:
		StringSymbolHashTable(){}

};


class RR_DECLSPEC IntStringHashTable : public rrObject, public hash_map<int, string>
{
	protected:

    public:
		IntStringHashTable(){}

};


} //namespace rr
#endif

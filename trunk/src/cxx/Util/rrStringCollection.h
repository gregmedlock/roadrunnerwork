#ifndef rrStringCollectionH
#define rrStringCollectionH
#include <vector>
#include <string>
#include "rrObject.h"

using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC StringCollection : public rrObject
{
	protected:
		vector<string> mStrings;

	public:
    	void Add(const string& str);
        int  size(){return mStrings.size();}
};

class RR_DECLSPEC StringCollections : public rrObject
{
	protected:
		vector<StringCollection> mCollection;

	public:
		void Add(const StringCollection& coll);
        int  size(){return mCollection.size();}
        StringCollection& operator[](const int& index){return mCollection[index];}
};

}
#endif

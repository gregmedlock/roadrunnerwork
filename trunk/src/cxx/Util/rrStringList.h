#ifndef rrStringListH
#define rrStringListH
#include <vector>
#include <string>
#include "rrObject.h"

using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC StringList : public rrObject
{
	protected:
		vector<string> 				mStrings;
        vector<string>::iterator    mListIterator;

	public:
    	void 				        Add(const string& str);
        int  				        size(){return mStrings.size();}
	    string&  			        operator[](const int& index){return mStrings[index];}
        int					        find(const string& item);
};

}
#endif

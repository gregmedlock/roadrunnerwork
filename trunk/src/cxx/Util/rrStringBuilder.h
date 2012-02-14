#ifndef rrStringBuilderH
#define rrStringBuilderH
#include <sstream>
#include <string>
#include "rrObject.h"

using std::stringstream;
using std::string;

namespace rr
{

class RR_DECLSPEC StringBuilder : public rrObject
{
	protected:
    	stringstream mStringing;

	public:
    	StringBuilder(const string& aStr = "")
        {
        	mStringing<<aStr;
        }

        void  Append(const string& str)
        {
        	mStringing<<str;
        }

        void AppendFormat(){}
        string ToString(){return mStringing.str();}
};

}

#endif

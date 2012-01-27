#ifndef rrStringBuilderH
#define rrStringBuilderH
//---------------------------------------------------------------------------
#include <sstream>
#include <string>
#include "rrExporter.h"
using std::stringstream;
using std::string;
namespace rr
{

class RR_DECLSPEC StringBuilder
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
};

}

#endif

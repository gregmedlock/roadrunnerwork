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

    	StringBuilder(const string& aStr = "");
        void  Append(const string& str);
        void  Append(const unsigned int& str);
        void  Append(const double& str);
        void  Append(const string& s1, const string& s2);
        void  Append(const string& s1, const string& s2, const string& s3);
        void  Append(const string& s1, const unsigned int& s2, const string& s3);
        void  Append(const string& s1, const unsigned int& s2, const string& s3, const string& s4);

		stringstream& operator<<(const string& str);

        void AppendFormat(const string& str1, const string& str2);
        void AppendFormat(const string& str1, const string& str2, const string& str3);
        void AppendFormat(const string& str1, const string& arg1, const string& arg2, const string& arg3);
        void AppendFormat(const string& str1, const string& arg1, const int& arg2);

        void AppendFormat(const string& str1, const int& arg);
        void AppendFormat(const string& str1, const unsigned int& arg1, const string& arg2);
        void AppendFormat(const string& str1, const string& arg1, const int& arg2, const string& arg3);
        void AppendFormat(const string& str1, const unsigned int& arg1, const unsigned int& arg2, const string& arg3, const string& arg4);
        void AppendFormat(const string& str1, const unsigned int& arg1, const string& arg2, const string& arg3);
        string ToString(){return mStringing.str();}
};

}

#endif

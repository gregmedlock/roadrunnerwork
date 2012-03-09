#ifndef rrStringBuilderH
#define rrStringBuilderH
#include <sstream>
#include <string>
#include "rrObject.h"

using std::stringstream;
using std::string;

namespace rr
{
#define tab "\t"

class RR_DECLSPEC StringBuilder : public rrObject
{

	protected:
    	stringstream 				mStringing;
		int							mSizeOfVarField1;
		int							mSizeOfVarField2;
		int							mSizeOfVarField3;
	public:

    								StringBuilder(const string& aStr = "");

		stringstream& 				operator<<(const string& str);
        string 						ToString();
		void 						FormatVariable(const string& type, const string& varName, const string& comment = "");
		void 						FormatArray(const string& type, const string& varName, const int& arraySize, const string& comment = "");
        void						NewLine(const string& line = "");
        void						Line(const string& line);
        void						TLine(const string& line, const int& tabs = 1);
        void						Clear();


};

}

#endif

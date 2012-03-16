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
        string						mDeclSpec;
	public:

    								StringBuilder(const string& aStr = "", const string& decl_spec = "D_S");

		stringstream& 				operator<<(const string& str);
        string 						ToString();

		void 						FormatVariable(const string& type, const string& varName, const string& comment = "");
		void						AddFunctionExport(const string& retValue, const string& funcProto);
		void						AddFunctionProto(const string& retValue, const string& funcProto);
		void 						FormatArray(const string& type, const string& varName, const int& arraySize, const string& comment = "");
        void						NewLine(const string& line = "");
        void						Line(const string& line);
        void						TLine(const string& line, const int& tabs = 1);
        void						Clear();


};

}

#endif

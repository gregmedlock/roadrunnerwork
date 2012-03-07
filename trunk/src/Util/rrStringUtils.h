#ifndef rrStringUtilsH
#define rrStringUtilsH
#include <string>
#include <list>
#include <vector>
#include "rrExporter.h"

using std::string;
using std::list;
using std::vector;

namespace rr
{

string 			RR_DECLSPEC ChangeFileNameExtensionTo(const string& theFileName, const string& newExtension);

string			RR_DECLSPEC Trim(const string& str);
bool			RR_DECLSPEC StartsWith(const string& src, const string& sub);
bool			RR_DECLSPEC EndsWith(const string& src, const string& sub);

//conversions
string			RR_DECLSPEC IntToStr(const int& nt);
int				RR_DECLSPEC StrToInt(const string& nt);
string			RR_DECLSPEC DblToStr(const double& nt);
double			RR_DECLSPEC StrToDbl(const string& nt);
vector<string>  RR_DECLSPEC SplitString(const string &text, const string &separators);
vector<string> 	RR_DECLSPEC SplitString(const string& input, const char& delimiters);
int				RR_DECLSPEC ToInt(const string& str);
bool			RR_DECLSPEC ToBool(const string& str);
double			RR_DECLSPEC ToDouble(const string& str);

string			RR_DECLSPEC ToString(const bool& b);
string			RR_DECLSPEC ToString(const double& d, const string& format = "%f");
string			RR_DECLSPEC ToString(const unsigned int n, const int nBase=10);
string			RR_DECLSPEC ToString(const int n, const int nBase=10);
string			RR_DECLSPEC ToString(const long n, const int nBase=10);
string			RR_DECLSPEC ToString(const unsigned long n, const int nBase=10);
string			RR_DECLSPEC ToString(const unsigned short n, const int nBase=10);
string			RR_DECLSPEC ToString(const short n, const int nBase=10);
string			RR_DECLSPEC ToString(const char n);
string			RR_DECLSPEC ToString(const unsigned char n);
string			RR_DECLSPEC ToString(const string & s);
string			RR_DECLSPEC ToString(const char* str);
//bool 			RR_DECLSPEC InStringList(const string& fldr, list<string>& theList);

string 			RR_DECLSPEC Format(const string& src, const int& arg);
string 			RR_DECLSPEC Format(const string& src, const string& arg);
string 			RR_DECLSPEC Format(const string& src, const string& arg1, const string& arg2, const string& arg3);
string 			RR_DECLSPEC Format(const string& src, const string& arg1, const string& arg2);

string 			RR_DECLSPEC Format(const string& src, const string& arg1, const int& arg2);
string 			RR_DECLSPEC Format(const string& src, const string& arg1, const int& arg2, const string& arg3);

string 			RR_DECLSPEC Substitute(const string& src, const string& thisOne, const string& withThisOne, const int& howMany = -1);
string 			RR_DECLSPEC Substitute(const string& src, const string& thisOne, const int& withThisOne, const int& howMany = -1);
string      	RR_DECLSPEC RemoveNewLines(const string& str, const int& howMany = -1);
}
#endif

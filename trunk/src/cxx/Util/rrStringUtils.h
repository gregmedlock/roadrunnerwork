#ifndef rrStringUtilsH
#define rrStringUtilsH
//---------------------------------------------------------------------------
#include <string>
#include <list>
#include <vector>
#include "rrExporter.h"
using std::string;
using std::list;
using std::vector;

namespace rr
{
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
string			RR_DECLSPEC ToUpperOrLowerCase(const string& inStr, int (*func)(int) = std::toupper);
string			RR_DECLSPEC ToUpper(const string& inStr);
string			RR_DECLSPEC ToLower(const string& inStr);
string			RR_DECLSPEC ToString(const bool b);
string			RR_DECLSPEC ToString(const double& d);
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
}
#endif

#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrStringBuilder.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

StringBuilder::StringBuilder(const string& aStr)
{
    mStringing<<aStr;
}

void StringBuilder::Append(const string& str)
{
    mStringing<<str;
	Log(lDebug5)<<"Appended :"<<RemoveNewLines(str, 1);
}

void StringBuilder::Append(const unsigned int& str)
{
    mStringing<<str;
	Log(lDebug5)<<"Appended :"<<str;
}

void StringBuilder::Append(const string& s1, const string& s2)
{
    Append(s1);
    Append(s2);
}

void  StringBuilder::Append(const string& s1, const string& s2, const string& s3)
{
    Append(s1);
    Append(s2);
    Append(s3);
}

void  StringBuilder::Append(const string& s1, const unsigned int& s2, const string& s3)
{
    Append(s1);
    Append(s2);
    Append(s3);
}

void StringBuilder::Append(const string& s1, const unsigned int& s2, const string& s3, const string& s4)
{
    Append(s1);
    Append(s2);
    Append(s3);
    Append(s4);
}

void StringBuilder::AppendFormat(const string& str1, const string& str2)
{
	//Find all {0} in str1 and substitute with str2
	string token("{0}");
	string newString(str1);

    newString = Substitute(newString, token, str2);
	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const string& sub1, const string& sub2)
{
	string token1("{0}");
	string token2("{1}");
	string newString(str1);

    newString = Substitute(newString, token1, sub1);
    newString = Substitute(newString, token2, sub2);
	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const string& arg1, const string& arg2, const string& arg3)
{
	string token1("{0}");
	string token2("{1}");
	string token3("{2}");
	string newString(str1);

    newString = Substitute(newString, token1, arg1);
    newString = Substitute(newString, token2, arg2);
    newString = Substitute(newString, token3, arg3);
	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const int& arg1)
{
	string token1("{0}");
	string newString(str1);

    newString = Substitute(newString, token1, arg1);
	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const unsigned int& sub1, const string& sub2)
{
	string token1("{0}");
	string token2("{1}");
	string newString(str1);

    newString = Substitute(newString, token1, sub1);
    newString = Substitute(newString, token2, sub2);
	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const unsigned int& sub1, const string& sub2, const string& sub3)
{
	string token1("{0}");
	string token2("{1}");
	string token3("{2}");
	string newString(str1);

    newString = Substitute(newString, token1, rr::ToString(sub1));
	newString = Substitute(newString, token2, sub2);
  	newString = Substitute(newString, token3, sub3);

	Append(newString);
}

void StringBuilder::AppendFormat(const string& str1, const unsigned int& sub1, const unsigned int& sub2, const string& sub3, const string& sub4)
{
	string tok1("{0}");
	string tok2("{1}");
	string tok3("{2}");
	string tok4("{2}");
	string newString(str1);

    newString = Substitute(newString, tok1, sub1);
	newString = Substitute(newString, tok2, sub2);
  	newString = Substitute(newString, tok3, sub3);
  	newString = Substitute(newString, tok4, sub4);

	Append(newString);
}

stringstream& StringBuilder::operator<<(const string& str)
{
	Append(str);
    return mStringing;
}

}

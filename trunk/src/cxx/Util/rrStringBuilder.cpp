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

string Substitute(const string& src, const string& thisOne, const string& withThisOne);

StringBuilder::StringBuilder(const string& aStr)
{
    mStringing<<aStr;
}

void StringBuilder::Append(const string& str)
{
    mStringing<<str;
}

void  StringBuilder::Append(const string& s1, const string& s2)
{
    mStringing<<s1<<s2;
}

void  StringBuilder::Append(const string& s1, const string& s2, const string& s3)
{
    mStringing<<s1<<s2<<s3;
}

void  StringBuilder::Append(const string& s1, const unsigned int& s2, const string& s3)
{
    mStringing<<s1<<s2<<s3;
}

void StringBuilder::Append(const string& s1, const unsigned int& s2, const string& s3, const string& s4)
{
    mStringing<<s1<<s2<<s3<<s4;
}

void StringBuilder::AppendFormat(const string& str1, const string& str2)
{
	//Find all {0} in str1 and substitute with str2
	string token("{0}");
	string newString(str1);

    while(newString.find(token) != string::npos)
    {
    	newString.replace(newString.find(token),token.size(), str2);
    }
	mStringing<<newString;
}

void StringBuilder::AppendFormat(const string& str1, const unsigned int& sub1, const string& sub2)
{
	string token1("{0}");
	string token2("{1}");
	string newString(str1);

    while(newString.find(token1) != string::npos)
    {
    	newString.replace(newString.find(token1),token1.size(), rr::ToString(sub1));
    }

    while(newString.find(token2) != string::npos)
    {
    	newString.replace(newString.find(token2),token2.size(), sub2);
    }
	mStringing<<newString;
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

	mStringing<<newString;
}

string Substitute(const string& src, const string& thisOne, const string& withThisOne)
{
	string newString(src);

    while(newString.find(thisOne) != string::npos)
    {
    	newString.replace(newString.find(thisOne), thisOne.size(), withThisOne);
    }

	return newString;
}
stringstream& StringBuilder::operator<<(const string& str)
{
	mStringing<<str;
    return mStringing;
}

}

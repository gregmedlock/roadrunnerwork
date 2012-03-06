#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{

string ChangeFileNameExtensionTo(const string& fName, const string& newExtension)
{
	string newFName;

	//First create the file name, remove current extension if it exists
    if(fName.find('.'))
    {
		//Extension does exist. Cut it, and append new one
        newFName =  fName.substr(0, fName.find_last_of('.'));
    }

    if(newExtension[0] == '.')
    {
        newFName = newFName + newExtension;
    }
    else
    {
    	newFName = newFName + "." + newExtension;
    }

	return newFName;
}

bool StartsWith(const string& src, const string& sub)
{
	bool result = (src.compare(0, sub.size(), sub) == 0);
	return result;
}

bool EndsWith(const string& src, const string& sub)
{
	bool result = (src.compare(src.size() - sub.size(), src.size(), sub) == 0);
	return result;
}

string Trim(const string& str)
{

	string trimmed(str);

	string::size_type pos = trimmed.find_last_not_of(' ');
  	if(pos != string::npos)
    {
    	trimmed.erase(pos + 1);
    	pos = trimmed.find_first_not_of(' ');
    	if(pos != string::npos)
        {
        	trimmed.erase(0, pos);
        }
  	}
    else
    {
    	trimmed.erase(trimmed.begin(), trimmed.end());
    }
	return trimmed;
}


string RemoveNewLines(const string& str, const int& howMany)
{
	return Substitute(str, "\n" , "", howMany);
}

string Format(const string& src, const string& arg)
{
	return Substitute(src, "{0}", arg);
}

string Format(const string& src, const int& arg)
{
	return Substitute(src, "{0}", ToString(arg));
}

string Format(const string& src, const string& arg1, const string& arg2)
{
	string tmp = Substitute(src, "{0}", arg1);
    return Substitute(tmp, "{1}", arg2);
}

string Format(const string& src, const string& arg1, const int& arg2)
{
	string tmp = Substitute(src, "{0}", arg1);
    return Substitute(tmp, "{1}", ToString(arg2));
}

string Format(const string& src, const string& arg1, const string& arg2, const string& arg3)
{
	string tmp = Substitute(src, "{0}", arg1);
    tmp = Substitute(src, "{1}", arg2);
 	return Substitute(src, "{2}", arg3);
}

string Format(const string& src, const string& arg1, const int& arg2, const string& arg3)
{
	string tmp = Substitute(src, "{0}", arg1);
    tmp = Substitute(src, "{1}", ToString(arg2));
 	return Substitute(src, "{2}", arg3);
}

string Substitute(const string& src, const string& thisOne, const int& withThisOne, const int& howMany)
{
	return Substitute(src, thisOne, ToString(withThisOne), howMany);
}

string Substitute(const string& src, const string& thisOne, const string& withThisOne, const int& howMany)
{
	string newString(src);
    int count = 0;

    while(newString.find(thisOne) != string::npos)
    {
        if(count == howMany)
        {
            break;
        }
        else
        {
    		newString.replace(newString.find(thisOne), thisOne.size(), withThisOne);
            count++;
        }
    }
	return newString;
}

//bool InStringList(const string& fldr, list<string>& theList)
//{
//	list<string>::iterator index = std::find_if(theList.begin(), theList.end(),  mtkCompareStrings(fldr));
//	return (index != theList.end()) ? true : false;
//}

string IntToStr(const int& nt)
{
	//char *itoa(int value, char *string, int radix);
	char str[100];
	itoa(nt, str, 10);
	string valStr(str);
	return valStr;
}

string DblToStr(const double& nt)
{
	char str[25];
	int sig = 5; /* significant digits */
	 gcvt(nt, sig, str);
	return string(str);
}

int StrToInt(const string& str)
{
	return atoi(str.c_str());
}

double StrToDbl(const string& str)
{
	char *endptr;
	return strtod(str.c_str(), &endptr);
}

vector<string> SplitString(const string &text, const char& oneSep)
{
    string separator;
    separator = oneSep;
    return SplitString(text, separator);
}

vector<string> SplitString(const string &text, const string &separators)
{
    vector<string> words;
    int n = text.length();
    int start = text.find_first_not_of(separators);

    while( (start >= 0) && (start < n) )
    {
        int stop = text.find_first_of(separators, start);
        if( (stop < 0) || (stop > n) )
        {
            stop = n;
        }
        words.push_back(text.substr(start, stop - start));
        start = text.find_first_not_of(separators, stop+1);
    }

    return words;
}

vector<string> SplitString(const string &text, const string &separators, bool cutDelimiter)
{
    vector<string> words;
    int n = text.length();
    int start = text.find_first_not_of(separators);
    while( (start >= 0) && (start < n) )
    {
        int stop = text.find_first_of(separators, start);
        if( (stop < 0) || (stop > n) )
        {
            stop = n;
        }

        if(cutDelimiter)
        {
            words.push_back(text.substr(start, stop - start));
        }
        else
        {
            start -= 1;
            if(stop != n)
            {
                stop += 1;
            }
            words.push_back(text.substr(start, stop - start));
        }
        start = text.find_first_not_of(separators, stop+1);
    }

    return words;
}

int SplitString(vector<string>& words, const string &text, const string &separators)
{
    int n = text.length();
    int start = text.find_first_not_of(separators);
    while( (start >= 0) && (start < n) )
    {
        int stop = text.find_first_of(separators, start);
        if( (stop < 0) || (stop > n) )
        {
            stop = n;
        }
        words.push_back(text.substr(start, stop - start));
        start = text.find_first_not_of(separators, stop+1);
    }

    return words.size();
}

int ToInt(const string& str)
{
	return atoi(str.c_str());
}

bool ToBool(const string& str)
{
    if(str.size() < 2)
        return (str == "1")     ? true : false;
    else
        return (str == "true")  ? true : false;
}

double ToDouble(const string& str)
{
	if(!str.size())
    	return 0;

	char *endptr = NULL;
	return strtod(str.c_str(), &endptr);
}
string ToUpperOrLowerCase(const string& inStr, int (*func)(int))
{
	string rString(inStr);
	std::transform(rString.begin(), rString.end(), rString.begin(), func);
	return rString;
}

string ToUpper(const string& inStr)
{
	string rString(inStr);
	std::transform(rString.begin(), rString.end(), rString.begin(), (int(*)(int)) toupper);
	return rString;
}

string ToLower(const string& inStr)
{
	string rString(inStr);
	std::transform(rString.begin(), rString.end(), rString.begin(), (int(*)(int)) tolower);
	return rString;
}

string ToString(const char* str)
{
    return (string(str));
}

string ToString(const bool& val)
{
    return val ? "true" : "false";
}

string ToString(const double& d, const string& format)
{
    char sBuffer[256];
    sprintf(sBuffer, format.c_str(), d);
    return string(sBuffer);
}

string ToString(const unsigned int n, const int nBase)
{
    char sBuffer[256];
    if (nBase == 16)
	{
		sprintf(sBuffer, "%X", n);
    	return string("0x") + string(sBuffer);
	}
    else if(nBase == 2)
	{
		string tmp = "";
		int k = n;
		for (int i=0; i<8; i++)
		{
			if ((k & 0x80) != 0)
				tmp += "1";
			else
				tmp += "0";
			k = k<<1;
		}
    	return "0b" + tmp;
	}
    else
	{
		sprintf(sBuffer, "%d", n);
    	return string(sBuffer);
	}
}

string ToString(const int n, const int nBase)
{
    char sBuffer[256];
    if (nBase == 16)
	{
		sprintf(sBuffer, "%X", n);
    	return string("0x") + string(sBuffer);
	}
    else if(nBase == 2)
	{
		string tmp = "";
		int k = n;
		for (int i=0; i<8; i++)
		{
			if ((k & 0x80) != 0)
				tmp += "1";
			else
				tmp += "0";
			k = k<<1;
		}
    	return "0b" + tmp;
	}
    else
	{
		sprintf(sBuffer, "%d", n);
    	return string(sBuffer);
	}
}

string ToString(const long n, const int nBase)
{
	char sBuffer[256];
	if (nBase == 10)
	{
		sprintf(sBuffer, "%lu", n);
		return string(sBuffer);
	}
	return ToString( int(n), nBase);
}

string ToString(const unsigned long n, const int nBase)
{
    char sBuffer[256];
    if (nBase == 10)
	{
		sprintf(sBuffer, "%lu", n);
    	return string(sBuffer);
	}
	return ToString( int(n), nBase);
}

string ToString(const unsigned short n, const int nBase)
{
    char sBuffer[256];
    if (nBase == 10)
	{
		sprintf(sBuffer, "%u", n);
    	return string(sBuffer);
	}
	return ToString( int(n), nBase);
}

string ToString(const short n, const int nBase)
{
	return ToString( int(n), nBase);
}

string ToString(const char n)
{
    char sBuffer[256];
	sprintf(sBuffer, "%c", n);
    return string(sBuffer);
}

string ToString(const unsigned char n)
{
    char sBuffer[256];
	sprintf(sBuffer, "%c", n);
   	return string(sBuffer);
}

int CompareNoCase(const string& str1, const string& str2)
{
	int res = stricmp(str1.c_str(), str2.c_str());
	return res;
}


//double ConvertstringToDouble(const string& dbl);
//double ConvertstringToDouble(const string& s)
//{
//	//get unit;
//	int nPos = strcspn(s.c_str(), "nums");
//	char unit = s[nPos];
//
//	//get the scaling factor;
//	double dScale = 1.0;
//	switch (unit)
//	{
//		case 'n':   dScale = 1.0; break;
//		case 'u':   dScale = 1.0e3; break;
//		case 'm':   dScale = 1.0e6; break;
//		case 's':   dScale = 1.0e9; break;
//		default:
//	            dScale = 1.0;
//	}
//
//	return atof(s.c_str()) * dScale;
//}

}


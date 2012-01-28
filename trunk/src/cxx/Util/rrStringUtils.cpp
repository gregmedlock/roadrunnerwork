#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)




namespace rr
{

string GetStringFromSeconds(long elapsed)
{
	if(elapsed < 0)
    {
    	return "00:00:00";
    }

	int hours = elapsed / 3600;
	int minutes = (elapsed / 60) % 60 ;
	int seconds = elapsed % 60;

	string h,m,s;
	h 	= ToString(hours);
	m 	= ToString(minutes);
	s	= ToString(seconds);
	if(hours < 10)
	{
		h = "0" + h;
	}
	if(minutes <10)
	{
		m = "0" + m;
	}
	if(seconds < 10)
	{
		s = "0" + s;
	}
	string timeStr = h + ":" + m + ":" + s;

	return timeStr;
}

//bool InStringList(const string& fldr, list<string>& theList)
//{
//	list<string>::iterator index = std::find_if(theList.begin(), theList.end(),  mtkCompareStrings(fldr));
//	return (index != theList.end()) ? true : false;
//}


string IntToStr(const int& nt)
{
	//char *itoa(int value, char *string, int radix);
	char str[100];// = new char(100);
	itoa(nt, str, 10);
	string valStr(str);
//	delete str;
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

string GetPath(const string& f)
{
	string file = f;
	//Check if the file comes with a path..
	string separators = "\\";
	int start = file.find_first_of(separators);
	int end   = file.find_last_of(separators);
	if(start < 0)
{//no path
		return string("");
	}
	else{
		return string(file.erase(end, file.size()));
	}
}

string ExtractPath(string& file)
{
	//Check if the file comes with a path..
	string separators = "\\";
  	string temp = file;
	int start = file.find_first_of(separators);
	int end   = file.find_last_of(separators);
	if(start < 0) //no path
    {
		return string("");
	}
	else
    {
  	file.erase(0, end);
		return string(temp.erase(end, file.size()));
	}
}

string RemoveExtension(const string& file)
{
    string fName = file;
    if(fName.rfind(".") <= fName.size())
    {
        return string(fName.erase(fName.rfind("."), fName.size()));
    }
    else
    {
		return string(fName);
    }
}

string GetFileExtension(const string& file)
{
	string ext = file;
	ext = ext.erase(0, ext.find("."));
	if(ext.size())
    {
		ext.erase(0,ext.find(".") + 1);
    }
	return ext;
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


//vector<int> StrRecordsToIntVector(const string& theString)
//{
//	vector<int> vec;
//
//	CTable	tRecords(theString);
//	if(tRecords.GetEntryCount())
//	{
//		vec.resize(tRecords.GetEntryCount());
//	}
//	else
//	{
//		return vector<int>(0);
//	}
//
//	for(unsigned int i = 0; i < vec.size(); i++)
//	{
//		vec[i] = atoi(tRecords.GetCurrentEntry().c_str());
//		tRecords.IncrementTablePointer();
//	}
//	return vec;
//}
//
//bool GetBool(mtkIniFile& iniFile, const string& section, const string& descr, const bool& defValue)
//{
//	string defValueS = ToString(defValue);
//
//	string tempStr 		= 	ToLower( iniFile.GetString(section, descr, defValueS) );
//	if(tempStr.size() == 1)
//		return (tempStr == "0") ? false: true;
//	else
//		return (tempStr == "true") ? true : false;
//}

string JoinPath(const string& aPath, const string& aFile)
{
	//Just check the paths last position. it has to be a "/"
	//Otherwise, add it before joining
	if(aPath.size() > 0)
	{
		if(aPath[aPath.size() - 1] != '\\')
		{
			return aPath + "\\" + aFile;
		}
		return aPath + aFile;
	}
	return aFile;
}

string GetTimeString()
{
	time_t timer;
   	struct tm *tblock;
   	/* gets time of day */
   	timer = time(NULL);
   	/* converts date/time to a structure */
   	tblock = localtime(&timer);
   	string theTime = asctime(tblock);
	return theTime;
}

string GetFormattedDateTimeString(const string& format)
{
    struct tm *time_now;
    time_t secs_now;
    char str[200];
    time(&secs_now);
    time_now = localtime(&secs_now);
    strftime(str, 80, format.c_str(), time_now);
    return string(str);
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

//double EatNumber(string &str)
//{
////    //First remove any non-digit characters...
//// 	while(str[0])
////    {
////		if(isdigit(str[0]))
////            break;
////         else
////            str.erase(0,1);
////	}
//
//	char *endptr;
//	double nr = strtod(str.c_str(), &endptr);
//	str.erase(0, str.find(endptr));
//	return nr;
//}

//complex<float> ToComplexFloat(const string& _str)
//{
//    string str = _str;
//    float re = EatNumber(str);
//
//    float im = EatNumber(str);
//	return complex<float>(re,im);
//}

double ToDouble(const string& str)
{
	if(!str.size())
    	return 0;

	char *endptr = NULL;
	return strtod(str.c_str(), &endptr);
}

string GetFilePath(const string& pathAndFile)
{
	string file = pathAndFile;
	//Check if the file comes with a path..
//#ifdef WINDOWS
//	string separators = "\\";
//#else
    string seperators = "/";
//#endif
	int start = file.find_first_of(seperators);
	int end   = file.find_last_of(seperators);
	if(start < 0)
    {
		return string("");
	}
	else
    {
		return string(file.erase(end + 1, file.size()));
	}
}

string ExtractPathTo(const string& filePathOrJustPath, const string& pathSeparator)
{
	string file = filePathOrJustPath;

	//Check if the file comes with a path..
	int start = file.find_first_of(pathSeparator);
	int end   = file.find_last_of(pathSeparator);
	if(start < 0)
    {
		return string("");
	}
	else
    {
       //	if(
		return string(file.erase(end , file.size()));
	}
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

using namespace std;
string ToString(const bool b)
{
	return (b ? "true" : "false");
}

//string ToString(const float d)
//{
//    char sBuffer[256];
//    sprintf(sBuffer,"%g", d);
//    return string(sBuffer);
//}

string ToString(const string & s)
{
    return s;
}

string ToString(const char* str)
{
    return (string(str));
}

string ToString(const double& d)
{
    char sBuffer[256];
    sprintf(sBuffer,"%e", d);
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


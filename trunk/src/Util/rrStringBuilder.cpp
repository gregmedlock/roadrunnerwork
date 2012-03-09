#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iomanip>
#include "rrStringBuilder.h"
#include "rrStringUtils.h"
#include "rrLogger.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------

using namespace std;
namespace rr
{

StringBuilder::StringBuilder(const string& aStr)
:
mSizeOfVarField1(35),
mSizeOfVarField2(40),
mSizeOfVarField3(20)
{
    mStringing<<aStr;
}

string StringBuilder::ToString()
{
	return mStringing.str();
}

void StringBuilder::Clear()
{
	mStringing.str("");
}

stringstream& StringBuilder::operator<<(const string& str)
{
	mStringing<<str;
	Log(lDebug5)<<"Appended :"<<RemoveNewLines(str, 1);
    return mStringing;
}

void StringBuilder::FormatVariable(const string& type, const string& varName, const string& comment)
{

    mStringing<<"\t"<<left<<setw(mSizeOfVarField1)<<type	<<varName<< 	setw(mSizeOfVarField2)<<";";
    if(comment.size())
    {
    	mStringing<<"//"<<comment;
    }

   	mStringing<<endl;
}

void StringBuilder::FormatArray(const string& type, const string& varName, const int& arraySize, const string& comment)
{
	string field2(varName +"["+ rr::ToString(arraySize)+"];");
    mStringing<<"\t"<<left<<setw(mSizeOfVarField1)<<type	<< setw(mSizeOfVarField2)<<field2;
    if(comment.size())
    {
    	mStringing<<left<<setw(mSizeOfVarField3)<<"//" + comment;
    }

   	mStringing<<endl;
}

void StringBuilder::NewLine(const string& line)
{
	mStringing<<"\n"<<line<<endl;
}

void StringBuilder::Line(const string& line)
{
	mStringing<<line<<endl;
}

void StringBuilder::TLine(const string& line, const int& nrTabs)
{
	string tabs;
    for(int i = 0; i < nrTabs; i++)
    {
    	tabs +="\t";
    }

	mStringing<<tabs<<line<<endl;
}

}

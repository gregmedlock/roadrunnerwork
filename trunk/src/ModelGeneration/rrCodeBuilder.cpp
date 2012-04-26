#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iomanip>
#include "rrStringUtils.h"
#include "rrCodeBuilder.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{

CodeBuilder::CodeBuilder(const string& aStr, const string& decl_spec, const string& call_conv)
:
mSizeOfVarField1(35),
mSizeOfVarField2(40),
mSizeOfVarField3(20),
mDeclSpec(decl_spec),
mCallingConvention(call_conv)

{
    mStringing<<aStr;
}

void CodeBuilder::FormatVariable(const string& type, const string& varName, const string& comment)
{

    mStringing<<"\t"<<left<<setw(mSizeOfVarField1)<<type	<<varName<< 	setw(mSizeOfVarField2)<<";";
    if(comment.size())
    {
    	mStringing<<"//"<<comment;
    }

   	mStringing<<endl;
}

void CodeBuilder::AddFunctionExport(const string& retValue, const string& funcProto)
{
	//mStringing<<mDeclSpec<<" "<<left<<setw(mSizeOfVarField1)<<retValue<<mCallingConvention<<setw(mSizeOfVarField2)<<funcProto + ";"<<endl;
	mStringing<<mDeclSpec<<" "<<left<<setw(mSizeOfVarField1)<<retValue<<setw(mSizeOfVarField2)<<funcProto + ";"<<endl;
}

void CodeBuilder::AddFunctionProto(const string& retValue, const string& funcProto)
{
	mStringing<<"   "<<" "<<left<<setw(mSizeOfVarField1)<<retValue<<setw(mSizeOfVarField2)<<funcProto + ";"<<endl;
}

void CodeBuilder::FormatArray(const string& type, const string& varName, const int& _arraySize, const string& comment)
{
	int arraySize = _arraySize;
    if(arraySize == 0)
    {
        //an array of zero length is undefined.. don't put it in the header..
     //   mStringing<<"//";
     	arraySize = 10;
    }
	string field2(varName +"["+ rr::ToString(arraySize)+"];");
    mStringing<<"\t"<<left<<setw(mSizeOfVarField1)<<type	<< setw(mSizeOfVarField2)<<field2;
    if(comment.size())
    {
    	mStringing<<left<<setw(mSizeOfVarField3)<<"//" + comment;
    }

   	mStringing<<endl;
}

}
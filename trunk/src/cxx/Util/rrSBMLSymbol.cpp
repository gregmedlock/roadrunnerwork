#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include <limits>

#include "rrSBMLSymbol.h"
#include "rrStringUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
namespace rr
{
SBMLSymbol::SBMLSymbol()
:
mValue(std::numeric_limits<double>::quiet_NaN()),//Represents an un-initialized value
mConcentration(mValue),
mAmount(mValue),
mHasRule(false)
{

}

SBMLSymbol::SBMLSymbol(const SBMLSymbol& cp)
:
mValue(cp.mValue),
mConcentration(mValue), //tie reference.. does this work?
mAmount(mValue),			//tie reference.. does this work?
mId(cp.mId),
mType(cp.mType),
IsSetAmount(cp.IsSetAmount),
IsSetConcentration(cp.IsSetConcentration),
mInitialAssignment(cp.mInitialAssignment),
mHasRule(cp.mHasRule),
mRule(cp.mRule)
{

}

SBMLSymbol& SBMLSymbol::operator =(const SBMLSymbol& rhs)
{
	//Copy properties, one by one
	mId = rhs.mId;
	mType = rhs.mType;
	mValue = rhs.mValue;
	IsSetAmount = rhs.IsSetAmount;
	IsSetConcentration = rhs.IsSetConcentration;
	mInitialAssignment = rhs.mInitialAssignment;
	mHasRule = rhs.mHasRule;
	mRule = rhs.mRule;
	return *this;
}


bool SBMLSymbol::HasValue()
{
	return IsNaN(mValue) ? false : true;
}

bool SBMLSymbol::HasInitialAssignment() const
{
	return mInitialAssignment.size() ? true : false;
}

bool SBMLSymbol::HasRule()
{
	return mRule.size() ? true : false;
}


ostream& operator<<(ostream& stream, const SBMLSymbol& symbol)
{
	//stream symbol to stream
    stream<<"ID = "<<			symbol.mId								<<endl;
    stream<<"Type = "<<         symbol.mType							<<endl;
    stream<<"Value = "<<		symbol.mValue							<<endl;
	stream<<"Has Initial Assignment = "<<symbol.HasInitialAssignment() 	<<endl;

    if(symbol.HasInitialAssignment())
    {
    	stream<<"Intial Assignment = "<<symbol.mInitialAssignment			<<endl;
    }
    stream<<"..add more info..."										<<endl;
    stream<<"Has Rule = "<<		ToString(symbol.mHasRule)				<<endl;
    if(symbol.mHasRule)
    {
		stream<<"Rule = "<<		symbol.mRule					<<endl;

    }
    return stream;
}

}

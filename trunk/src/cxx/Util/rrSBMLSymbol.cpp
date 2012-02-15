#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrSBMLSymbol.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{
SBMLSymbol& SBMLSymbol::operator =(const SBMLSymbol& rhs)
{
	//Copy properties, one by one
	mId = rhs.mId;
	mType = rhs.mType;
	mValue = rhs.mValue;
	//	double& mConcentration; //Assing ref to mValue..
//	double& mAmount; //Assing ref to mValue..
	IsSetAmount = rhs.IsSetAmount;
	IsSetConcentration = rhs.IsSetConcentration;
	mInitialAssignment = rhs.mInitialAssignment;
	mHasRule = rhs.mHasRule;
	mRule = rhs.mRule;

	return *this;
}

}

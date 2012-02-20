#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrRule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


namespace rr
{

RRRule::RRRule(const string& rule, const string& ruleTypeStr)
:
mTheRule(rule),
mRuleTypeStr(ruleTypeStr),
mRuleType(rtUnknown)
{
	AssignType();
}

string RRRule::GetLHS()
{

}

string RRRule::GetRHS()
{

}

RuleType RRRule::GetType()
{

}

void RRRule::AssignType()
{

	mRuleType = GetRuleTypeFromString(mRuleTypeStr);

}

RuleType GetRuleTypeFromString(const string& str)
{
	if(str == "Algebraic_Rule")
    {
		return rtAlgebraic;
    }
    else if(str == "Assignment_Rule")
    {
		return rtAssignment;
    }
    else if(str == "Rate_Rule")
    {
		return rtRate;
    }
    else
    {
    	return rtUnknown;
    }
}

}

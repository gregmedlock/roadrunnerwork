#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrRule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)


Rule::Rule(const string& rule, const string& ruleTypeStr)
:
mTheRule(rule),
mRuleTypeStr(ruleTYpeStr),
mRuleType(rtUnknown),
{
	AssignType();
}

string Rule::GetLHS()
{

}

string Rule::GetRHS()
{

}

RuleType GetType()
{

}

void RuleType::AssignType()
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



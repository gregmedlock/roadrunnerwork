#ifndef rrRuleH
#define rrRuleH
#include <string>
#include "rrObject.h"

using std::string;
namespace rr
{

enum RuleType {rtAlgebraic = 0, rtAssignment, rtRate, rtUnknown};

RuleType GetRuleTypeFromString(const string& str);

class RR_DECLSPEC Rule : rrObject
{
	protected:
		string 			mTheRule;
		string 			mRuleTypeStr;
        RuleType		mRuleType;
        void			AssignType();

    public:
						Rule(const string& rule, const string& ruleType);

    	string			GetLHS();
    	string			GetRHS();
        RuleType		GetType();


};


}
#endif

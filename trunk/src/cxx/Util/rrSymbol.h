#ifndef rrSymbolH
#define rrSymbolH
#include <string>
#include "rrExporter.h"
using std::string;

//---------------------------------------------------------------------------
namespace rr
{
    class RR_DECLSPEC Symbol
    {
    	public:
        	string compartmentName; 	// Used when symbol is a species
        	bool hasOnlySubstance; 		// used when symbol is a species
            string formula; 			// used in case of species defined using initialAmounts;
            string keyName; 			// Used when storing local parameters, keyName is the reaction name
            string name;

            // Set if species also has a rate rule. Use to prevent a dydt being output
            // in the model function if there is a rate rule for it
            bool rateRule;
            double value;

            Symbol(const string& _name = "", const double& _value = 0)
            {
                name = _name;
                value = _value;
                rateRule = false;
            }

            Symbol(const string& _keyName, const string& _name, const double& _value)
            {
                keyName = _keyName;
                name = _name;
                value = _value;
                rateRule = false;
            }

            Symbol(const string& _name, const double _value, const string& _compartmentName)
            {
                name = _name;
                value = _value;
                compartmentName = _compartmentName;
            }

            Symbol(const string& _name, const double& _value, const string& _compartmentName, const string& _formula)
            {
                name = _name;
                value = _value;
                compartmentName = _compartmentName;
                formula = _formula;
            }
    }; //class rr::Symbol
}//namespace rr


#endif
//namespace LibRoadRunner.Util
//{
//    public class Symbol
//    {
//        public string compartmentName; // Used when symbol is a species
//        public bool hasOnlySubstance; // used when symbol is a species
//
//        public string formula; // used in case of species defined using initialAmounts;
//        public string keyName; // Used when storing local parameters, keyName is the reaction name
//        public string name;
//
//
//        // Set if species also has a rate rule. Use to prevent a dydt being output
//        // in the model function if there is a rate rule for it
//        public bool rateRule;
//        public double value;
//
//        public Symbol(string name, double value)
//        {
//            this.name = name;
//            this.value = value;
//            rateRule = false;
//        }
//
//        public Symbol(string keyName, string name, double value)
//        {
//            this.keyName = keyName;
//            this.name = name;
//            this.value = value;
//            rateRule = false;
//        }
//
//
//        public Symbol(string name, double value, string compartmentName)
//        {
//            this.name = name;
//            this.value = value;
//            this.compartmentName = compartmentName;
//        }
//
//        public Symbol(string name, double value, string compartmentName, string formula)
//        {
//            this.name = name;
//            this.value = value;
//            this.compartmentName = compartmentName;
//            this.formula = formula;
//        }
//    }
//}

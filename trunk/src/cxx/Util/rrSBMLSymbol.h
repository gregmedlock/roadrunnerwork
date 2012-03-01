#ifndef rrSBMLSymbolH
#define rrSBMLSymbolH
#include <vector>
#include "rrObject.h"
#include "rrUtils.h"

using std::vector;
namespace rr
{

class RR_DECLSPEC SBMLSymbol : public rrObject
{
	protected:
    public:
        string 					mId;
        enum SBMLType 			mType;
        vector<SBMLSymbol> 		mDependencies;

        double 					mValue;
        bool 					HasValue();

        double& 				mConcentration; //Assing ref to mValue..
        double& 				mAmount; //Assing ref to mValue..

        bool 					IsSetAmount;
        bool 					IsSetConcentration;

        bool 					HasInitialAssignment();
        string					mInitialAssignment;

        bool 					mHasRule;
        bool 					HasRule();
        string 					mRule;

    public:
    	SBMLSymbol();
        SBMLSymbol(const SBMLSymbol& cp);
        SBMLSymbol& operator =(const SBMLSymbol& rhs);

};
//COuld make it a friend...
ostream& RR_DECLSPEC operator<<(ostream& stream, const SBMLSymbol& symbol);
}
#endif



////namespace SBMLSupport
////{
////    public class SBMLSymbol
////    {
////        private string _Id;
////        public string Id
////        {
////            get { return _Id; }
////            set { _Id = value; }
////        }
////
////        private SBMLType _Type;
////        public SBMLType Type
////        {
////            get { return _Type; }
////            set { _Type = value; }
////        }
////
////        private List<SBMLSymbol> _Dependencies = new List<SBMLSymbol>();
////        public List<SBMLSymbol> Dependencies
////        {
////            get { return _Dependencies; }
////            set { _Dependencies = value; }
////        }
////
////        public bool HasValue
////        {
////            get { return !Double.IsNaN(_Value); }
////        }
////
////        private double _Value = Double.NaN;
////        public double Value
////        {
////            get { return _Value; }
////            set { _Value = value; }
////        }
////
////
////        public double Concentration
////        {
////            set
////            {
////                _Value = value;
////                _IsSetConcentration = true;
////            }
////        }
////
////        public double Amount
////        {
////            set
////            {
////                _Value = value;
////                _IsSetAmount = true;
////            }
////        }
////
////        private bool _IsSetAmount = false;
////        public bool IsSetAmount
////        {
////            get { return _IsSetAmount; }
////            set { _IsSetAmount = value; }
////        }
////
////        private bool _IsSetConcentration = false;
////        public bool IsSetConcentration
////        {
////            get { return _IsSetConcentration; }
////            set { _IsSetConcentration = value; }
////        }
////
////        public bool HasInitialAssignment
////        {
////            get { return !string.IsNullOrEmpty(_InitialAssignment); }
////        }
////
////        private string _InitialAssignment;
////        public string InitialAssignment
////        {
////            get { return _InitialAssignment; }
////            set { _InitialAssignment = value; }
////        }
////
////
////        public bool HasRule
////        {
////            get { return !string.IsNullOrEmpty(_Rule); }
////        }
////
////        private string _Rule;
////        public string Rule
////        {
////            get { return _Rule; }
////            set { _Rule = value; }
////        }
////
////    }
////}


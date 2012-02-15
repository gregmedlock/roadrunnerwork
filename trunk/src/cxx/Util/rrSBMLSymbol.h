#ifndef rrSBMLSymbolH
#define rrSBMLSymbolH
//---------------------------------------------------------------------------
#include "rrObject.h"

namespace rr
{

class RR_DECLSPEC SBMLSymbol : public rrObject
{
	protected:
    public:
		string mId;
////        private string _Id;
////        public string Id
////        {
////            get { return _Id; }
////            set { _Id = value; }
////        }
////
		enum SBMLType mType;
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
		double mValue;
////        public double Value
////        {
////            get { return _Value; }
////            set { _Value = value; }
////        }
////
////
		double& mConcentration; //Assing ref to mValue..
////        public double Concentration
////        {
////            set
////            {
////                _Value = value;
////                _IsSetConcentration = true;
////            }
////        }
////
		double& mAmount; //Assing ref to mValue..
////        public double Amount
////        {
////            set
////            {
////                _Value = value;
////                _IsSetAmount = true;
////            }
////        }
////
	bool IsSetAmount;
////        private bool _IsSetAmount = false;
////        public bool IsSetAmount
////        {
////            get { return _IsSetAmount; }
////            set { _IsSetAmount = value; }
////        }
////
	bool IsSetConcentration;
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
	string	mInitialAssignment;
////        public string InitialAssignment
////        {
////            get { return _InitialAssignment; }
////            set { _InitialAssignment = value; }
////        }
////
////
	bool mHasRule;
////        public bool HasRule
////        {
////            get { return !string.IsNullOrEmpty(_Rule); }
////        }
////
////        private string _Rule;
	string mRule;
////        public string Rule
////        {
////            get { return _Rule; }
////            set { _Rule = value; }
////        }
////

    public:
    	SBMLSymbol() : mConcentration(mValue), mAmount(mValue){}
        SBMLSymbol(const SBMLSymbol& cp) : mConcentration(mValue), mAmount(mValue){}
        SBMLSymbol& operator =(const SBMLSymbol& rhs);
};

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


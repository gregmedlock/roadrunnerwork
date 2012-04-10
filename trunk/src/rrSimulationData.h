#ifndef rrSimulationDataH
#define rrSimulationDataH
#include <fstream>
#include "rrObject.h"
#include "rrStringList.h"
#include "rrDoubleMatrix.h"

namespace rr
{

using std::ofstream;

//Class that  holds the data after a simulation...
class SimulationData : public rrObject
{

	protected:
		StringList 			mColumnNames;
        DoubleMatrix       	mTheData;
        DoubleMatrix       	mReferenceData;
		int					mTimePrecision;		//The precision when saved to file
		int					mDataPrecision;		//The precision when saved to file

	public:
							SimulationData();
							SimulationData(const StringList& colNames, const DoubleMatrix& theData);
		void				SetTimeDataPrecision(const int& prec){mTimePrecision = prec;}
		void				SetDataPrecision(const int& prec){mDataPrecision = prec;}
		void				SetColumnNames(const StringList& colNames);
		void				SetData(const DoubleMatrix& theData);
		bool				Load(const string& fileName);
		bool				LoadReference(const string& fileName);//When comparing to a reference
		bool				WriteTo(const string& fileName);
        friend ofstream& 	operator << (ofstream& fs, SimulationData& data);
        bool				Check();	//Check if containst proper data
};


}


#endif

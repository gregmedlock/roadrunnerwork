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
		int					mTimePrecision;		//The precision when saved to file
		int					mDataPrecision;		//The precision when saved to file

	public:
							SimulationData();
							SimulationData(const StringList& colNames, const DoubleMatrix& theData);
		StringList			GetColumnNames();
		string 				GetColumnNamesAsString();
		void				Allocate(const int& cSize, const int& rSize);
		void				SetTimeDataPrecision(const int& prec){mTimePrecision = prec;}
		void				SetDataPrecision(const int& prec){mDataPrecision = prec;}
		void				SetColumnNames(const StringList& colNames);
		void				SetNrOfCols(const int& cols);
        int					GetNrOfCols(){return mTheData.CSize();}
        int					GetNrOfRows(){return mTheData.RSize();}
		void				SetData(const DoubleMatrix& theData);
		bool				Load(const string& fileName);
		bool				WriteTo(const string& fileName);
        friend ofstream& 	operator << (ofstream& fs, SimulationData& data);
        bool				Check();	//Check if containst proper data

		double& 			operator() (const unsigned& row, const unsigned& col);
		double  			operator() (const unsigned& row, const unsigned& col) const;

};


}


#endif

#ifndef rrSimulationDataH
#define rrSimulationDataH
#include <fstream>
#include <sstream>
#include "rrObject.h"
#include "rrStringList.h"
#include "rrDoubleMatrix.h"
#include "rrExporter.h"
namespace rr
{

using std::ofstream;
using std::stringstream;

//Class that  holds the data after a simulation...
class RR_DECLSPEC SimulationData : public rrObject
{

    protected:
        StringList              mColumnNames;
        DoubleMatrix            mTheData;
        int                     mTimePrecision;        //The precision when saved to file
        int                     mDataPrecision;        //The precision when saved to file
        string                  mName;                //For debugging purposes mainly..

    public:
                                SimulationData();
                                SimulationData(const StringList& colNames, const DoubleMatrix& theData);
        StringList              GetColumnNames();
        string                  GetColumnNamesAsString();
        void                    Allocate(const int& cSize, const int& rSize);
        void                    SetTimeDataPrecision(const int& prec){mTimePrecision = prec;}
        void                    SetDataPrecision(const int& prec){mDataPrecision = prec;}
        void                    SetColumnNames(const StringList& colNames);
        void                    SetNrOfCols(const int& cols);
        int                     GetNrOfCols(){return mTheData.CSize();}
        int                     GetNrOfRows(){return mTheData.RSize();}
        void                    SetData(const DoubleMatrix& theData);
        bool                    Load(const string& fileName);
        bool                    WriteTo(const string& fileName);

        friend std::ostream&    operator << (std::ostream& ss, SimulationData& data);
        bool                    Check();    //Check if containst proper data

        double&                 operator() (const unsigned& row, const unsigned& col);
        double                  operator() (const unsigned& row, const unsigned& col) const;
        void                    SetName(const string& name);
        string                  GetName(){return mName;}
        pair<int,int>           Dimension(){return pair<int,int>(mTheData.RSize(), mTheData.CSize());}

};

//This function is not class member, so need to export separately
RR_DECLSPEC    std::ostream&         operator << (std::ostream& ss, SimulationData& data);
}


#endif

#ifndef rrSimulationDataH
#define rrSimulationDataH
#include <fstream>
#include <sstream>
#include "rrObject.h"
#include "rrStringList.h"
#include "libstruct/lsMatrix.h"
#include "rrExporter.h"
namespace rr
{

using namespace LIB_LA;
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
        void                    SetTimeDataPrecision(const int& prec);
        void                    SetDataPrecision(const int& prec);
        void                    SetColumnNames(const StringList& colNames);
        void                    SetNrOfCols(const int& cols);
        int                     GetNrOfCols();
        int                     GetNrOfRows();
        void                    SetData(const DoubleMatrix& theData);
        bool                    Load(const string& fileName);
        bool                    WriteTo(const string& fileName);

RR_DECLSPEC        friend std::ostream&    operator << (std::ostream& ss, SimulationData& data);
        bool                    Check();    //Check if containst proper data

        double&                 operator() (const unsigned& row, const unsigned& col);
        double                  operator() (const unsigned& row, const unsigned& col) const;
        void                    SetName(const string& name);
        string                  GetName();
        pair<int,int>           Dimension();

};

//This function is not class member, so need to export separately
RR_DECLSPEC    std::ostream&         operator << (std::ostream& ss, SimulationData& data);
}


#endif

#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iomanip>
#include "rrLogger.h"
#include "rrSimulationData.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
    #pragma package(smart_init)
#endif


using namespace std;

namespace rr
{


SimulationData::SimulationData()
:
mTimePrecision(6),
mDataPrecision(16)
{

}

SimulationData::SimulationData(const StringList& colNames, const DoubleMatrix& theData)
:
mColumnNames(colNames),
mTheData(theData)
{

}

void SimulationData::SetName(const string& name)
{
	mName = name;
    mTheData.SetNamePointer(&mName);
}

StringList SimulationData::GetColumnNames()
{
	return mColumnNames;
}

string SimulationData::GetColumnNamesAsString()
{
	return mColumnNames.AsString();
}

void SimulationData::Allocate(const int& cSize, const int& rSize)
{
	mTheData.Allocate(cSize, rSize);
}

//=========== OPERATORS
double& SimulationData::operator() (const unsigned& row, const unsigned& col)
{
	return mTheData(row,col);
}

double SimulationData::operator() (const unsigned& row, const unsigned& col) const
{
	return mTheData(row,col);
}

void SimulationData::SetColumnNames(const StringList& colNames)
{
	mColumnNames = colNames;
    Log(lDebug3)<<"Simulation Data Columns: "<<mColumnNames;
}

void SimulationData::SetNrOfCols(const int& cols)
{
	mTheData.Allocate(1, cols);
}

void SimulationData::SetData(const DoubleMatrix& theData)
{
	mTheData = theData;
    Log(lDebug5)<<"Simulation Data =========== \n"<<mTheData;
    Check();
}

bool SimulationData::Check()
{
    if(mTheData.CSize() != mColumnNames.size())
	{
    	Log(lError)<<"Number of columns in simulation data is not equal to number of columns in column header!";
    	return false;
    }
    return true;
}

bool SimulationData::Load(const string& fileName)
{
	return false;
}

////When comparing to a reference
//bool SimulationData::LoadReference(const string& fileName)
//{
//	return false;
//}

bool SimulationData::WriteTo(const string& fileName)
{
	return false;
}

ofstream& operator << (ofstream& fs, SimulationData& data)
{
	//Check that the dimensions of col header and data is ok
	if(!data.Check())
    {
    	Log(lError)<<"Can't write data..";
        return fs;
    }

	//First create the header
    for(u_int i = 0; i < data.mColumnNames.size(); i++)
    {
		fs<<data.mColumnNames[i];
        if(i < data.mColumnNames.size() - 1)
        {
        	fs << ",";
        }
        else
        {
        	fs << endl;
        }
    }
	//Then the data
    for(u_int row = 0; row < data.mTheData.RSize(); row++)
    {
	    for(u_int col = 0; col < data.mTheData.CSize(); col++)
    	{
        	if(col == 0)
            {
				fs<<setprecision(data.mTimePrecision)<<data.mTheData(row, col);
            }
            else
            {
				fs<<setprecision(data.mDataPrecision)<<data.mTheData(row, col);
            }

            if(col <data.mTheData.CSize() -1)
            {
                fs << ",";
            }
            else
            {
                fs << endl;
            }
        }
    }

	return fs;
}

}//end of namespace

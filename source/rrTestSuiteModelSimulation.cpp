#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <iomanip>
#include <map>
#include <math.h>
#include <sstream>
#include "rrLogger.h"
#include "rrTestSuiteModelSimulation.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------

namespace rr
{

TestSuiteModelSimulation::TestSuiteModelSimulation(const string& dataOutputFolder, const string& modelFilePath, const string& modelFileName)
:
SBMLModelSimulation(dataOutputFolder, dataOutputFolder),
mCurrentCaseNumber(-1),
mNrOfFailingPoints(0)
{
    //make sure the output folder exists..
    mResultData.SetName("ResultData");
    mReferenceData.SetName("ReferenceData");
    mErrorData.SetName("ErrorData");
}

TestSuiteModelSimulation::~TestSuiteModelSimulation()
{}

void TestSuiteModelSimulation::SetCaseNumber(int cNr)
{
    mCurrentCaseNumber = cNr;
}

bool TestSuiteModelSimulation::CopyFilesToOutputFolder()
{
    if(!mModelSettingsFileName.size())
    {
        mModelSettingsFileName = JoinPath(mModelFilePath, GetSettingsFileNameForCase(mCurrentCaseNumber));
    }

    string fName = ExtractFileName(mModelSettingsFileName);
    fName = JoinPath(mDataOutputFolder, fName);
	return CopyFileA(mModelSettingsFileName.c_str(), fName.c_str(), false) == TRUE ? true : false;
}

bool TestSuiteModelSimulation::LoadSettings(const string& settingsFName)
{
    mModelSettingsFileName = (settingsFName);

    if(!mModelSettingsFileName.size())
    {
        mModelSettingsFileName = JoinPath(mModelFilePath, GetSettingsFileNameForCase(mCurrentCaseNumber));
    }
    return SBMLModelSimulation::LoadSettings(mModelSettingsFileName);
}

bool TestSuiteModelSimulation::LoadReferenceData()
{
    //The reference data is located in the folder where the model is located
    string refDataFileName = JoinPath(mModelFilePath, GetReferenceDataFileNameForCase(mCurrentCaseNumber));
    if(!FileExists(refDataFileName))
    {
        Log(lWarning)<<"Could not open reference data file: "<<refDataFileName;
        return false;
    }

    vector<string> lines = GetLinesInFile(refDataFileName);
    if(!lines.size())
    {
        Log(lWarning)<<"This file is empty..";
        return false;
    }

    //Create the data..
    for(int row = 0; row < lines.size(); row++)
    {
           vector<string> recs = SplitString(lines[row], ",");
        if(row == 0) //This is the header
        {
            mReferenceData.SetColumnNames(recs);
            //Assign how many columns the data has
            mReferenceData.Allocate(lines.size() - 1, recs.size());
        }
        else    //This is data
        {
            for(int col = 0; col < mReferenceData.GetNrOfCols(); col++)
            {
                 double val = ToDouble(recs[col]);
                mReferenceData(row - 1,col) = val; //First line is the header..
             }
        }
    }

    return true;
}

bool TestSuiteModelSimulation::Pass()
{
    return mNrOfFailingPoints > 0 ? false : true;
}

int TestSuiteModelSimulation::NrOfFailingPoints()
{
    return mNrOfFailingPoints;
}

double TestSuiteModelSimulation::LargestError()
{
    return mLargestError;
}

bool TestSuiteModelSimulation::CreateErrorData()
{
    mResultData = GetResult();
    //Check that result data and reference data has the same dimensions
    if(mResultData.GetNrOfCols() != mReferenceData.GetNrOfCols() || mResultData.GetNrOfRows() != mReferenceData.GetNrOfRows())
    {
        mNrOfFailingPoints = mResultData.GetNrOfRows();
        return false;
    }

    mErrorData.Allocate(mResultData.GetNrOfRows(), mResultData.GetNrOfCols());
    mLargestError = 0;
    for(int row = 0; row < mResultData.GetNrOfRows(); row++)
    {
        for(int col = 0; col < mResultData.GetNrOfCols(); col++)
        {
            double error = fabsl(mResultData(row, col) - mReferenceData(row,col));
            mErrorData(row, col) = error;

            if(error > mSettings.mAbsolute + mSettings.mRelative*fabs(mReferenceData(row,col)))
            {
                mNrOfFailingPoints++;;
            }

            if(error > mLargestError)
            {
                mLargestError = error;
            }
        }
    }
    return true;
}

bool TestSuiteModelSimulation::SaveAllData()
{
    //Save all data to one file that can be plotted "as one"

    //First save the reference data to a file for comparison to result data
    string refDataFileName = JoinPath(mDataOutputFolder, GetReferenceDataFileNameForCase(mCurrentCaseNumber));
    ofstream fs(refDataFileName.c_str());
    fs<<mReferenceData;
    fs.close();

    string outputAllFileName;
    string dummy;
    CreateTestSuiteFileNameParts(mCurrentCaseNumber, "-result-comparison.csv", dummy, outputAllFileName);
    fs.open(JoinPath(mDataOutputFolder, outputAllFileName).c_str());

    //Check matrices dimension, if they are not equal, bail..?
    if(mResultData.Dimension() != mReferenceData.Dimension() ||
       mResultData.Dimension() != mErrorData.Dimension()        ||
       mErrorData.Dimension()  != mReferenceData.Dimension() )
    {
        Log(lWarning)<<"Data dimensions are not equal, not saving to one file..";
        return false;
    }
    for(int row = 0; row < mResultData.GetNrOfRows(); row++)
    {
        for(int col = 0; col < mReferenceData.GetNrOfCols(); col++)
        {
            if(row == 0)
            {
                if(col == 0)
                {
                    StringList ref_cnames =  mReferenceData.GetColumnNames();
                    ref_cnames.PostFix("_ref");
                    fs << ref_cnames.AsString();
                    fs << ",";
                    StringList res_cnames =  mResultData.GetColumnNames();
                    res_cnames.PostFix("_rr");
                    fs << res_cnames.AsString();
                    fs << ",";
                    StringList err_names = ref_cnames - res_cnames;
                    fs << err_names.AsString();
                }
            }

            //First column is the time...
            if(col == 0)
            {
                fs << endl << setw(10)<<left<<setprecision(6)<< mReferenceData(row, col); // this is time..
            }
            else
            {
                if(row <= mReferenceData.GetNrOfRows())
                {
                    fs << "," << mReferenceData(row, col);
                }
                else
                {
                    fs << "," << " ";
                }
            }
        }

        //Then the simulated data
        for(int col = 0; col < mResultData.GetNrOfCols(); col++)
        {
            //First column is the time...
            if(col == 0)
            {
                fs << "," << setw(10)<<left<<setprecision(6)<< mResultData(row , col);
            }
            else
            {
                fs << "," << mResultData(row, col);
            }
        }

        //Then the error data
        for(int col = 0; col < mErrorData.GetNrOfCols(); col++)
        {
            //First column is the time...
            if(col == 0)
            {
                fs << "," << setw(10)<<left<<setprecision(6)<<mErrorData(row, col); //Becuase row 0 is the header
            }
            else
            {
                fs << "," << mErrorData(row, col);
            }
        }
    }

    return true;
}

string TestSuiteModelSimulation::GetSettingsFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr;
    name<<string("-settings.txt");        //create the "00023" subfolder format
    string theName = name.str();
    return theName;
}

string TestSuiteModelSimulation::GetReferenceDataFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-results.csv";
    return name.str();

}

} //end of namespace



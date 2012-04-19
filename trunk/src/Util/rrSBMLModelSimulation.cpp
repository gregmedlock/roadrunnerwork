#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iomanip>
#include <map>
#include "rrLogger.h"
#include "rrSBMLModelSimulation.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------

#if defined(__CODEGEARC__)
    #pragma package(smart_init)
#endif

namespace rr
{

SBMLModelSimulation::SBMLModelSimulation(const string& dataOutputFolder, const string& modelFilePath, const string& modelFileName)
:
mModelFilePath(modelFilePath),
mModelFileName(modelFileName),
mDataOutputFolder(dataOutputFolder),
mCurrentCaseNumber(-1),
mCompileIfDllExists(true),
mSimulationError(0)			//No error if not calculated..
{
	//make sure the output folder exists..
	mReferenceData.SetName("ReferenceData");
	mResultData.SetName("ResultData");
    mErrorData.SetName("ErrorData");
}

SBMLModelSimulation::~SBMLModelSimulation()
{}

bool SBMLModelSimulation::UseEngine(RoadRunner* engine)
{
	mEngine = engine;
    if(mEngine)
    {
    	mEngine->PartOfSimulation(this);	//Road runner then gets access to data oupt folders etc..
    }
    return true;
}

bool SBMLModelSimulation::LoadSettings(const string& settingsFName)
{
	string fName(settingsFName);

	if(!fName.size())
    {
       	//string settings file name is based on the case number
        fName = GetSettingsFileNameForCase(mCurrentCaseNumber);

    	//Try to read from a file within folder where the model is
        fName = JoinPath(mModelFilePath, fName);
    }

    if(!FileExists(fName))
    {
        mSettings.mStartTime 	= 0;
        mSettings.mDuration 	= 5;
        mSettings.mSteps	 	= 50;
        mSettings.mAbsolute 	= 1.e-7;
        mSettings.mRelative 	= 1.e-4;
        mSettings.mEndTime 		= mSettings.mStartTime + mSettings.mDuration;
    }
    else
    {
        map<string, string> settings;
        map<string, string>::iterator it;
        //Read each line in the settings file
        vector<string> lines = GetLinesInFile(fName);
        for(int i = 0; i < lines.size(); i++)
        {
            vector<string> line = SplitString(lines[i], ":");
            if(line.size() == 2)
            {
                settings.insert( pair<string, string>(line[0], line[1]));
            }
            else
            {
                Log(lDebug)<<"Badly formatted, or empty line in settings file:"<<lines[i];
            }
        }

        Log(lDebug)<<"Settings File =============";
        for (it = settings.begin() ; it != settings.end(); it++ )
        {
            Log(lDebug) << (*it).first << " => " << (*it).second;
        }
        Log(lDebug)<<"===========================";

        //Assign values
        it = settings.find("start");
        mSettings.mStartTime = (it != settings.end()) ?  ToDouble((*it).second) : 0;

        it = settings.find("duration");
        mSettings.mDuration = (it != settings.end()) ?  ToDouble((*it).second) : 0;

        it = settings.find("steps");
        mSettings.mSteps = (it != settings.end()) ?  ToInt((*it).second) : 50;

        it = settings.find("absolute");
        mSettings.mAbsolute = (it != settings.end()) ?  ToDouble((*it).second) : 1.e-7;

        it = settings.find("relative");
        mSettings.mRelative = (it != settings.end()) ?  ToDouble((*it).second) : 1.e-4;

        mSettings.mEndTime = mSettings.mStartTime + mSettings.mDuration;
    }

    if(mEngine)
    {
        mEngine->UseSimulationSettings(mSettings);
    }

	return true;
}

bool SBMLModelSimulation::LoadReferenceData()
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
        else	//This is data
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

bool SBMLModelSimulation::CreateErrorData()
{
	//Check tht result data and reference data has the same dimensions
    if(mResultData.GetNrOfCols() != mReferenceData.GetNrOfCols() || mResultData.GetNrOfRows() != mReferenceData.GetNrOfRows())
    {
    	return false;
    }

    mErrorData.Allocate(mResultData.GetNrOfRows(), mResultData.GetNrOfCols());


    for(int row = 0; row < mResultData.GetNrOfRows(); row++)
    {
    	for(int col = 0; col < mResultData.GetNrOfCols(); col++)
        {
			double error = fabs(mResultData(row, col) - mReferenceData(row,col));
            mErrorData(row, col) = error;

            if(error > mSimulationError)
            {
				mSimulationError = error;
            }
        }
    }
	return true;
}

bool SBMLModelSimulation::SaveAllData()
{
	//Save all data to one file that can be plotted "as one"

	//First save the reference data to a file for comparison to result data
    string refDataFileName = JoinPath(mDataOutputFolder, GetReferenceDataFileNameForCase(mCurrentCaseNumber));
    ofstream fs(refDataFileName.c_str());
    fs<<mReferenceData;
    fs.close();

	string outputAllFileName;
    string dummy;
    CreateTestSuiteFileNameParts(mCurrentCaseNumber, "-result-comparison.dat", dummy, outputAllFileName);
    fs.open(JoinPath(mDataOutputFolder, outputAllFileName).c_str());

    //Check matrices dimension, if they are not equal, bail..?
    if(mResultData.Dimension() != mReferenceData.Dimension() ||
       mResultData.Dimension() != mErrorData.Dimension()   	 ||
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

                    StringList res_cnames =  mResultData.GetColumnNames();
                    res_cnames.PostFix("_rr");
                    fs << res_cnames.AsString();

                    StringList err_names = ref_cnames - res_cnames;
                    fs << err_names.AsString();
                }
            }
            else
            {
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
        }

        //Then the simulated data
    	for(int col = 0; col < mResultData.GetNrOfCols(); col++)
        {
			if(row != 0)
            {            	
            	//First column is the time...
                if(col == 0)
                {
            		fs << "," << setw(10)<<left<<setprecision(6)<< mResultData(row, col); // this is time..
                }
                else
                {
                    fs << "," << mResultData(row, col);
                }
            }
        }

        //Then the error data
    	for(int col = 0; col < mErrorData.GetNrOfCols(); col++)
        {
			if(row != 0)
            {
            	//First column is the time...
                if(col == 0)
                {
            		fs << "," << setw(10)<<left<<setprecision(6)<<mErrorData(row, col); // this is time..
                }
                else
                {
                    fs << "," << mErrorData(row, col);
                }
            }
        }
    }

	return true;
}

bool SBMLModelSimulation::LoadModel()
{
	return (mEngine) ? mEngine->loadSBMLFromFile(GetModelsFullFilePath()) : false;
}

bool SBMLModelSimulation::GenerateAndCompileModel()
{
	if(!mEngine)
    {
    	return false;
    }
    return mEngine->GenerateAndCompileModel();
}

bool SBMLModelSimulation::Run()
{
	if(!mEngine)
    {
    	return false;
    }

	return mEngine->Simulate();
}

bool SBMLModelSimulation::SaveResult()
{
	string resultFileName(JoinPath(mDataOutputFolder, mModelFileName));
	resultFileName = ChangeFileExtensionTo(resultFileName, "result.dat");

	Log(lInfo)<<"Saving result to file: "<<resultFileName;
    mResultData = mEngine->GetSimulationResult();

    ofstream fs(resultFileName.c_str());
    fs << mResultData;
    fs.close();
    return true;

}

string SBMLModelSimulation::GetSettingsFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-settings.txt";		//create the "00023" subfolder format

	return name.str();
}

string SBMLModelSimulation::GetReferenceDataFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-results.csv";
	return name.str();

}
double SBMLModelSimulation::GetSimulationError()
{
	return mSimulationError;
}
} //end of namespace



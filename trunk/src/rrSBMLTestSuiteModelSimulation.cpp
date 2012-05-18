#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <iomanip>
#include <map>
#include "rrLogger.h"
#include "rrSBMLTestSuiteModelSimulation.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------

#if defined(__CODEGEARC__)
    #pragma package(smart_init)
#endif

namespace rr
{

SBMLTestSuiteModelSimulation::SBMLTestSuiteModelSimulation(const string& dataOutputFolder, const string& modelFilePath, const string& modelFileName)
:
mModelFilePath(modelFilePath),
mModelFileName(modelFileName),
mDataOutputFolder(dataOutputFolder),
mCurrentCaseNumber(-1),
mCompileIfDllExists(true),
mSimulationError(0)            //No error if not calculated..
{
    //make sure the output folder exists..
    mReferenceData.SetName("ReferenceData");
    mResultData.SetName("ResultData");
    mErrorData.SetName("ErrorData");
}

SBMLTestSuiteModelSimulation::~SBMLTestSuiteModelSimulation()
{}

void SBMLTestSuiteModelSimulation::SetCaseNumber(int cNr)
{
    mCurrentCaseNumber = cNr;
}

bool SBMLTestSuiteModelSimulation::SetModelFilePath(const string& path)
{
    mModelFilePath = path;
    return true;
}

bool SBMLTestSuiteModelSimulation::SetModelFileName(const string& name)
{
    if(ExtractFilePath(name).size() > 0)
    {
        mModelFilePath = ExtractFilePath(name);
    }

    mModelFileName = ExtractFileName(name);

    if(!FileExists(JoinPath(mModelFilePath, mModelFileName)))
    {
        Log(lError)<<"The file: "<<JoinPath(mModelFilePath, mModelFileName)<<" don't exist.";
        return false;
    }

    return true;
}

string SBMLTestSuiteModelSimulation::GetTempDataFolder()
{
    return mTempFolder;
}

bool SBMLTestSuiteModelSimulation::SetDataOutputFolder(const string& name)
{
    mDataOutputFolder = name;
    return true;
}

string  SBMLTestSuiteModelSimulation::GetModelsFullFilePath()
{
    return JoinPath(mModelFilePath, mModelFileName);
}

string  SBMLTestSuiteModelSimulation::GetDataOutputFolder()
{
    return mDataOutputFolder;
}

void    SBMLTestSuiteModelSimulation::CompileIfDllExists(const bool& doIt)
{
    mCompileIfDllExists = doIt;
}

bool    SBMLTestSuiteModelSimulation::CompileIfDllExists()
{
    return mCompileIfDllExists;
}

bool SBMLTestSuiteModelSimulation::UseEngine(RoadRunner* engine)
{
    mEngine = engine;
    if(mEngine)
    {
        mEngine->PartOfSimulation(this);    //Road runner then gets access to data oupt folders etc..
    }
    return true;
}

bool SBMLTestSuiteModelSimulation::GenerateModelCode()
{
    if(!mEngine)
    {
        return false;
    }
    return mEngine->GenerateModelCode();
}

bool SBMLTestSuiteModelSimulation::CompileModel()
{
    if(!mEngine)
    {
        return false;
    }

    return mEngine->CompileCurrentModel();
}

bool SBMLTestSuiteModelSimulation::LoadSettings(const string& settingsFName)
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
        mSettings.mStartTime     = 0;
        mSettings.mDuration     = 5;
        mSettings.mSteps         = 50;
        mSettings.mAbsolute     = 1.e-7;
        mSettings.mRelative     = 1.e-4;
        mSettings.mEndTime         = mSettings.mStartTime + mSettings.mDuration;
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
                Log(lDebug2)<<"Empty line in settings file: "<<lines[i];
            }
        }

        Log(lDebug3)<<"Settings File =============";
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

        it = settings.find("variables");
        if(it != settings.end())
        {
            vector<string> vars = SplitString((*it).second, ",");
            for(int i=0; i < vars.size(); i++)
            {
                mSettings.mVariables.push_back(Trim(vars[i]));
            }
        }

        it = settings.find("amount");
        if(it != settings.end())
        {
            vector<string> vars = SplitString((*it).second, ",");
            for(int i=0; i < vars.size(); i++)
            {
                string rec = Trim(vars[i]);
                if(rec.size())
                {
                    mSettings.mAmount.push_back(rec);
                }
            }
        }

        it = settings.find("concentration");
        if(it != settings.end())
        {
            vector<string> vars = SplitString((*it).second, ",");
            for(int i=0; i < vars.size(); i++)
            {
                string rec = Trim(vars[i]);
                if(rec.size())
                {
                    mSettings.mConcentration.push_back(rec);
                }
            }
        }
    }

    if(mEngine)
    {
        mEngine->UseSimulationSettings(mSettings);
        mEngine->CreateSelectionList();    //This one creates the list of what we will look at in the result
    }

    return true;
}

bool SBMLTestSuiteModelSimulation::LoadReferenceData()
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

bool SBMLTestSuiteModelSimulation::CreateErrorData()
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
            double error = fabsl(mResultData(row, col) - mReferenceData(row,col));
            mErrorData(row, col) = error;

            if(error > mSimulationError)
            {
                mSimulationError = error;
            }
        }
    }
    return true;
}

bool SBMLTestSuiteModelSimulation::SaveAllData()
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

                    StringList res_cnames =  mResultData.GetColumnNames();
                    res_cnames.PostFix("_rr");
                    fs << res_cnames.AsString();

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

bool SBMLTestSuiteModelSimulation::LoadSBMLFromFile()                    //Use current file information to load sbml from file
{
    if(!mEngine)
    {
        return false;
    }
    bool val = mEngine->LoadSBMLFromFile(GetModelsFullFilePath());
    return val;
}

bool SBMLTestSuiteModelSimulation::SaveModelAsXML(const string& folder)
{
    if(!mEngine)
    {
        return false;
    }
    string fName = JoinPath(folder, mModelFileName);
    fName = ChangeFileExtensionTo(fName, "xml");

    fstream fs(fName.c_str(), fstream::out);

    if(!fs)
    {
        Log(lError)<<"Failed writing sbml to file "<< fName;
        return false;
    }
    fs<<mEngine->getSBML();
    fs.close();
    return true;
}

bool SBMLTestSuiteModelSimulation::CreateModel()
{
    if(!mEngine)
    {
        return false;
    }

    return mEngine->CreateModel();
}

bool SBMLTestSuiteModelSimulation::InitializeModel()
{
    if(!mEngine)
    {
        return false;
    }

    return mEngine->InitializeModel();
}

bool SBMLTestSuiteModelSimulation::GenerateAndCompileModel()
{
    if(!mEngine)
    {
        return false;
    }
    return mEngine->GenerateAndCompileModel();
}

bool SBMLTestSuiteModelSimulation::Run()
{
    if(!mEngine)
    {
        return false;
    }

    return mEngine->Simulate();
}

bool SBMLTestSuiteModelSimulation::SaveResult()
{
    string resultFileName(JoinPath(mDataOutputFolder, mModelFileName));
    resultFileName = ChangeFileExtensionTo(resultFileName, ".csv");

    Log(lInfo)<<"Saving result to file: "<<resultFileName;
    mResultData = mEngine->GetSimulationResult();

    ofstream fs(resultFileName.c_str());
    fs << mResultData;
    fs.close();
    return true;
}

string SBMLTestSuiteModelSimulation::GetSettingsFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-settings.txt";        //create the "00023" subfolder format

    return name.str();
}

string SBMLTestSuiteModelSimulation::GetReferenceDataFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-results.csv";
    return name.str();

}
double SBMLTestSuiteModelSimulation::GetSimulationError()
{
    return mSimulationError;
}
} //end of namespace



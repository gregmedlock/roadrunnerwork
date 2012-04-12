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
mCurrentCaseNumber(-1)
{
	//make sure the output folder exists..

}

SBMLModelSimulation::~SBMLModelSimulation()
{

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
                Log(lWarning)<<"Badly formatted line in settings file:"<<lines[i];
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
        mSettings.mSteps = (it != settings.end()) ?  ToDouble((*it).second) : 50;

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

	return true;
}

bool SBMLModelSimulation::CreateErrorData()
{
	return true;
}

bool SBMLModelSimulation::SaveAllData()
{
	return true;
}

bool SBMLModelSimulation::LoadModel()
{
	return (mEngine) ? mEngine->loadSBMLFromFile(GetModelsFullFilePath()) : false;
}

bool SBMLModelSimulation::CompileModel()
{
	if(!mEngine)
    {
    	return false;
    }
    return mEngine->CompileModel();
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

	Log(lInfo)<<"Saving simulation result to file: "<<resultFileName;
    SimulationData result = mEngine->GetSimulationResult();

    ofstream fs(resultFileName.c_str());
    fs << result;
    fs.close();
    return true;

}

string SBMLModelSimulation::GetSettingsFileNameForCase(int caseNr)
{
    stringstream name;
    name<<setfill('0')<<setw(5)<<caseNr<<"-settings.txt";		//create the "00023" subfolder format

	return name.str();
}

} //end of namespace



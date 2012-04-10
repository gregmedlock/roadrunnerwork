#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrSBMLModelSimulation.h"
//---------------------------------------------------------------------------

#if defined(__CODEGEARC__)
    #pragma package(smart_init)
#endif

namespace rr
{

SBMLModelSimulation::SBMLModelSimulation(const string& modelFilePath, const string& modelFileName)
:
mModelFilePath(modelFilePath),
mModelFileName(modelFileName)
{}

bool SBMLModelSimulation::LoadSettings(const string& fName)
{
	if(fName.size())
    {

    }
    else
    {
    	//Try to read from a file within folder where the model is
        mSettings.mStartTime = 0;
        mSettings.mDuration = 5;
        mSettings.mSteps = 50;
        mSettings.mEndTime = mSettings.mStartTime + mSettings.mDuration;
        if(mEngine)
        {
        	mEngine->UseSimulationSettings(mSettings);
        }

    }
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
	string resultFileName(GetModelsFullFilePath());
	resultFileName = ChangeFileExtensionTo(resultFileName, "result.dat");

	Log(lInfo)<<"Saving simulat ion result to file: "<<resultFileName;
    SimulationData result = mEngine->GetSimulationResult();

    ofstream fs(resultFileName.c_str());
    fs << result;
    fs.close();
    return true;

}

} //end of namespace



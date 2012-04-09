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
{

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

    DoubleMatrix result = mEngine->GetSimulationResult();

    return true;

}

} //end of namespace



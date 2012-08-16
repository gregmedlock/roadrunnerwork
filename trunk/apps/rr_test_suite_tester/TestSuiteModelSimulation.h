#ifndef rrSBMLTestSuiteModelSimulationH
#define rrSBMLTestSuiteModelSimulationH
//---------------------------------------------------------------------------
#include <string>
#include "rrSBMLModelSimulation.h"
#include "rrStringUtils.h"
#include "rrRoadRunner.h"
#include "rrSimulationSettings.h"
#include "rrSimulationData.h"

namespace rr
{

class TestSuiteModelSimulation : public SBMLModelSimulation
{
    protected:
        int                     mCurrentCaseNumber;                     //If simulating test suite cases...
        string                  mModelSettingsFileName;
        SimulationData          mResultData;
        SimulationData          mReferenceData;
        SimulationData          mErrorData;
        string                  GetSettingsFileNameForCase(int sim_case);
        string                  GetReferenceDataFileNameForCase(int caseNr);
        double                  mSimulationError;

    public:
                                TestSuiteModelSimulation(const string& dataOutputFolder = "", const string& modelFilePath = "", const string& modelFileName = "");
                               ~TestSuiteModelSimulation();
        void                    SetCaseNumber(int cNr);
        bool                    LoadReferenceData();
        bool                    CreateErrorData();
        bool                    SaveAllData();
        double                  GetSimulationError();
        bool                    LoadSettings(const string& fName = "");
        bool                    CopyFilesToOutputFolder();

};

}



#endif

#ifndef rrSBMLModelSimulationH
#define rrSBMLModelSimulationH
//---------------------------------------------------------------------------
#include <string>
#include "rrObject.h"
#include "rrStringUtils.h"
#include "rrRoadRunner.h"
#include "rrSimulationSettings.h"
#include "rrSimulationData.h"

namespace rr
{

class RR_DECLSPEC SBMLModelSimulation : public rrObject
{
    protected:
        int                     mCurrentCaseNumber;    //If simulating test suite cases...
        string                  mModelFileName;
        string                  mModelFilePath;
        string                  mModelSettingsFileName;
        string                  mSimulationLogFile;
        string                  mDataOutputFolder;
        string                  mTempFolder;
        RoadRunner              *mEngine;
        SimulationSettings      mSettings;
        SimulationData          mResultData;
        SimulationData          mReferenceData;
        SimulationData          mErrorData;
        string                  GetSettingsFileNameForCase(int sim_case);
        string                  GetReferenceDataFileNameForCase(int caseNr);
        bool                    mCompileIfDllExists;
        double                  mSimulationError;

    public:
                                SBMLModelSimulation(const string& dataOutputFolder = "", const string& modelFilePath = "", const string& modelFileName = "");
                               ~SBMLModelSimulation();
        void                    SetCaseNumber(int cNr);
        bool                    SetModelFilePath(const string& path);
        bool                    SetModelFileName(const string& name);
        bool                    SetDataOutputFolder(const string& name);
        string                  GetModelsFullFilePath();
        string                  GetDataOutputFolder();
        bool                    UseEngine(RoadRunner* engine);
        bool                    LoadSBMLFromFile();                    //Use current file information to load sbml from file
        bool                    GenerateModelCode();
        bool                    CreateModel();
        bool                    CompileModel();
        bool                    InitializeModel();
        bool                    GenerateAndCompileModel();
        bool                    Run();
        bool                    SaveResult();
        bool                    LoadReferenceData();
        bool                    CreateErrorData();
        bool                    LoadSettings(const string& fName = "");
        bool                    SaveAllData();
        void                    CompileIfDllExists(const bool& doIt);
        bool                    CompileIfDllExists();
        double                  GetSimulationError();
        bool                    SaveModelAsXML(const string& folder);

};

}



#endif

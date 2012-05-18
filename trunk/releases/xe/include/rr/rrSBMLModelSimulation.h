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
        string                  mModelFileName;
        string                  mModelFilePath;
        string                  mModelSettingsFileName;
        string                  mSimulationLogFile;
        string                  mDataOutputFolder;
        string                  mTempDataFolder;
        RoadRunner             *mEngine;
        SimulationSettings      mSettings;
        SimulationData          mResultData;
        bool                    mCompileIfDllExists;

    public:
                                SBMLModelSimulation(const string& dataOutputFolder = "", const string& tempDataFilePath = "");
                               ~SBMLModelSimulation();
        bool                    SetModelFilePath(const string& path);
        bool                    SetModelFileName(const string& name);
        bool                    SetDataOutputFolder(const string& name);
        string                  GetModelsFullFilePath();
        string                  GetDataOutputFolder();
        string                  GetTempDataFolder();
        bool                    UseEngine(RoadRunner* engine);
        bool                    LoadSBMLFromFile();                    //Use current file information to load sbml from file
        bool                    GenerateModelCode();
        bool                    CreateModel();
        bool                    CompileModel();
        bool                    InitializeModel();
        bool                    GenerateAndCompileModel();
        bool                    Run();
        bool                    SaveResult();
        bool                    LoadSettings(const string& fName = "");
        void                    CompileIfDllExists(const bool& doIt);
        bool                    CompileIfDllExists();
        bool                    SaveModelAsXML(const string& folder);
};

}



#endif

#ifndef rrSBMLModelSimulationH
#define rrSBMLModelSimulationH
//---------------------------------------------------------------------------
#include "rrObject.h"
#include "rrStringUtils.h"
#include "rrRoadRunner.h"
#include "rrSimulationSettings.h"
namespace rr
{

class RR_DECLSPEC SBMLModelSimulation : public rrObject
{
	protected:
		string 		           	mModelFileName;
        string 		           	mModelFilePath;

        string 		           	mModelSettingsFileName;

        string 		           	mModelOutputFilePath;
        string 		           	mSimulationLogFile;
        RoadRunner	           *mEngine;
        SimulationSettings		mSettings;

    public:
						        SBMLModelSimulation(const string& modelFilePath = "", const string& modelFileName = "");
    	bool			        SetModelFilePath(const string& path){mModelFilePath = path; return true;}
    	bool			        SetModelFileName(const string& name){mModelFileName = name; return true;}
        string 			        GetModelsFullFilePath(){return JoinPath(mModelFilePath, mModelFileName);}
        bool			        UseEngine(RoadRunner* engine){mEngine = engine;}
        bool			        LoadModel();
		bool			        CompileModel();
        bool			        Run();
        bool			        SaveResult();
        bool			        LoadSettings(const string& fName = "");

};

}



#endif

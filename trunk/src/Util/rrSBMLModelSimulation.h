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
		int						mCurrentCaseNumber;	//If simulating test suite cases...
		string 		           	mModelFileName;
        string 		           	mModelFilePath;
        string 		           	mModelSettingsFileName;
        string 		           	mSimulationLogFile;
        string					mDataOutputFolder;
		string					mTempFolder;
        RoadRunner	           *mEngine;
        SimulationSettings		mSettings;
        SimulationData			mResultData;
        SimulationData			mReferenceData;
        SimulationData			mErrorData;
		string				    GetSettingsFileNameForCase(int sim_case);
		string 					GetReferenceDataFileNameForCase(int caseNr);


    public:
						        SBMLModelSimulation(const string& dataOutputFolder = "", const string& modelFilePath = "", const string& modelFileName = "");
							   ~SBMLModelSimulation();
		void					SetCaseNumber(int cNr){mCurrentCaseNumber = cNr;}
    	bool			        SetModelFilePath(const string& path){mModelFilePath = path; return true;}
    	bool			        SetModelFileName(const string& name){mModelFileName = name; return true;}
    	bool			        SetDataOutputFolder(const string& name){mDataOutputFolder = name; return true;}
        string 			        GetModelsFullFilePath(){return JoinPath(mModelFilePath, mModelFileName);}
        bool			        UseEngine(RoadRunner* engine){mEngine = engine; return true;}
        bool			        LoadModel();
		bool			        CompileModel();
        bool			        Run();
		bool			        SaveResult();
		bool			        LoadReferenceData();
        bool					CreateErrorData();
        bool			        LoadSettings(const string& fName = "");
        bool					SaveAllData();

};

}



#endif
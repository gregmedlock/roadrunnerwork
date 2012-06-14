#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
//---------------------------------------------------------------------------
#include <windows.h>
#include "rrRoadRunner.h"
#include "rrLogger.h"           //Might be useful for debugging later on
#include "rr_c_api.h"
#include "rr_c_api_support.h"   //Support functions, not exposed as api functions and or data
//---------------------------------------------------------------------------
using namespace rr;
using namespace rr_c_api;
namespace rr_c_api
{
static  rr::RoadRunner*     gRRHandle       = NULL;
char*                       gLastError      = NULL;
}

char* __stdcall getBuildDate(void)
{
    char* date = new char[strlen(__DATE__) + 1];
    strcpy(date, __DATE__);
    return date;
}

RRHandle __stdcall getRRInstance()
{
    if(!gRRHandle)
    {
        gRRHandle = new rr::RoadRunner();
    }
    return gRRHandle;
}

char* __stdcall getCopyright()
{
    char* text = NULL;
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        text = new char[strlen(ALLOCATE_API_ERROR_MSG) + 1];
        strcpy(text, ALLOCATE_API_ERROR_MSG);
    }
    else
    {
        text = new char[gRRHandle->getCopyright().size() + 1];
        strcpy(text, gRRHandle->getCopyright().c_str());
    }
    return text;
}

bool __stdcall setTempFolder(const char* folder)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    return gRRHandle->SetTempFileFolder(folder);
}

bool __stdcall loadSBMLFromFile(const char* sbmlFileName)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    if(!gRRHandle->loadSBMLFromFile(sbmlFileName))
    {
        SetAPIError("Failed to load SBML semantics");
        return false;
    }
    return true;
}

bool __stdcall loadSBML(const char* sbml)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    if(!gRRHandle->loadSBML(sbml))
    {
        SetAPIError("Failed to load SBML semantics");
        return false;
    }
    return true;
}

bool __stdcall setTimeStart(double timeStart)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    gRRHandle->setTimeStart(timeStart);
    return true;
}

bool __stdcall setTimeEnd(double timeEnd)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    gRRHandle->setTimeEnd(timeEnd);
    return true;
}

bool __stdcall setNumPoints(int nrPoints)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    gRRHandle->setNumPoints(nrPoints);
    return true;
}

bool  __stdcall setSelectionList(const char* list)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    gRRHandle->setSelectionList(list);
    return true;
}

RRResultHandle __stdcall simulate(void)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    if(!gRRHandle->Simulate())
    {
        return NULL;
    }

    SimulationData result = gRRHandle->GetSimulationResult();

    //Extract the data and return struct..
    RRResult* aResult  = new RRResult;
    aResult->ColumnHeaders = new char*[result.GetNrOfCols()];
    for(int i = 0; i < result.GetNrOfCols(); i++)
    {
        aResult->ColumnHeaders[i] = new char(32);
        strcpy(aResult->ColumnHeaders[i], result.GetColumnNames()[i].c_str());
    }

    aResult->RSize = result.GetNrOfRows();
    aResult->CSize = result.GetNrOfCols();
    int size = aResult->RSize*aResult->CSize;
    aResult->Data = new double[size];

    int index = 0;
    //The data layout is simple row after row, in one single long row...
    for(int row = 0; row < aResult->RSize; row++)
    {
        for(int col = 0; col < aResult->CSize; col++)
        {
            aResult->Data[index++] = result(row, col);
        }
    }
    return aResult;
}

RRStringListHandle __stdcall getReactionNames(void)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return NULL;
    }

    StringList rNames = gRRHandle->getReactionNames();

    if(!rNames.Count())
    {
        return NULL;
    }

    RRStringListHandle list = new RRStringList;
    list->Count = rNames.size();
    list->String = new char*[list->Count];

    for(int i = 0; i < list->Count; i++)
    {
        list->String[i] = new char[rNames[i].size()];
        strcpy(list->String[i], rNames[i].c_str());
    }

    return list;
}

double __stdcall getValue(const char* speciesID)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return 0;
    }
    return gRRHandle->getValue(speciesID);
}

bool __stdcall setValue(const char* speciesID, const double& val)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    return gRRHandle->setValue(speciesID, val);
}

RRDataMatrixHandle __stdcall getStoichiometryMatrix(void)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return NULL;
    }

    LIB_LA::DoubleMatrix tempMat = gRRHandle->getStoichiometryMatrix();

    RRDataMatrixHandle matrix = new RRDataMatrix;
    matrix->RSize = tempMat.RSize();
    matrix->CSize = tempMat.CSize();
    matrix->Data =  new double[tempMat.RSize()*tempMat.CSize()];

    int index = 0;
    for(rr::u_int row = 0; row < tempMat.RSize(); row++)
    {
        for(rr::u_int col = 0; col < tempMat.CSize(); col++)
        {
            matrix->Data[index++] = tempMat(row,col);
        }
    }

    return matrix;
}

void __stdcall printMatrix(RRDataMatrixHandle matrixHandle)
{
    gLog.SetCutOffLogLevel(lDebug2);

    if(!matrixHandle)
    {
        Log(lInfo)<<"Null matrix in printMatrix...";
        return;
    }

    RRDataMatrix& mat = *matrixHandle;
    stringstream ss;
    ss<<"matrix dimension: "<<mat.RSize<<"x"<<mat.CSize<<" --\n";

    int index = 0;
    for(int row = 0; row < mat.RSize; row++)
    {
        for(int col = 0; col < mat.CSize; col++)
        {
            ss<<mat.Data[index++];
            if(col < mat.CSize + 1)
            {
                ss<<"\t";
            }
        }
        ss<<endl;
    }
    cout<<ss.str();
}

C_DECL_SPEC bool __stdcall hasError()
{
    return (gLastError != NULL) ? true : false;
}

char* __stdcall getLastError()
{
    return gLastError;
}

bool __stdcall reset()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    gRRHandle->reset();
    return true;
}

int   __stdcall getNumberOfReactions()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return -1;
    }
    return gRRHandle->getNumberOfReactions();
}

double __stdcall getReactionRate(int rateNr)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return -1;
    }
    return gRRHandle->getReactionRate(rateNr);
}

int __stdcall getNumberOfBoundarySpecies()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return -1;
    }
    return gRRHandle->getNumberOfBoundarySpecies();
}

RRStringListHandle __stdcall getBoundarySpeciesNames()          // <- treat char* as you treat it in setSelectionList (char *)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return NULL;
    }

    StringList bnames = gRRHandle->getBoundarySpeciesNames();
 
    if(!bNames.Count())
    {
        return NULL;
    }

    RRStringListHandle list = new RRStringList;
    list->Count = bNames.size();
    list->String = new char*[list->Count];

    for(int i = 0; i < list->Count; i++)
    {
        list->String[i] = new char[bNames[i].size()];
        strcpy(list->String[i], bNames[i].c_str());
    }

    return list;       
}

int __stdcall getNumberOfFloatingSpecies()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    return gRRHandle->getNumberOfFloatingSpecies();
}

char* __stdcall getFloatingSpeciesNames()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    StringList names = gRRHandle->getFloatingSpeciesNames();
    string namesTemp = names.AsString();
    char* nameList = new char[namesTemp.size() + 1];
    strcpy(nameList, namesTemp.c_str());
    return nameList;
}


int __stdcall getNumberOfGlobalParameters()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    return gRRHandle->getNumberOfGlobalParameters();
}

char* __stdcall getGlobalParameterNames()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    StringList names = gRRHandle->getGlobalParameterNames();
    string namesTemp = names.AsString();
    char* nameList = new char[namesTemp.size() + 1];
    strcpy(nameList, namesTemp.c_str());
    return nameList;
}

bool __stdcall setInitialConditions(RRDoubleVector* vec)     // <- might be called changeInitialConditions in roadRunner
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    vector<double> aVec;
    CopyRRVector(vec, aVec);
    gRRHandle->changeInitialConditions(aVec);
    return true;

}

double __stdcall oneStep(const double& currentTime, const double& stepSize)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    return gRRHandle->oneStep(currentTime, stepSize);
}

RRSymbolListsHandle __stdcall getAvailableSymbols()              // <- You'll have to decide what type to return
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return NULL;
    }

    StringListContainer slSymbols = gRRHandle->getAvailableSymbols();

    RRSymbolListsHandle symbols = new RRSymbolLists;
    symbols->NumberOfLists  = slSymbols.Count();
    symbols->List           = new RRStringList[slSymbols.Count()];

    //Allocate and fill out lists
    for(int listNr = 0; listNr < slSymbols.Count(); listNr++)
    {
        StringList aList = slSymbols[listNr];
        symbols->List[listNr].Count = aList.Count();
        symbols->List[listNr].Label = new char[aList.mLabel.size() + 1];
        strcpy(symbols->List[listNr].Label, aList.mLabel.c_str());

        if(aList.Count())
        {
            symbols->List[listNr].String = new char*[aList.Count()];
            for(int itemNr = 0; itemNr < aList.Count(); itemNr++)
            {
                symbols->List[listNr].String[itemNr] = new char[aList[itemNr].size() + 1];
                strcpy(symbols->List[listNr].String[itemNr], aList[itemNr].c_str());
            }
        }
    }
    return symbols;
}

double __stdcall steadyState()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    return gRRHandle->steadyState();
}

RRDoubleVectorHandle __stdcall computeSteadyStateValues()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
    vector<double> vec =  gRRHandle->computeSteadyStateValues();

    RRDoubleVector* aVec = CreateRRDoubleVecFrom(vec);
    return aVec;
}

bool __stdcall setSteadyStateSelectionList(char* list)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

    StringList aList(list, " ,");
    gRRHandle->setSteadyStateSelectionList(aList);
    return true;
}

//Free Functions
void __stdcall freeRRInstance(RRHandle handle)
{
//    rr::RoadRunner* test = (rr::RoadRunner*)(handle);

    //We don't really care about the handle.
    //Delete the roadrunner instance and if succesful, set the handle to
    //NULL

//    if(test == gRRHandle)
//    {
        delete gRRHandle;
        gRRHandle = NULL;
        handle = NULL;
//    }
}

bool __stdcall freeRRDataMatrix(RRDataMatrixHandle matrix)
{
    if(matrix)
    {
        delete [] (matrix->Data);
        delete matrix;
    }
    return true;
}

bool __stdcall freeRRResult(RRResultHandle handle)
{
    delete handle;
    return true;
}

bool __stdcall freeText(char* text)
{
    if(text != ALLOCATE_API_ERROR_MSG)
    {
        delete [] text;
    }
    return true;
}

bool __stdcall freeStringList(RRStringListHandle sl)
{
    delete sl;
    return true;
}

//============================================================================
//#pragma argsused
//int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
//{
//    //Intialize the logger
//    LogOutput::mLogToConsole = false;
//    return 1;
//}
//

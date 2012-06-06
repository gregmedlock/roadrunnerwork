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
static rr::RoadRunner*  gRRHandle = NULL;
char*            gLastError = NULL;

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

    if(gRRHandle->Simulate())
    {
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
    else
    {
        return NULL;
    }
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

    RRStringListHandle sl = new RRStringList;
    sl->Count = rNames.size();
    sl->String = new char*[sl->Count];

    for(int i = 0; i < sl->Count; i++)
    {
        sl->String[i] = new char[rNames[i].size()];
        strcpy(sl->String[i], rNames[i].c_str());
    }

    return sl;
}

double __stdcall getValue(const char* speciesID)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return NULL;
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
    for(int row = 0; row < tempMat.RSize(); row++)
    {
        for(int col = 0; col < tempMat.CSize(); col++)
        {
//            double val = tempMat(row,col);
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
        return false;
    }
}

double __stdcall getReactionRate(int)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

int __stdcall getNumberOfBoundarySpecies()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

char* __stdcall getBoundarySpeciesNames()          // <- treat char* as you treat it in setSelectionList (char *)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

int __stdcall getNumberOfFloatingSpecies()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

char* __stdcall getFloatingSpeciesNames()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

int __stdcall getNumberOfGlobalParameterNames()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

char* __stdcall getGlobalParameterNames()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

bool __stdcall setInitialConditions(double[])     // <- might be called changeInitialConditions in roadRunner
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

double __stdcall oneStep (double, double)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }

}

RRSymbolListHandle __stdcall getAvailableSymbols()              // <- You'll have to decide what type to return
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

double __stdcall steadyState()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

RRDoubleVectorHandle __stdcall computeSteadyStateValues()
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

bool __stdcall setSteadyStateSelectionList(char *)
{
    if(!gRRHandle)
    {
        SetAPIError(ALLOCATE_API_ERROR_MSG);
        return false;
    }
}

//Free Functions
void __stdcall freeRRInstance(RRHandle handle)
{
    delete gRRHandle;
//    delete handle;
    handle = NULL;
}

bool __stdcall freeRRDataMatrix(RRDataMatrixHandle matrix)
{
    delete [] (matrix->Data);
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
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
    //Intialize the logger
    LogOutput::mLogToConsole = false;
    return 1;
}


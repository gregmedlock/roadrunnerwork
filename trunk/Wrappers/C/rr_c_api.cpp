#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
//---------------------------------------------------------------------------
#include <windows.h>
#include "rrRoadRunner.h"
#include "rrLogger.h"
#include "rr_c_api.h"
//---------------------------------------------------------------------------

using namespace rr;
rr::RoadRunner *gRRHandle = NULL;
RRResult*       gResult = NULL;
char*           gError = NULL;

//Internal prototypes
void SetError(const string& err);

RRResult::~RRResult()
{
    delete [] Data;
    delete [] ColumnHeaders;
}

char* __stdcall getBuildDate(void)
{
    return __DATE__;
}

RRHandle __stdcall getRRInstance()
{
    if(!gRRHandle)
    {
        gRRHandle = new rr::RoadRunner();
    }
    return gRRHandle;
}

void __stdcall deleteRRInstance(RRHandle handle)
{
    delete handle;
    handle = NULL;
}

char* __stdcall getCopyright()
{
    char* text;
    if(!gRRHandle)
    {
        string msg = "Please allocate a handle to roadrunner API before calling any API function";
        text = new char[msg.size() + 1]; //Where do we free this one.. ->  freeText(....);
        return text;
    }

    text = new char[gRRHandle->getCopyright().size() + 1];
    strcpy(text, gRRHandle->getCopyright().c_str());
    return text;
}

bool __stdcall setTempFolder(const char* folder)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    return gRRHandle->SetTempFileFolder(folder);
}

bool __stdcall loadSBML(const char* sbml)
{
    if(!gRRHandle)
    {
        SetError("Please allocate a handle to roadrunner API before calling any API function");
        return false;
    }

    if(!gRRHandle->loadSBML(sbml))
    {
        SetError("Failed to load SBML semantics");
        return false;
    }
    return true;
}

bool __stdcall setTimeStart(double timeStart)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    gRRHandle->setTimeStart(timeStart);
    return true;
}

bool __stdcall setTimeEnd(double timeEnd)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    gRRHandle->setTimeEnd(timeEnd);
    return true;
}

bool __stdcall setNumPoints(int nrPoints)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    gRRHandle->setNumPoints(nrPoints);
    return true;
}

bool  __stdcall setSelectionList(const char* list)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    gRRHandle->setSelectionList(list);
    return true;
}

RRResultHandle __stdcall simulate(void)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    if(gRRHandle->Simulate())
    {
        SimulationData result = gRRHandle->GetSimulationResult();

        //Extract the data and return struct..
        gResult  = new RRResult;
        gResult->ColumnHeaders = new char*[result.GetNrOfCols()];
        for(int i = 0; i < result.GetNrOfCols(); i++)
        {
            gResult->ColumnHeaders[i] = new char(32);
            strcpy(gResult->ColumnHeaders[i], result.GetColumnNames()[i].c_str());
        }

        gResult->RSize = result.GetNrOfRows();
        gResult->CSize = result.GetNrOfCols();
        int size = gResult->RSize*gResult->CSize;
        gResult->Data = new double[size];

        int index = 0;
        //The data layout is simple row after row, in one single long row...
        for(int row = 0; row < gResult->RSize; row++)
        {
            for(int col = 0; col < gResult->CSize; col++)
            {
                gResult->Data[index++] = result(row, col);
            }
        }
        return gResult;
    }
    else
    {
        return NULL;
    }
}

bool __stdcall freeRRResult()
{
    delete gResult;
    return true;
}

RRStringListHandle __stdcall getReactionNames(void)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
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

bool __stdcall freeStringList(RRStringListHandle sl)
{
    delete sl;
    return true;
}

double __stdcall getValue(const char* speciesID)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return NULL;
    }
    return gRRHandle->getValue(speciesID);
}

bool __stdcall setValue(const char* speciesID, const double& val)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }

    return gRRHandle->setValue(speciesID, val);
}

RRDataMatrixHandle __stdcall getStoichiometryMatrix(void)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
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
    Log(lInfo)<<ss.str();
}

bool __stdcall freeRRDataMatrixHandle(RRDataMatrixHandle matrix)
{
    free(matrix->Data);
    return true;
}

void SetError(const string& err)
{
    gError = new char[err.size() + 1];
    strcpy(gError, err.c_str());
}

char* __stdcall getLastError()
{
    return gError;
}

bool __stdcall reset()
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }
    gRRHandle->reset();
    return true;
}

int   __stdcall getNumberOfReactions()
{

}

double __stdcall getReactionRate(int)
{

}

int __stdcall getNumberOfBoundarySpecies()
{

}

char* __stdcall getBoundarySpeciesNames()          // <- treat char* as you treat it in setSelectionList (char *)
{

}

int __stdcall getNumberOfFloatingSpecies()
{

}

char* __stdcall getFloatingSpeciesNames()
{

}

int __stdcall getNumberOfGlobalParameterNames()
{

}

char* __stdcall getGlobalParameterNames()
{

}

void __stdcall setInitialConditions(double[])     // <- might be called changeInitialConditions in roadRunner
{

}

double __stdcall oneStep (double, double)
{

}

RRSymbolListHandle __stdcall getAvailableSymbols()              // <- You'll have to decide what type to return
{

}

double __stdcall steadyState()
{

}

RRDoubleVectorHandle __stdcall computeSteadyStateValues()
{

}

void __stdcall setSteadyStateSelectionList(char *)
{

}


//============================================================================
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
    //Intialize the logger
    LogOutput::mLogToConsole = false;
    return 1;
}


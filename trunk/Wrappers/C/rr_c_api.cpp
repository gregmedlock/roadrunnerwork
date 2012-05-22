//---------------------------------------------------------------------------
#pragma hdrstop
#include <windows.h>
#include "rrRoadRunner.h"
#include "rrLogger.h"
#include "rr_c_api.h"
//---------------------------------------------------------------------------

using namespace rr;
rr::RoadRunner *gRRHandle = NULL;
RRResult*       gResult = NULL;

void __stdcall   AssignLogger(FileLog& logger)
{
//    gLog = logger;
}

RRResult::~RRResult()
{
    delete [] Data;
    delete [] ColumnHeaders;
}

char* __stdcall   getBuildDate(void)
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
    if(!gRRHandle)
    {
        return "Please allocate a handle to roadrunner API before calling any API function";
    }

    char* text = new char[512]; //Where do we free this one.. ->  freeText(....);
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
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return false;
    }
    return gRRHandle->loadSBML(sbml);
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

double __stdcall getValue(void)
{
    return false;
}

bool __stdcall setValue(double val)
{
    return false;
}

RRDataMatrixHandle __stdcall getStoichiometryMatrix(void)
{
    if(!gRRHandle)
    {
        Log(lError)<<"Please allocate a handle to roadrunner API before calling any API function";
        return NULL;
    }

    DoubleMatrix tempMat = gRRHandle->getStoichiometryMatrix();

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

//============================================================================
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
    //Intialize the logger here..
    LogOutput::mLogToConsole = true;
    return 1;
}


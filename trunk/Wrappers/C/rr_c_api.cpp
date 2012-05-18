//---------------------------------------------------------------------------
#pragma hdrstop
#include <windows.h>
#include "rrRoadRunner.h"
#include "rr_c_api.h"
//---------------------------------------------------------------------------

rr::RoadRunner *gRRHandle = NULL;

RRHandle __stdcall getRRInstance()
{
    if(!gRRHandle)
    {
        gRRHandle = new rr::RoadRunner();
    }
    return gRRHandle;
}

int __stdcall GetNumber()
{
    return 123;
}

void __stdcall deleteRRInstance(RRHandle *handle)
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
    else
    {
        char* text = new char(512);

        strcpy(text, gRRHandle->getCopyright().c_str());
        return text;
    }
}

bool __stdcall loadSBML(const char* filePath);
bool __stdcall setTimeStart(double timeStart);
bool __stdcall setTimeEnd(double timeEnd);
bool __stdcall setNumPoints(int nrPoints);
RRResultHandle __stdcall simulate(void);
bool __stdcall FreeRRResult(RRResultHandle rrResult);
bool  __stdcall setSelectionList(const char* list);
char* __stdcall getReactionNames(void);
double __stdcall getValue(void);
bool __stdcall setValue(double val);

RRDataMatrixHandle __stdcall getStoichiometryMatrix(void)
{

    RRDataMatrixHandle matrix;
    matrix->mNrOfRows = 1;
    matrix->mNrOfCols = 10;
    matrix->mData = (double*) malloc(sizeof(double)*100);
    return matrix;
}

bool __stdcall FreeRRDataMatrixHandle(RRDataMatrixHandle matrix)
{
    free(matrix->mData);
    return true;
}

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
    return 1;
}


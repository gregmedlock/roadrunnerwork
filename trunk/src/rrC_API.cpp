//---------------------------------------------------------------------------
#pragma hdrstop
#include <windows.h>
#include "rrRoadRunner.h"
#include "rrC_API.h"
//---------------------------------------------------------------------------


rr::RoadRunner *gRRHandle = NULL;

RRHandle getRRInstance()
{
    if(!gRRHandle)
    {
        gRRHandle = new rr::RoadRunner();
    }
    return gRRHandle;
}

int GetNumber()
{
    return 123;
}

void deleteRRInstance(RRHandle *handle)
{
    delete handle;
    handle = NULL;
}

char* getCopyright()
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

bool loadSBML(const char* filePath);
bool setTimeStart(double timeStart);
bool setTimeEnd(double timeEnd);
bool setNumPoints(int nrPoints);
RRResultHandle simulate(void);
bool FreeRRResult(RRResultHandle rrResult);
bool  setSelectionList(const char* list);
char* getReactionNames(void);
double getValue(void);
bool setValue(double val);

RRDataMatrixHandle getStoichiometryMatrix(void)
{

	RRDataMatrixHandle matrix;
    matrix->mNrOfRows = 1;
	matrix->mNrOfCols = 10;
	matrix->mData = (double*) malloc(sizeof(double)*100);
    return matrix;
}

bool FreeRRDataMatrixHandle(RRDataMatrixHandle matrix)
{
	free(matrix->mData);
}




//---------------------------------------------------------------------------
//   Important note about DLL memory management when your DLL uses the
//   static version of the RunTime Library:
//
//   If your DLL exports any functions that pass String objects (or structs/
//   classes containing nested Strings) as parameter or function results,
//   you will need to add the library MEMMGR.LIB to both the DLL project and
//   any other projects that use the DLL.  You will also need to use MEMMGR.LIB
//   if any other projects which use the DLL will be performing new or delete
//   operations on any non-TObject-derived classes which are exported from the
//   DLL. Adding MEMMGR.LIB to your project will change the DLL and its calling
//   EXE's to use the BORLNDMM.DLL as their memory manager.  In these cases,
//   the file BORLNDMM.DLL should be deployed along with your DLL.
//
//   To avoid using BORLNDMM.DLL, pass string information using "char *" or
//   ShortString parameters.
//
//   If your DLL uses the dynamic version of the RTL, you do not need to
//   explicitly add MEMMGR.LIB as this will be done implicitly for you
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
	return 1;
}
//---------------------------------------------------------------------------

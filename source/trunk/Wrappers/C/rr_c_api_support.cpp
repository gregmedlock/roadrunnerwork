#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rr_c_api.h"
#include "rr_c_api_support.h"
//---------------------------------------------------------------------------
#if defined (__CODEGEAR__)
#pragma package(smart_init)
#endif

namespace rr_c_api
{
extern char* gLastError;

//static const char* ALLOCATE_API_ERROR_MSG = {"Please allocate a handle to the roadrunner API before calling any API function"};
void SetAPIError(const string& err)
{
    if(gLastError)
    {
        delete gLastError;
    }
    gLastError = new char[err.size() + 1];
    strcpy(gLastError, err.c_str());
}

char* createText(const string& str)
{
    char* newstr = new char[str.size() + 1];

    strcpy(newstr, str.c_str());
    return newstr;
}

RRDataMatrix* createMatrixFrom(const LIB_LA::DoubleMatrix& mat)
{
    RRDataMatrixHandle matrix = new RRDataMatrix;
    matrix->RSize = mat.RSize();
    matrix->CSize = mat.CSize();
    matrix->Data =  new double[mat.RSize()*mat.CSize()];

    int index = 0;
    for(rr::u_int row = 0; row < mat.RSize(); row++)
    {
        for(rr::u_int col = 0; col < mat.CSize(); col++)
        {
            matrix->Data[index++] = mat(row,col);
        }
    }
    return matrix;
}

RRDoubleVector* createVectorFrom(const vector<double>& vec)
{
    RRDoubleVector* aVec = new RRDoubleVector;
    aVec->Size = vec.size();
    if(aVec->Size)
    {
        aVec->Data = new double[aVec->Size];
    }
    for(int i = 0; i < aVec->Size; i++)
    {
        aVec->Data[i] =  vec[i];
    }

    return aVec;
}

bool CopyRRVector(const RRDoubleVector* vec, vector<double>& aVec)
{
    if(!vec)
    {
        return false;
    }

    aVec.resize(vec->Size);

    for(int i = 0; i < vec->Size; i++)
    {
        aVec[i] = vec->Data[i];
    }

    return true;
}


}

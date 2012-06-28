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
void setError(const string& err)
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

RRMatrix* createMatrix(const LIB_LA::DoubleMatrix& mat)
{
    RRMatrixHandle matrix = new RRMatrix;
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

RRVector* createVector(const vector<double>& vec)
{
    RRVector* aVec = new RRVector;
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

bool copyVector(const RRVector* src, vector<double>& dest)
{
    if(!src)
    {
        return false;
    }

    dest.resize(src->Size);

    for(int i = 0; i < src->Size; i++)
    {
        dest[i] = src->Data[i];
    }

    return true;
}

}

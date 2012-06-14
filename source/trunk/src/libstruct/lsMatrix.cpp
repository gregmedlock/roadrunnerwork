#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <string.h>
#include <stdlib.h>
#include <complex>

#include "lsMatrix.h"
#include "lsComplex.h"
#include "lsUtil.h"

//---------------------------------------------------------------------------
namespace LIB_LA
{

ostream& operator<<(ostream& stream, const DoubleMatrix& mat)
{
    for(unsigned row = 0; row < mat.RSize(); row++)
    {
        for(unsigned col = 0; col < mat.CSize(); col++)
        {
            double val = mat(row,col);
            stream<<val<<"\t";
        }
        stream<<std::endl;
    }
    return stream;
}


template<typename T> void Matrix<T>::initializeFrom2DMatrix(T** &oRawData, int rows, int cols)
{
    resize(rows, cols);
    for (unsigned int i = 0; i < _Rows; i++)
    {
        for (unsigned int j = 0; j < _Cols; j++)
        {
            this->operator ()(i,j) = oRawData[i][j];
        }
    }

}
template<typename T> void Matrix<T>::initializeFromConst2DMatrix(const T** oRawData, int rows, int cols)
{
    resize(rows, cols);
    for (unsigned int i = 0; i < _Rows; i++)
    {
        for (unsigned int j = 0; j < _Cols; j++)
        {
            (*this)(i,j) = oRawData[i][j];
        }
    }
}

template<typename T> T** Matrix<T>::get2DMatrix(int &nRows, int &nCols)
{

    T** oBuffer = (T**) malloc(sizeof(T*)*_Rows);
    for (unsigned int i = 0; i < _Rows; i++)
    {
        oBuffer[i] = (T*) malloc(sizeof(T)*_Cols);
    }

    for (unsigned int i = 0; i < _Rows; i++)
    {
        for (unsigned int j = 0; j < _Cols; j++)
        {
            oBuffer[i][j] = this->operator ()(i,j);
        }
    }

    nRows = _Rows;
    nCols = _Cols;

    return oBuffer;
}

template class Matrix<double>;
template class Matrix<int>;


template class Matrix< LIB_LA::Complex >;
template class Matrix< std::complex<double> >;

}

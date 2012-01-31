#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrDoubleMatrix.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

rrDoubleMatrix::rrDoubleMatrix(unsigned rows, unsigned cols)
:
mRowCount (rows),
mColCount (cols),
mMatrix(NULL)
{
	if (rows != 0 && cols != 0)
	{
    	mMatrix = new double[rows * cols];
	}
}

rrDoubleMatrix::~rrDoubleMatrix()
{
	delete[] mMatrix;
}

bool rrDoubleMatrix::Allocate(unsigned rows, unsigned cols)
{
	if(mMatrix)
    {
    	delete [] mMatrix;
    }

	mMatrix = new double[rows * cols];
    return mMatrix ? true : false;
}

double& rrDoubleMatrix::operator() (unsigned row, unsigned col)
{
    if (row >= mRowCount || col >= mColCount)
    {
    // throw BadIndex("Matrix subscript out of bounds");
    }
    return mMatrix[mColCount*row + col];
}

double rrDoubleMatrix::operator() (unsigned row, unsigned col) const
{
    if (row >= mRowCount || col >= mColCount)
    {
    //    throw BadIndex("const Matrix subscript out of bounds");
    }
    return mMatrix[mColCount*row + col];
}


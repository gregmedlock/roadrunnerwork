#ifndef rrDoubleMatrixH
#define rrDoubleMatrixH
//---------------------------------------------------------------------------
#include <vector>
#include "rrExporter.h"
using std::vector;

class RR_DECLSPEC rrDoubleMatrix
{
    private:
    	unsigned mRowCount, mColCount;
    	double* mMatrix;

    public:
    	rrDoubleMatrix(unsigned rows = 1, unsigned cols = 1);
    	double& operator() (unsigned row, unsigned col);
    	double  operator() (unsigned row, unsigned col) const;

    	~rrDoubleMatrix();                              // Destructor
//    	rrDoubleMatrix(rrDoubleMatrix const& m);               // Copy constructor
    	rrDoubleMatrix& operator = (rrDoubleMatrix const& m);   // Assignment operator
};

rrDoubleMatrix::rrDoubleMatrix(unsigned rows, unsigned cols)
: mRowCount (rows)
, mColCount (cols)
//mMatrix <--initialized below (after the 'if/throw' statement)
{
if (rows == 0 || cols == 0)
{
 //throw BadIndex("Matrix constructor has 0 size");
}
mMatrix = new double[rows * cols];
}


rrDoubleMatrix::~rrDoubleMatrix()
{
delete[] mMatrix;
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

#endif

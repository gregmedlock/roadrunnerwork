#ifndef rrDoubleMatrixH
#define rrDoubleMatrixH
//---------------------------------------------------------------------------
#include <vector>
#include "rrExporter.h"
using std::vector;

class RR_DECLSPEC rrDoubleMatrix
{
    private:
    	unsigned rows_, cols_;
    	double* data_;

    public:
    	rrDoubleMatrix(unsigned rows = 1, unsigned cols = 1);
    	double& operator() (unsigned row, unsigned col);
    	double  operator() (unsigned row, unsigned col) const;

    	~rrDoubleMatrix();                              // Destructor
//    	rrDoubleMatrix(rrDoubleMatrix const& m);               // Copy constructor
    	rrDoubleMatrix& operator = (rrDoubleMatrix const& m);   // Assignment operator
};

rrDoubleMatrix::rrDoubleMatrix(unsigned rows, unsigned cols)
: rows_ (rows)
, cols_ (cols)
//data_ <--initialized below (after the 'if/throw' statement)
{
if (rows == 0 || cols == 0)
{
 //throw BadIndex("Matrix constructor has 0 size");
}
data_ = new double[rows * cols];
}


rrDoubleMatrix::~rrDoubleMatrix()
{
delete[] data_;
}

double& rrDoubleMatrix::operator() (unsigned row, unsigned col)
{
if (row >= rows_ || col >= cols_)
{
// throw BadIndex("Matrix subscript out of bounds");
}
return data_[cols_*row + col];
}

double rrDoubleMatrix::operator() (unsigned row, unsigned col) const
{
if (row >= rows_ || col >= cols_)
{
//    throw BadIndex("const Matrix subscript out of bounds");
}
return data_[cols_*row + col];
}

#endif

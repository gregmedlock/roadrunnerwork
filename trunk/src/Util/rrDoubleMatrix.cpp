#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrException.h"
#include "rrDoubleMatrix.h"
//---------------------------------------------------------------------------



namespace rr
{


DoubleMatrix::DoubleMatrix(unsigned rows, unsigned cols)
:
mRowCount (rows),
mColCount (cols),
mMatrix(NULL),
mIsOwner(true)
{
	if (rows != 0 && cols != 0)
	{
    	mMatrix = new double[rows * cols];
	}
}

DoubleMatrix::DoubleMatrix(double* ptrToArray, const int& rowCount, const int& colCount)
{
	mRowCount = rowCount;
	mColCount = colCount;

	//Shallow or deep copy?
    mMatrix = ptrToArray; //Thats is pretty shallow...
    mIsOwner = false; 	  //Somebody else allocatesd this one
}

DoubleMatrix::~DoubleMatrix()
{
	if(mIsOwner)
    {
    	delete mMatrix;
    }
}

bool DoubleMatrix::Allocate(unsigned rows, unsigned cols)
{
	if(mMatrix)
    {
    	delete [] mMatrix;
    }

	mMatrix = new double[rows * cols];
	mRowCount = rows;
	mColCount = cols;
    return mMatrix ? true : false;
}

//=========== OPERATORS
double& DoubleMatrix::operator() (unsigned row, unsigned col)
{
//    if (row >= mRowCount || col >= mColCount)
    {
//    	throw Exception("Matrix subscript out of bounds");
    }
    return mMatrix[mColCount*row + col];
}

double DoubleMatrix::operator() (unsigned row, unsigned col) const
{
//    if (row >= mRowCount || col >= mColCount)
    {
    //    throw BadIndex("const Matrix subscript out of bounds");
    }
    return mMatrix[mColCount*row + col];
}


DoubleMatrix& DoubleMatrix::operator = (DoubleMatrix const& rhs)
{
	DoubleMatrix mat(rhs.RSize(), rhs.CSize());

    for(int col = 0; col < CSize(); col++)
    {
		for(int row = 0; row < RSize(); row++)
        {
			mMatrix[col*row + col] = rhs(col,row);
        }
    }
	return *this;
}


////        internal static double[][] GetDoubleMatrixFromPtr(IntPtr pointer, int nRows, int nCols)
////        {
////            IntPtr[] rawRows = new IntPtr[nRows];
////            double[][] oResult = new double[nRows][];
////            Marshal.Copy(pointer, rawRows, 0, nRows);
////            for (int i = 0; i < nRows; i++)
////            {
////                oResult[i] = new double[nCols];
////                Marshal.Copy(rawRows[i], oResult[i], 0, nCols);
////            } // for (int)
////            StructAnalysis.FreeMatrix(pointer, nRows);
////            return oResult;
////        } // GetDoubleMatrixFromPtr(pointer, nRows, nCols)

DoubleMatrix RR_DECLSPEC GetDoubleMatrixFromPtr(double** *pointer, const int& nRows, const int& nCols)
{
	DoubleMatrix mat(nRows, nCols);
    for(int col = 0; col < mat.CSize(); col++)
    {
		for(int row = 0; row < mat.RSize(); row++)
        {
        	double val = **pointer[col*row + col];
			mat(row,col) = val ;
        }
    }

	return mat;
}

ostream& operator<<(ostream& stream, const DoubleMatrix& mat)
{
	for(int row = 0; row < mat.RSize(); row++)
    {
	    for(int col = 0; col < mat.CSize(); col++)
        {
        	double val = mat(row,col);
			stream<<val<<"\t";
        }
        stream<<std::endl;
    }
	return stream;
}

} //namespace rr

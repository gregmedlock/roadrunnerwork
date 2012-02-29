#ifndef rrDoubleMatrixH
#define rrDoubleMatrixH
#include <vector>
#include "rrObject.h"

using std::vector;

namespace rr
{
class RR_DECLSPEC DoubleMatrix : public rrObject
{
    protected:
    	unsigned 		mRowCount;
        unsigned 		mColCount;
    	double* 		mMatrix;
		bool			mIsOwner;

    public:
    					DoubleMatrix(unsigned rows = 0, unsigned cols = 0);
    					DoubleMatrix(double* ptrToArray, const int& rowCount = 0, const int& colCount = 0);
		int				RSize() const {return mRowCount;}
        int 			CSize() const {return mColCount;}
    	double& 		operator() (unsigned row, unsigned col);
    	double  		operator() (unsigned row, unsigned col) const;

        bool			Allocate(unsigned rows, unsigned cols);

    					~DoubleMatrix();                            // Destructor
        //    			DoubleMatrix(DoubleMatrix const& m);        // Copy constructor
        DoubleMatrix& 	operator = (DoubleMatrix const& rhs);   	// Assignment operator
        double*			GetPointer(){return mMatrix;}

};

//DoubleMatrix RR_DECLSPEC GetDoubleMatrixFromPtr(double** *pointer, const int& nRows, const int& nCols);
ostream& RR_DECLSPEC operator<<(ostream&, const DoubleMatrix& mat);
}
#endif

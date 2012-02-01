#ifndef rrDoubleMatrixH
#define rrDoubleMatrixH
#include <vector>
#include "rrObject.h"

using std::vector;

namespace rr
{
class RR_DECLSPEC rrDoubleMatrix : public rrObject
{
    private:
    	unsigned 		mRowCount;
        unsigned 		mColCount;
    	double* 		mMatrix;

    public:
    					rrDoubleMatrix(unsigned rows = 0, unsigned cols = 0);
    	double& 		operator() (unsigned row, unsigned col);
    	double  		operator() (unsigned row, unsigned col) const;
        bool			Allocate(unsigned rows, unsigned cols);

    					~rrDoubleMatrix();                              // Destructor
        //    			rrDoubleMatrix(rrDoubleMatrix const& m);               // Copy constructor
    					rrDoubleMatrix& operator = (rrDoubleMatrix const& m);   // Assignment operator
};


}
#endif

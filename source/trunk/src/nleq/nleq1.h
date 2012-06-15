#ifndef rrNLEQH
#define rrNLEQH

#include "f2c.h"

//#ifdef WIN32
//#define DLLEXPORT __declspec(dllexport)
//#define STDCALL  __stdcall
//#else
//#define DLLEXPORT
//#define STDCALL
//#endif
//

extern "C"
{
int NLEQ1(
    integer *n,
    U_fp fcn,
    U_fp jac,
    doublereal *x,
    doublereal *xscal,
    doublereal *rtol,
    integer *iopt,
    integer *ierr,
    integer *liwk,
    integer *iwk,
    integer *lrwk,
    doublereal *rwk);

}
#endif

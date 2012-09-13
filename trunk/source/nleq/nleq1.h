#ifndef nleqH
#define nleqH

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#define STDCALL  __stdcall
#else
#define DLLEXPORT
#define STDCALL
#endif

#ifdef __cplusplus
extern "C"
{
#endif

#include "f2c_nleq.h"

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
#ifdef __cplusplus
}
#endif

#endif


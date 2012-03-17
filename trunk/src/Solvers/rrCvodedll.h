#ifndef rrCvodedllH
#define rrCvodedllH
#include <stdlib.h>
//#include <sundials/sundials_types.h>
//#include <nvector/nvector_serial.h>
//#include <cvode/cvode.h>
//#include <cvode/cvode_dense.h>
//#include <cvode/cvode_bandpre.h>

#ifdef WIN32
#include <windows.h>
#define STDCALL  __stdcall
#else
#define STDCALL
#endif


typedef  void (STDCALL *TModelCallBack)(int n, double Time, double *y, double *ydot, void *f_data);
typedef  void (STDCALL *TRootCallBack)(double t, double *y, double *gout, void *g_data);

#define Ith(v,i)    NV_Ith_S(v,i-1)       /* Ith numbers components 1..NEQ */

// Declare the call back pointers
TModelCallBack callBackModel;
TRootCallBack  callBackRoot;

// N_Vector is a point to an N_Vector structure
//todo: most of these are wrappers for functions in cvode, unnecesarry
RR_DECLSPEC void* 		NewCvode_Vector(int);
RR_DECLSPEC void 		FreeCvode_Vector (N_Vector);
RR_DECLSPEC void 		FreeCvode_Mem (void **p);
RR_DECLSPEC void 		Cvode_SetVector (N_Vector v, int Index, double Value);
RR_DECLSPEC double 		Cvode_GetVector (N_Vector v, int Index);
RR_DECLSPEC void*		Create_BDF_NEWTON_CVode();
RR_DECLSPEC void*		Create_ADAMS_FUNCTIONAL_CVode();
RR_DECLSPEC int  		AllocateCvodeMem (void *, int n, TModelCallBack, double, N_Vector, double, N_Vector);//, long int[], double[]);
RR_DECLSPEC int  		CvDense (void *, int);  // int = size of systems
RR_DECLSPEC int  		CVReInit (void *cvode_mem, double t0, N_Vector y0, double reltol, N_Vector abstol);
RR_DECLSPEC int  		Run_Cvode (void *cvode_mem, double tout, N_Vector y, double *t, char *ErrMsg);
RR_DECLSPEC int  		CVGetRootInfo (void *cvode_mem, int *rootsFound);
RR_DECLSPEC int  		CVRootInit (void *cvode_mem, int numRoots, TRootCallBack callBack, void *gdata);
RR_DECLSPEC int         SetMaxNumSteps(void *cvode_mem, int mxsteps);
RR_DECLSPEC int         SetMaxOrder(void *cvode_mem, int mxorder);
RR_DECLSPEC int         CVSetFData (void *cvode_mem, void *f_data);
RR_DECLSPEC int         SetMaxErrTestFails(void *cvode_mem, int maxnef);
RR_DECLSPEC int         SetMaxConvFails(void *cvode_mem, int maxncf);
RR_DECLSPEC int         SetMaxNonLinIters (void *cvode_mem, int maxcor);
RR_DECLSPEC int         SetErrFile (void *cvode_mem, FILE *errfp);
RR_DECLSPEC int         SetErrHandler (void *cvode_mem, CVErrHandlerFn callback, void* user_data );
RR_DECLSPEC int         SetMinStep(void *cvode_mem, double minStep);
RR_DECLSPEC int         SetMaxStep(void *cvode_mem, double maxStep);
RR_DECLSPEC int         SetInitStep(void *cvode_mem, double initStep);
RR_DECLSPEC FILE       *fileOpen (char *fileName);
RR_DECLSPEC void        fileClose (FILE *fp);

#endif

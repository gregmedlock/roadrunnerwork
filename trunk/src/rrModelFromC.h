#ifndef rrModelFromCH
#define rrModelFromCH
#include <windows.h>
#include "rrIModel.h"
//---------------------------------------------------------------------------

namespace rr
{
class CGenerator;

typedef void 	(WINAPI*c_void)();
typedef int 	(WINAPI*c_int)();
typedef int 	(WINAPI*c_int_int)(int);
typedef char* 	(WINAPI*c_charStar)();
typedef void    (WINAPI*c_void_doubleStar)(double*);
typedef double  (WINAPI*c_double_int)(int);
typedef double* (WINAPI*c_doubleStar_void)();

class RR_DECLSPEC ModelFromC : public IModel	//This model sets up nnecessary handles to C DLL functions
{
	protected:
	    CGenerator*					mCodeGenerator;	//There are some arrays returned that we don't know the size of..!

        bool						mIsInitialized;	//If all functions are found properly in the dll, this one is true
		HINSTANCE					mDLLHandle;

        c_int 				        cInitModel;
        c_charStar 		            cGetModelName;
        c_void                      cinitializeInitialConditions;
        c_void                      csetParameterValues;
        c_void                      csetCompartmentVolumes;
		c_int_int			        cgetNumLocalParameters;
        c_void                      csetBoundaryConditions;
        c_void                      csetInitialConditions;
        c_void                      cevalInitialAssignments;
        c_void_doubleStar           ccomputeRules;
        c_void                      cconvertToAmounts;
        c_void                      ccomputeConservedTotals;
        c_double_int   		        cgetConcentration;
        c_doubleStar_void	        cGetCurrentValues;

		//Utility
		HANDLE 						GetFunctionPtr(const string& function);


    public:
						    		ModelFromC(CGenerator* generator, HINSTANCE dllHandle = NULL);
                                   ~ModelFromC();
		bool						SetupDLLFunctions();


        //Non inherited
        bool						SetupDLLData();


        //The following functions C equivalent may need to be in the DLL
        //Inherited functions
    	void 						setCompartmentVolumes();
        int 						getNumLocalParameters(int reactionId);
        void                        computeRules(vector<double>& _y);
		void  						initializeInitialConditions();
		void  						setParameterValues();
		void 						setBoundaryConditions();
		void 						setInitialConditions();
        void	                 	evalInitialAssignments();
        void			           	computeRules(double* ay);
        void 	                	convertToAmounts();
        void    	         	    computeConservedTotals();
        double		   				getConcentration(int index);
        vector<double> 				GetCurrentValues();




};

}


#endif

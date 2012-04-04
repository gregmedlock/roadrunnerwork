#ifndef rrModelFromCH
#define rrModelFromCH
#include <windows.h>
#include "rrIModel.h"
//---------------------------------------------------------------------------

namespace rr
{
class CGenerator;

typedef void 	(__cdecl*c_void)();
typedef int 	(__cdecl*c_int)();
typedef int 	(__cdecl*c_int_int)(int);
typedef char* 	(__cdecl*c_charStar)();
typedef void    (__cdecl*c_void_doubleStar)(double*);
typedef double  (__cdecl*c_double_int)(int);
typedef double* (__cdecl*c_doubleStar)();
typedef void	(__cdecl*c_void_double_doubleStar)(double, double*);


class RR_DECLSPEC ModelFromC : public IModel	//This model sets up nnecessary handles to C DLL functions
{
	protected:
	    CGenerator*					mCodeGenerator;	//There are some arrays returned that we don't know the size of..!

        bool						mIsInitialized;	//If all functions are found properly in the dll, this one is true
		HINSTANCE					mDLLHandle;

        double*						mAmounts;		//This is the "amounts" data in the DLL. IModel also has amounts.. CONFUSING
		double*						m_dydt;	   		//This is the "dydt" data in the DLL. IModel also has amounts.. CONFUSING
		double*						mInitY;
        double*						mY;             //Corresponds to y in IModel
        double*						mRates;
        double*						mGP;

		//Function pointers...
        c_int 				        cInitModel;
        c_charStar 		            cGetModelName;
        c_void                      cinitializeInitialConditions;
        c_void                      csetParameterValues;
        c_void                      csetCompartmentVolumes;
		c_int_int			        cgetNumLocalParameters;
        c_void                      csetBoundaryConditions;
		c_void                      csetInitialConditions;
		c_void                      cevalInitialAssignments;
		c_void_doubleStar			cupdateDependentSpeciesValues;
		c_void_doubleStar           ccomputeRules;

		c_void                      cconvertToAmounts;
		c_void                      ccomputeConservedTotals;
		c_double_int   		        cgetConcentration;
		c_doubleStar	        	cGetCurrentValues;
		c_void_double_doubleStar 	cevalModel;
		c_void                      cconvertToConcentrations;
		c_void_double_doubleStar    cevalEvents;
		c_void						ccomputeAllRatesOfChange;


		//Utility
		HANDLE 						GetFunctionPtr(const string& function);

    public:
						    		ModelFromC(CGenerator* generator, HINSTANCE dllHandle = NULL);
                                   ~ModelFromC();
        //Non inherited
        bool						SetupDLLData();
		bool						SetupDLLFunctions();

        void						LoadData();	//This one copies data from the DLL to vectors and lists in the model..

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

        //Access dll data
        vector<double> 				GetCurrentValues();
        double		   				GetAmounts(const int& i);
        vector<double>				GetdYdT();

//        int                         getNumIndependentVariables();
//        int                         getNumDependentVariables();
//        int                         getNumTotalVariables();
//        int                         getNumBoundarySpecies();
//        int                         getNumGlobalParameters();
//        int                         getNumCompartments();
//        int                         getNumReactions();
//        int                         getNumRules();
//        int                         getNumEvents();
//        void                        initializeInitialConditions();
//        void                        setInitialConditions();
//        void                        setParameterValues();
//        void                        setBoundaryConditions();
//        void                        InitializeRates();
//        void                        AssignRates();
//        void                        AssignRates(vector<double>& rates);
//        void                        computeConservedTotals();
//        void                        computeEventPriorites();
//        void                        setConcentration(int index, double value);
//        void                        convertToAmounts();
        void                        convertToConcentrations();
        void                        updateDependentSpeciesValues(vector<double>& _y);
//        void                        computeRules(vector<double>& _y);
//        void                        computeReactionRates(double time, vector<double>& y);
		void                        computeAllRatesOfChange();
		void                        evalModel(double time, vector<double>& y);
        void                        evalEvents(double time, vector<double>& y);
//        void                        resetEvents();
//        void                        evalInitialAssignments();
//        void                        testConstraints();
//        void                        InitializeRateRuleSymbols();

};

}


#endif

#ifndef rrIModelH
#define rrIModelH
#include <string>
#include <vector>
#include <list>
#include "rrObject.h"
#include "rrTEventDelayDelegate.h"
#include "rrTEventAssignmentDelegate.h"
#include "rrTComputeEventAssignmentDelegate.h"
#include "rrTPerformEventAssignmentDelegate.h"

using std::list;
using std::vector;
using std::string;

namespace rr
{

class RR_DECLSPEC IModel : public rrObject	//Abstract class for Models
{
    protected:
    	//These variables is also generated in the c-code.
        //Init a decendent models data later
        int                                     numIndependentVariables;
        int                                     numDependentVariables;
        int                                     numTotalVariables;
        int                                     numBoundaryVariables;
        int                                     numGlobalParameters;
        int                                     numCompartments;
        int                                     numReactions;
        int                                     numRules;
        int                                     numEvents;


    public://==== this makes the following attributes public.. for now. No great design..

        // Property signatures:
        vector<double> 					        y;
        list<string> 					        Warnings;
        vector<double>                  		init_y;
        vector<double>                          amounts;
        vector<double>                          bc;

        /// <summary>
        /// modifiable species
        ///reference values
        /// </summary>
        vector<double> 					        sr;

        // Global parameters
        vector<double> 					        gp;

        // Local parameters
        vector<double> 					        lp ;

        // Compartment volumes
        vector<double> 	                        c ;
        vector<double> 	                        dydt;
        vector<double> 	                        rates;

        // Conservation totals
        vector<double> 					        ct ;

        // additional rateRules
        vector<double> 					        rateRules;
        double 							        time ;
        vector<double> 					        eventTests;
        vector<double> 					        eventPriorities;
        vector<TEventDelayDelegate> 	        eventDelay;
        vector<bool>                            eventType;
        vector<bool>                            eventPersistentType;
        vector<bool>                            eventStatusArray;
        vector<bool>                            previousEventStatusArray;

        vector<TEventAssignmentDelegate>		eventAssignments;
        vector<TComputeEventAssignmentDelegate> computeEventAssignments;
        vector<TPerformEventAssignmentDelegate> performEventAssignments;

        virtual int                             getNumIndependentVariables();
        virtual int                             getNumDependentVariables();
        virtual int                             getNumTotalVariables();
        virtual int                             getNumBoundarySpecies();
        virtual int                             getNumGlobalParameters();
        virtual int                             getNumCompartments();
        virtual int                             getNumReactions();
        virtual int                             getNumRules();
        virtual int                             getNumEvents();

    public:
												IModel();
		virtual									~IModel();

		
        vector<double>&							Get_y();
        vector<double>&							Get_bc();
		vector<double>&							Get_c();
		vector<double>&							Get_gp();
		vector<double>&							Get_ct();
		vector<double>&							Get_dydt();
		vector<double>&							Get_rates();
		vector<double>&							Get_rateRules();
		vector<double>&							Get_sr();
		double									Get_time();
		vector<bool>&							Get_eventStatusArray();
		vector<double>&							Get_eventTests();
		vector<bool>&							Get_previousEventStatusArray();

        virtual void                            setCompartmentVolumes() = 0;
        virtual vector<double> 					GetCurrentValues() = 0 ;
        virtual double 							getConcentration(int index) = 0;
        virtual void                            initializeInitialConditions(){}
        virtual void                            setInitialConditions(){}
        virtual void                            setParameterValues(){}
        virtual void                            setBoundaryConditions(){}
        virtual void                            InitializeRates(){}
        virtual void                            AssignRates(){}
        virtual void                            AssignRates(vector<double>& rates){}

        virtual void                            computeConservedTotals(){}
        virtual void                            computeEventPriorites(){}
        virtual void                            setConcentration(int index, double value){}

        virtual void                            convertToAmounts(){}
        virtual void                            convertToConcentrations(){}
        virtual void                            updateDependentSpeciesValues(vector<double>& _y){}
        virtual void                            computeRules(vector<double>& _y){}
        virtual void                            computeReactionRates(double time, vector<double>& y){}
        virtual void                            computeAllRatesOfChange(){}
        virtual void                            evalModel(double time, vector<double>& y){}
        virtual void                            evalEvents(double time, vector<double>& y){}
        virtual void                            resetEvents(){}
        virtual void                            evalInitialAssignments(){}
        virtual void                            testConstraints(){}
        virtual void                            InitializeRateRuleSymbols(){}

        // Level 2 support
        virtual int 							getNumLocalParameters(int reactionId) = 0;
};
} //namespace rr

//C#
//    public interface IModel
//    {
//        // Property signatures:
//        double[] y { get; set; }
//
//        List<string> Warnings { get; set; }
//
//
//        double[] init_y { get; set; }
//
//
//        double[] amounts { get; set; }
//
//        double[] bc { get; set; }
//
//        /// <summary>
//        /// modifiable species reference values
//        /// </summary>
//        double[] sr { get; set; }
//
//        // Global parameters
//        double[] gp { get; set; }
//
//        // Local parameters
//        double[][] lp { get; set; }
//
//        // Compartment volumes
//        double[] c { get; set; }
//
//        double[] dydt { get; set; }
//
//        double[] rates { get; set; }
//
//        // Conservation totals
//        double[] ct { get; set; }
//        // additional rateRules
//        double[] rateRules { get; set; }
//
//        double time { get; set; }
//
//        double[] eventTests { get; set; }
//
//        double[] eventPriorities { get; set; }
//
//        TEventDelayDelegate[] eventDelay { get; set; }
//
//        bool[] eventType { get; set; }
//        bool[] eventPersistentType { get; set; }
//
//        bool[] eventStatusArray { get; set; }
//
//        bool[] previousEventStatusArray { get; set; }
//
//        TEventAssignmentDelegate[] eventAssignments { get; set; }
//        TComputeEventAssignmentDelegate[] computeEventAssignments { get; set; }
//        TPerformEventAssignmentDelegate[] performEventAssignments { get; set; }
//
//        int getNumIndependentVariables { get; }
//        int getNumDependentVariables { get; }
//        int getNumTotalVariables { get; }
//        int getNumBoundarySpecies { get; }
//        int getNumGlobalParameters { get; }
//        int getNumCompartments { get; }
//        int getNumReactions { get; }
//        int getNumRules { get; }
//        int getNumEvents { get; }
//
//        void setCompartmentVolumes();
//        void initializeInitialConditions();
//        void setInitialConditions();
//        void setParameterValues();
//        void setBoundaryConditions();
//        void InitializeRates();
//        void AssignRates();
//        void AssignRates(double[] rates);
//        double[] GetCurrentValues();
//        void computeConservedTotals();
//        void computeEventPriorites();
//        void setConcentration(int index, double value);
//        double getConcentration(int index);
//        void convertToAmounts();
//        void convertToConcentrations();
//        void updateDependentSpeciesValues(double[] _y);
//        void computeRules(double[] _y);
//        void computeReactionRates(double time, double[] y);
//        void computeAllRatesOfChange();
//        void evalModel(double time, double[] y);
//        void evalEvents(double time, double[] y);
//        void resetEvents();
//
//        void evalInitialAssignments();
//        void testConstraints();
//        void InitializeRateRuleSymbols();
//
//        // Level 2 support
//        int getNumLocalParameters(int reactionId);
//    }

#endif

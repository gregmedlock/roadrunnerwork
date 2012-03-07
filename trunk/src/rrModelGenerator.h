#ifndef rrModelGeneratorH
#define rrModelGeneratorH
#include <string>
#include <vector>
#include <list>
#include "rrObject.h"
#include "rrStringList.h"
#include "rrSymbolList.h"
//#include "rrDoubleMatrix.h"
#include "rrStringBuilder.h"

//#include "rrRoadRunner.h"

#include "rrLibStructSupport.h"
#include "rrNOMSupport.h"
#include "rrScanner.h"
using std::string;
using std::vector;
using std::list;

namespace rr
{

class RR_DECLSPEC ModelGenerator : public rrObject
{
    protected:
        const string                        STR_DoubleFormat;
        const string                        STR_FixAmountCompartments;
        vector<int> 		                _LocalParameterDimensions;
        string 				                _ModelName;
        int                                 _NumBoundarySpecies;
        int                                 _NumCompartments;
        int                                 _NumDependentSpecies;
        int                                 _NumEvents;
        int                                 _NumFloatingSpecies;
        int                                 _NumGlobalParameters;
        int                                 _NumIndependentSpecies;
        int                                 _NumReactions;
        int                                 _TotalLocalParmeters;
        StringList       	                _functionNames;
        StringList          	            _functionParameters;
        StringList                     		dependentSpeciesList;
        StringList		                    independentSpeciesList;
        int 				                _NumModifiableSpeciesReferences;
        StructAnalysis						mStructAnalysis;					//Object to facilitate calls to libStruct library
        NOMSupport							mNOM;								//Object that provide some wrappers and new "NOM" functions
        IntStringHashTable   			    _oMapRateRule;
        SymbolList                         	boundarySpeciesList;
        SymbolList                         	compartmentList;
        SymbolList                         	conservationList;
        SymbolList                         	floatingSpeciesAmountsList;
        SymbolList                         	floatingSpeciesConcentrationList;
        SymbolList                         	globalParameterList;
        vector<SymbolList> 					localParameterList;
        SymbolList 							reactionList;
        StringList	                        Warnings;

		//Pure Virtual functions... =====================================
        virtual string                      convertUserFunctionExpression(const string& equation) = 0;
        virtual void 						SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& sb) = 0;
        virtual void 						SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb) = 0;
        virtual void 						SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb) = 0;
        virtual string 				        FindSymbol(const string& varName) = 0;
        virtual int                         ReadFloatingSpecies() = 0;
        virtual int                         ReadBoundarySpecies() = 0;
        virtual void                        WriteOutSymbolTables(StringBuilder& sb) = 0;
        virtual void                        WriteComputeAllRatesOfChange(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0) = 0;
        virtual void                        WriteComputeConservedTotals(StringBuilder& sb, const int& numFloatingSpecies, const int& numDependentSpecies) = 0;
        virtual void                        WriteUpdateDependentSpecies(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0) = 0;
        virtual void                        WriteUserDefinedFunctions(StringBuilder& sb) = 0;
        virtual void                        WriteResetEvents(StringBuilder& sb, const int& numEvents) = 0;
        virtual void                        WriteSetConcentration(StringBuilder& sb) = 0;
        virtual void                        WriteGetConcentration(StringBuilder& sb) = 0;
        virtual void                        WriteConvertToAmounts(StringBuilder& sb) = 0;
        virtual void                        WriteConvertToConcentrations(StringBuilder& sb) = 0;
        virtual void                        WriteProperties(StringBuilder& sb) = 0;
        virtual void                        WriteAccessors(StringBuilder& sb) = 0;
        virtual void                        WriteOutVariables(StringBuilder& sb) = 0;
        virtual void                        WriteClassHeader(StringBuilder& sb) = 0;
        virtual void                        WriteTestConstraints(StringBuilder& sb) = 0;
        virtual void                        WriteEvalInitialAssignments(StringBuilder& sb, const int& numReactions) = 0;
        virtual int 		 		        WriteComputeRules(StringBuilder& sb, const int& numReactions) = 0;
        virtual void                        WriteComputeReactionRates(StringBuilder& sb, const int& numReactions) = 0;
        virtual void                        WriteEvalEvents(StringBuilder& sb, const int& numEvents, const int& numFloatingSpecies) = 0;
        virtual void                        WriteEvalModel(StringBuilder& sb, const int& numReactions, const int& numIndependentSpecies, const int& numFloatingSpecies, const int& numOfRules) = 0;
        virtual void                        WriteEventAssignments(StringBuilder& sb, const int& numReactions, const int& numEvents) = 0;
        virtual void                        WriteSetParameterValues(StringBuilder& sb, const int& numReactions) = 0;
        virtual void                        WriteSetCompartmentVolumes(StringBuilder& sb) = 0;
        virtual void                        WriteSetBoundaryConditions(StringBuilder& sb) = 0;
        virtual void                        WriteSetInitialConditions(StringBuilder& sb, const int& numFloatingSpecies) = 0;
        virtual string                    	convertCompartmentToC(const string& compartmentName) = 0;
        virtual string                      convertSpeciesToBc(const string& speciesName) = 0;
        virtual string                      convertSpeciesToY(const string& speciesName) = 0;
        virtual string                      convertSymbolToC(const string& compartmentName) = 0;
        virtual string                      convertSymbolToGP(const string& parameterName) = 0;

        //////////////////////////////////////////////////////////////

        string                              substituteTerms(const int& numReactions, const string& reactionName, const string& equation);
		ASTNode* 							CleanEquation(ASTNode* ast);
        string 		                		CleanEquation(const string& equation);
        string 				                substituteTerms(const string& reactionName, const string& inputEquation, bool bFixAmounts);
        string 				                NL();
        double*				                InitializeL0(int& nrRows, int& nrCols);
	    bool 								ExpressionContainsSymbol(ASTNode* ast, const string& symbol);
        bool 		                		ExpressionContainsSymbol(const string& expression, const string& symbol);
        Symbol*				                GetSpecies(const string& id);
        int                                 ReadGlobalParameters();
        void 				                ReadLocalParameters(const int& numReactions,  vector<int>& localParameterDimensions, int& totalLocalParmeters);
        int 				                ReadCompartments();
        int 				                ReadModifiableSpeciesReferences();

    public:
									        ModelGenerator();
		virtual						       ~ModelGenerator();
        void								Reset();
        int                                 getNumberOfReactions();
        int                                 NumAdditionalRates();	//this variable is the size of _oMapRateRule
        StringList                          getBoundarySpeciesList();
        StringList                          getCompartmentList();
        StringList                          getConservationList();
        StringList                          getFloatingSpeciesConcentrationList();
        StringList                          getGlobalParameterList();
        StringList                          getLocalParameterList(int reactionId);
        StringList  	                   	getReactionNames();
        SymbolList 							ModifiableSpeciesReferenceList;
    	string 								WriteDouble(const double& value);

    	// Generates the Model Code from the SBML string
    	virtual string 		 				generateModelCode(const string& sbmlStr) = 0;	//Any decendant need to implement at least this one
};
}//namespace rr

#endif

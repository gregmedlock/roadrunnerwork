#ifndef rrCGeneratorH
#define rrCGeneratorH
//---------------------------------------------------------------------------
#include "rrModelGenerator.h"
namespace rr
{


class RR_DECLSPEC CGenerator : public ModelGenerator
{

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
        StructAnalysis						mStructAnalysis;		//!Object to facilitate calls to libStruct library
        NOMSupport							mNOM;					//Object that provide some wrappers and new "NOM" functions
        IntStringHashTable   			    _oMapRateRule;

        string                              convertCompartmentToC(const string& compartmentName);
        string                              convertSpeciesToBc(const string& speciesName);
        string                              convertSpeciesToY(const string& speciesName);
        string                              convertSymbolToC(const string& compartmentName);
        string                              convertSymbolToGP(const string& parameterName);
        string                              convertUserFunctionExpression(const string& equation);
        string                              substituteTerms(const int& numReactions, const string& reactionName, const string& equation);
        void 								SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& sb);
        void 								SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb);
        void 								SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb);
		ASTNode* 							CleanEquation(ASTNode* ast);
        string 		                		CleanEquation(const string& equation);
        string 				                substituteTerms(const string& reactionName, const string& inputEquation, bool bFixAmounts);
        string 				                NL();
        double*				                InitializeL0(int& nrRows, int& nrCols);
	    bool 								ExpressionContainsSymbol(ASTNode* ast, const string& symbol);
        bool 		                		ExpressionContainsSymbol(const string& expression, const string& symbol);
        Symbol*				                GetSpecies(const string& id);
        string 				                FindSymbol(const string& varName);
        void                                WriteOutSymbolTables(StringBuilder& sb);
        void                                WriteComputeAllRatesOfChange(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0);
        void                                WriteComputeConservedTotals(StringBuilder& sb, const int& numFloatingSpecies, const int& numDependentSpecies);
        void                                WriteUpdateDependentSpecies(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0);
        void                                WriteUserDefinedFunctions(StringBuilder& sb);
        void                                WriteResetEvents(StringBuilder& sb, const int& numEvents);
        void                                WriteSetConcentration(StringBuilder& sb);
        void                                WriteGetConcentration(StringBuilder& sb);
        void                                WriteConvertToAmounts(StringBuilder& sb);
        void                                WriteConvertToConcentrations(StringBuilder& sb);
        void                                WriteProperties(StringBuilder& sb);
        void                                WriteAccessors(StringBuilder& sb);
        void                                WriteOutVariables(StringBuilder& sb);
        void                                WriteClassHeader(StringBuilder& sb);
        void                                WriteTestConstraints(StringBuilder& sb);
        void                                WriteEvalInitialAssignments(StringBuilder& sb, const int& numReactions);
        int 		 		                WriteComputeRules(StringBuilder& sb, const int& numReactions);
        void                                WriteComputeReactionRates(StringBuilder& sb, const int& numReactions);
        void                                WriteEvalEvents(StringBuilder& sb, const int& numEvents, const int& numFloatingSpecies);
        void                                WriteEvalModel(StringBuilder& sb, const int& numReactions, const int& numIndependentSpecies, const int& numFloatingSpecies, const int& numOfRules);
        void                                WriteEventAssignments(StringBuilder& sb, const int& numReactions, const int& numEvents);
        void                                WriteSetParameterValues(StringBuilder& sb, const int& numReactions);
        void                                WriteSetCompartmentVolumes(StringBuilder& sb);
        void                                WriteSetBoundaryConditions(StringBuilder& sb);
        void                                WriteSetInitialConditions(StringBuilder& sb, const int& numFloatingSpecies);
        int                                 ReadFloatingSpecies();
        int                                 ReadBoundarySpecies();
        int                                 ReadGlobalParameters();
        void 				                ReadLocalParameters(const int& numReactions,  vector<int>& localParameterDimensions, int& totalLocalParmeters);
        int 				                ReadCompartments();
        int 				                ReadModifiableSpeciesReferences();

    public:
									        CGenerator();
		virtual						       ~CGenerator();
        void								Reset();
        SymbolList                         	boundarySpeciesList;
        SymbolList                         	compartmentList;
        SymbolList                         	conservationList;
        SymbolList                         	floatingSpeciesAmountsList;
        SymbolList                         	floatingSpeciesConcentrationList;
        SymbolList                         	globalParameterList;
        vector<SymbolList> 					localParameterList;
        SymbolList 							reactionList;
        int                                 getNumberOfReactions();
        int                                 NumAdditionalRates();	//this variable is the size of _oMapRateRule
        StringList                          getBoundarySpeciesList();
        StringList                          getCompartmentList();
        StringList                          getConservationList();
        StringList                          getFloatingSpeciesConcentrationList();
        StringList                          getGlobalParameterList();
        StringList                          getLocalParameterList(int reactionId);
        StringList  	                   	getReactionNames();
        StringList	                        Warnings;
        SymbolList 							ModifiableSpeciesReferenceList;
    	string 								WriteDouble(const double& value);

    	// Generates the Model Code from the SBML string
    	string 								generateModelCode(const string& sbmlStr);
};


}

#endif

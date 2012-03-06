#ifndef rrCSharpGeneratorH
#define rrCSharpGeneratorH
//---------------------------------------------------------------------------
#include "rrModelGenerator.h"

namespace rr
{

class RR_DECLSPEC CSharpGenerator : public ModelGenerator
{
	protected:
        string                              convertUserFunctionExpression(const string& equation);
        void 								SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& sb);
        void 								SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb);
        void 								SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb);
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

    public:
									        CSharpGenerator();
		virtual						       ~CSharpGenerator();

    	// Generates the Model Code from the SBML string
    	string 								generateModelCode(const string& sbmlStr);
};


}
#endif

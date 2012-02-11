#ifndef rrNOMWrapperH
#define rrNOMWrapperH
//---------------------------------------------------------------------------
#include <vector>
#include <string>
#include "NOMLib.h"
#include "rrObject.h"
#include "rrStringCollection.h"
using std::vector;
using std::string;

//---------------------------------------------------------------------------
namespace rr
{

class RR_DECLSPEC NOMWrapper : public rrObject
{
    protected:
		//The C# SBMLSupport.cs have two static objects, SBMLDocument and Model, created when NOM reads SBML..
    	SBMLDocument* 			mSBMLDoc;
		Model* 					mModel;

   	public:
						    	NOMWrapper();
    	virtual 			   ~NOMWrapper();

		string					getNthCompartmentId(const int& i);
        double					getValue(const string& id);
        StringCollections		GetFloatingSpecies();
        int						LoadSBML(const string& sbml);

//        static SBMLDocument _oDoc;
//        public static SBMLDocument Document
//        static Model _oModel;
//        public static Model Model
//        public static string GetAnnotatedModel(string targetSBML, string sourceSBML, bool checkModelId)
//        public static string GetId(SBase element)
//        public static string getMetaId(string sId)
//        private static string GetName(SBase element)
//        private static bool addMissingModifiers(Model oModel)
//        public static string addMissingModifiers(string sModel)
//        private static void checkForMissingNames(ASTNode node, StringCollection results, StringCollection symbols)
//        public static int checkConsistency()
//        public static string convertLevel1ToLevel2Impl(string sSBML)
//        public static string convertLevel2ToLevel1Impl(string sSBML)
//        public static string convertMathMLToString(string sMathML)
//        public static string convertPowImpl(string sSBML)
//        private static void changePow(ASTNode node)
//        public static string convertSBML(string sModel, int nLevel, int nVersion)
//        private static void RemoveSpatialSizeUnitsFromSpecies(SBMLDocument doc)
//        private static void RemoveTimeUnitsFromKineticLaws(SBMLDocument doc)
//        private static void RemoveSubstanceUnitsFromKineticLaws(SBMLDocument doc)
//        private static void AddMissingParameter(string parameterId, SBMLDocument doc)
//        private static void UpgradeToL2V4IfNecessary(SBMLDocument doc)
//        public static string getSBOCapableSBML(string sModel)
//        public static string convertSBML(string sModel, int nLevel, int nVersion, bool throwError)
//        public static string convertStringToMathML(string var0)
//        public static string convertTime(string sArg, string sTimeSymbol)
//        public static void ChangeConstantForRules(Model model)
//        public static string FixCommonIssues(string sbml)
//        public static string FixCommonIssues(string sbml, string programName, string programVersion)
//        public static string convertTimeToCSymbol(string sArg, string sTimeSymbol)
//        private static void changeSymbol(Model oModel, string sTimeSymbol, int targetType)
//        private static void ChangeNameToCSymbol(Model model, string name, int type)
//        private static ASTNode changeSymbol(ASTNode node, string time, int targetType)
//        public static ASTNode ReplaceSymbol(ASTNode node, string oldId, string newId)
//        private static ASTNode changeTimeToCSymbol(ASTNode node, string name, int type)
//        public static bool exists(string sId)
//        public static string getAnnotation(string sId)
//        public static string[] getBuiltinFunctionInfo(string var0)
//        public static string[] getBuiltinFunctions()
//        public static string getCompartmentIdBySpeciesId(string sId)
//        public static ArrayList getDerivedUnitDefinition(string sId)
//        private static ArrayList returnUnitDefinition(UnitDefinition oDefinition)
//        public static string getKineticLaw(int index)
//        public static ArrayList getListOfBoundarySpecies()
//        public static ArrayList getListOfBoundarySpeciesIds()
//        public static ArrayList getListOfErrors()
//        public static ArrayList getListOfFloatingSpecies()
//        public static ArrayList getListOfFloatingSpeciesIds()
//        public static ArrayList getListOfParameters()
//        public static string getModelId()
//        public static string getModelName()
//        public static string getNotes(string sId)
//        public static string getNthBoundarySpeciesCompartmentName(int nIndex)
//        public static string getNthBoundarySpeciesId(int nIndex)
//        public static string getNthBoundarySpeciesName(int nIndex)
//        public static string getNthCompartmentId(int nIndex)
//        public static string getNthCompartmentName(int nIndex)
//        public static ArrayList getNthError(int nIndex)
//        public static bool getNthUseValuesFromTriggerTime(int arg)
//        public static ArrayList getNthEvent(int arg)
		string getNthFloatingSpeciesCompartmentName(int nIndex);
//        public static string getNthFloatingSpeciesId(int nIndex)
//        public static string getNthFloatingSpeciesName(int nIndex)
//        public static ArrayList getNthFunctionDefinition(int arg)
//        public static string getNthGlobalParameterId(int nIndex)
//        public static string getNthGlobalParameterName(int nIndex)
//        public static ArrayList getNthListOfModifiers(int nIndex)
//        public static ArrayList getNthListOfProducts(int nIndex)
//        public static ArrayList getNthListOfReactants(int nIndex)
//        public static bool getNthParameterHasValue(int nReactionIndex, int nParameterIndex)
//        public static string getNthParameterId(int nReactionIndex, int nParameterIndex)
//        public static string getNthParameterName(int nReactionIndex, int nParameterIndex)
//        public static double getNthParameterValue(int nReactionIndex, int nParameterIndex)
//        public static string getNthProductName(int nIndex, int nProduct)
//        public static int getNthProductStoichiometry(int nIndex, int nProduct)
//        public static double getNthProductStoichiometryDouble(int nIndex, int nProduct)
//        public static string getNthReactantName(int nIndex, int nReactant)
//        public static int getNthReactantStoichiometry(int nIndex, int nReactant)
//        public static double getNthReactantStoichiometryDouble(int nIndex, int nReactant)
//        public static string getNthReactionId(int nIndex)
//        public static string getNthReactionName(int nIndex)
//        public static Pair<string, string> getNthInitialAssignmentPair(int nIndex)
//        public static string getNthInitialAssignment(int nIndex)
//        public static string getNthConstraint(int nIndex, out string sMessage)
//        public static string getNthRule(int nIndex)
//        public static string getNthRuleType(int arg)
//        public static int getNumBoundarySpecies()
//        public static int getNumCompartments()
//        public static int getNumErrors()
//        public static int getNumEvents()
//        public static int getNumFloatingSpecies()
//        public static int getNumInitialAssignments()
//        public static int getNumConstraints()
//        public static int getNumFunctionDefinitions()
//        public static int getNumGlobalParameters()
//        public static int getNumParameters(int var0)
//        public static int getNumProducts(int var0)
//        public static int getNumReactants(int var0)
//        public static int getNumReactions()
//        public static int getNumRules()
//        public static string getOutsideCompartment(string var0)
//        public static string getParamPromotedSBML(string sArg)
//        private static void modifyKineticLawsForLocalParameters(KineticLaw oLaw, string reactionId, Model oModel)
//        private static void modifyKineticLawsForReaction(KineticLaw oLaw, string reactionId, Model oModel)
//        private static void modifyKineticLaws(SBMLDocument oSBMLDoc, Model oModel)
//        private static void ChangeParameterName(ASTNode node, string sParameterName, string sPrefix)
//        public static string getSBML()
//        public static int getSBOTerm(string sId)
//        public static void TestASTTime()
//        public static double getValue(string sId)
//        public static bool hasInitialAmount(string sId)
//        public static bool hasInitialConcentration(string sId)
//        public static bool hasSBOTerm(string sId)
//        public static bool hasValue(string sId)
//        public static bool isConstantImpl(string sId)
//        public static bool isReactionReversible(int nIndex)
//        private static void GetSymbols(ASTNode node, List<System.String> list)
//        public static List<string> GetSymbols(ASTNode math)
//        public static List<Rule> ReorderAssignmentRules(List<Rule> assignmentRules)
//        public static void ReorderRules(SBMLDocument doc, Model model)
//        public static void loadSBML(string var0, string sTimeSymbol)
//        private static void changeTimeSymbol(Model model, string timeSymbol)
//        public static void loadParameterPromotedSBML(string var0, string sTimeSymbol)
//        public static void loadFromFile(string fileName)
//        static Hashtable _symbolTable = new Hashtable();
//        static private void BuildSymbolTable()
//        private static void LookForDependencies()
//        private static void UpdateDependencies(string sbmlId)
//        private static List<string> GetSymbols(string formula)
//        private static void addDependenciesToList(ASTNode node, List<string> sResult)
//        private static string GetRuleFor(string sbmlId)
//        private static string GetInitialAssignmentFor(string sbmlId)
//        private static List<string> _Namespaces;
//        public static List<string> Namespaces
//        public static void loadSBML(string var0)
//        private static ParameterSets _ParameterSets;
//        public static ParameterSets ParameterSets
//        public static void setAnnotation(string sId, string sAnnotation)
//        public static void setModelId(string sId)
//        public static void setNotes(string sId, string sNotes)
//        public static void setSBOTerm(string sId, int nSBOTerm)
//        public static void setValue(Model model, string id, double value, bool throwIfNotFound)
//        public static void setValue(string sId, double dValue)
//        public static string validateSBML(string sModel)
//        public static string validateWithConsistency(string sModel)
//        public static SBMLDocument SbmlDocument
//        public static Model SbmlModel
//        public static bool IsCompartment(string sId)
//        public static bool IsSpecies (string sId)
//        public static bool IsFloating(string sId)
//        public static SBase GetElement(string sId)
//        public static bool IsBoundary(string sId)
//        public static bool MultiplyCompartment(string sbmlId, out string compartmentId)
//        public static Stack<string> GetMatchForSymbol(string sbmlId)
//        public static void FillStack(Stack<string> stack, SBMLSymbol symbol)
//        public static string addSourceSinkNodes(string sbml)
//        public static bool NeedSourceNode(Model model)
//        public static bool NeedSinkNode(Model model)
//        public static bool NeedEmptySetNode(Model model)
//        public static string addEmptySetNodes(string sbml)
//        public static string addEmptySetNode(string sbml)
//        public static string RemoveJD2Layout(string sSBML)
//        public static string RemoveJD1Layout(string sSBML)
//        public static string RemoveLayoutInformation(string sSBML)
};

}


#endif

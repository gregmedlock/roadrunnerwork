#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include <cmath>
#include <stack>
#include "sbml/Model.h"
#include "sbml/SBMLDocument.h"
#include "rrModelGenerator.h"
//#include "NOMLib.h"
#include "libstructural.h"
#include "rrStringListContainer.h"
#include "rrUtils.h"
#include "rrRule.h"
#include "rrScanner.h"
#include "rrLogger.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


using namespace std;
using namespace LIB_STRUCTURAL;

namespace rr
{
ModelGenerator::ModelGenerator()
:
mStructAnalysis(),
STR_DoubleFormat("%.15G"),
STR_FixAmountCompartments("*")
{
	mNOM.Reset();
    mStructAnalysis.Reset();
}

ModelGenerator::~ModelGenerator(){}

void ModelGenerator::Reset()
{
	mNOM.Reset();
    mStructAnalysis.Reset();
}

int ModelGenerator::NumAdditionalRates()
{
	return _oMapRateRule.size();
}

// Generates the Model Code from the SBML string
string ModelGenerator::generateModelCode(const string& sbmlStr)
{
	Log(lDebug4)<<"Entering ModelGenerators generateModelCode(string) function";
    string sASCII = sbmlStr; //Encoding.ASCII.GetString(Encoding.ASCII.GetBytes(sbmlStr));
    StringList  Warnings;
    StringBuilder sb;
	mNOM.Reset();
    sASCII = mNOM.convertTime(sASCII, "time");

	Log(lDebug4)<<"Loading SBML into NOM";
	mNOM.loadSBML(sASCII.c_str(), "time");


    _ModelName = mNOM.getModelName();
    if(!_ModelName.size())
    {
        Log(lError)<<"Model name is empty! Exiting...";
    	return "";
    }

    Log(lDebug3)<<"Model name is "<<_ModelName;
    _NumReactions = mNOM.getNumReactions();

    Log(lDebug3)<<"Number of reactions:"<<_NumReactions;

	globalParameterList.Clear();
    ModifiableSpeciesReferenceList.Clear();
    localParameterList.reserve(_NumReactions);
    reactionList.Clear();
    boundarySpeciesList.Clear();
    floatingSpeciesConcentrationList.Clear();
    floatingSpeciesAmountsList.Clear();
    compartmentList.Clear();
    conservationList.Clear();
    _functionNames.empty();
    _functionParameters.empty();

   	LibStructural* instance = LibStructural::getInstance();
	string msg;
    try
    {
		Log(lDebug)<<"Loading sbml into StructAnalysis";
	    msg = mStructAnalysis.LoadSBML(sASCII);
	    if(!msg.size())
    	{
			Log(lError)<<"Failed loading sbml into StructAnalysis";
	    }
    }
    catch(...)
    {
		Log(lError)<<"Failed loading sbml into StructAnalysis";
    }

    // Are the list scrambled here??
    StringList reOrderedList = mStructAnalysis.GetReorderedSpeciesIds();

//    Log(lDebug5)<<reOrderedList;

	Log(lDebug3)<<"Message from StructAnalysis.LoadSBML function\n"<<msg;

	if (RoadRunner::_bComputeAndAssignConservationLaws)
    {
        _NumIndependentSpecies = mStructAnalysis.GetNumIndependentSpecies();
        independentSpeciesList = mStructAnalysis.GetIndependentSpeciesIds();
        dependentSpeciesList   = mStructAnalysis.GetDependentSpeciesIds();
    }
    else
    {
        _NumIndependentSpecies = mStructAnalysis.GetNumSpecies();
        independentSpeciesList = mStructAnalysis.GetSpeciesIds();
    }

    sb.Append("//************************************************************************** " + NL());

    // Load the compartment array (name and value)
	_NumCompartments 		= ReadCompartments();

    // Read FloatingSpecies
    _NumFloatingSpecies 	= ReadFloatingSpecies();
    _NumDependentSpecies 	= _NumFloatingSpecies - _NumIndependentSpecies;

    // Load the boundary species array (name and value)
	_NumBoundarySpecies 	= ReadBoundarySpecies();

    // Get all the parameters into a list, global and local
    _NumGlobalParameters 	= ReadGlobalParameters();
	_NumModifiableSpeciesReferences = ReadModifiableSpeciesReferences();

    // Load up local parameters next
	ReadLocalParameters(_NumReactions, _LocalParameterDimensions, _TotalLocalParmeters);
    _NumEvents = mNOM.getNumEvents();

    WriteClassHeader(sb);
    WriteOutVariables(sb);
    WriteOutSymbolTables(sb);
    WriteResetEvents(sb, _NumEvents);
    WriteSetConcentration(sb);
    WriteGetConcentration(sb);
    WriteConvertToAmounts(sb);
    WriteConvertToConcentrations(sb);
    WriteProperties(sb);
    WriteAccessors(sb);
    WriteUserDefinedFunctions(sb);
    WriteSetInitialConditions(sb, _NumFloatingSpecies);
    WriteSetBoundaryConditions(sb);
    WriteSetCompartmentVolumes(sb);
    WriteSetParameterValues(sb, _NumReactions);

//    if(mStructAnalysis.GetGammaMatrix()) //Todo: If there is no Gamma matrix, some problems in the next functions?
    {
    	WriteComputeConservedTotals(sb, _NumFloatingSpecies, _NumDependentSpecies);
    }

    // Get the L0 matrix
    int nrRows;
    int nrCols;
    double* aL0 = InitializeL0(nrRows, nrCols); 	//Todo: What is this doing? answer.. it is used below..
    DoubleMatrix L0(aL0,nrRows, nrCols); 		//How many rows and cols?? We need to know that in order to use the matrix properly!

    WriteUpdateDependentSpecies(sb, _NumIndependentSpecies, _NumDependentSpecies, L0);
    int numOfRules = WriteComputeRules(sb, _NumReactions);

    WriteComputeAllRatesOfChange(sb, _NumIndependentSpecies, _NumDependentSpecies, L0);
    WriteComputeReactionRates(sb, _NumReactions);
    WriteEvalModel(sb, _NumReactions, _NumIndependentSpecies, _NumFloatingSpecies, numOfRules);
    WriteEvalEvents(sb, _NumEvents, _NumFloatingSpecies);
    WriteEventAssignments(sb, _NumReactions, _NumEvents);
    WriteEvalInitialAssignments(sb, _NumReactions);
    WriteTestConstraints(sb);
	sb.AppendFormat("}{0}{0}", NL());
	return sb.ToString();
}


////        public string generateModelCode(string sbmlStr)
////        {
////            string sASCII = Encoding.ASCII.GetString(Encoding.ASCII.GetBytes(sbmlStr));
////            Warnings = new List<string>();
////            var sb = new StringBuilder();
////            sASCII = NOM.convertTime(sASCII, "time");
////            NOM.loadSBML(sASCII, "time");
////
////            _ModelName = NOM.getModelName();
////            _NumReactions = NOM.getNumReactions();
////
////            globalParameterList = new SymbolList();
////            ModifiableSpeciesReferenceList = new SymbolList();
////
////            localParameterList = new SymbolList[_NumReactions];
////            reactionList = new SymbolList();
////            boundarySpeciesList = new SymbolList();
////            floatingSpeciesConcentrationList = new SymbolList();
////            floatingSpeciesAmountsList = new SymbolList();
////            compartmentList = new SymbolList();
////            conservationList = new SymbolList();
////            _functionNames = new ArrayList();
////            _functionParameters = new StringCollection();
////
////            StructAnalysis.LoadSBML(sASCII);
////
////            if (RoadRunner._bComputeAndAssignConservationLaws)
////            {
////                _NumIndependentSpecies = StructAnalysis.GetNumIndependentSpecies();
////                independentSpeciesList = StructAnalysis.GetIndependentSpeciesIds();
////                dependentSpeciesList = StructAnalysis.GetDependentSpeciesIds();
////            }
////            else
////            {
////                _NumIndependentSpecies = StructAnalysis.GetNumSpecies();
////                independentSpeciesList = StructAnalysis.GetSpeciesIds();
////                dependentSpeciesList = new string[0];
////            }
////
////            sb.Append("//************************************************************************** " + NL());
////
////            // Load the compartment array (name and value)
////            _NumCompartments = ReadCompartments();
////
////            // Read FloatingSpecies
////            _NumFloatingSpecies = ReadFloatingSpecies();
////
////            _NumDependentSpecies = _NumFloatingSpecies - _NumIndependentSpecies;
////
////            // Load the boundary species array (name and value)
////            _NumBoundarySpecies = ReadBoundarySpecies();
////
////            // Get all the parameters into a list, global and local
////            _NumGlobalParameters = ReadGlobalParameters();
////
////            _NumModifiableSpeciesReferences = ReadModifiableSpeciesReferences();
////
////            // Load up local parameters next
////            ReadLocalParameters(_NumReactions, out _LocalParameterDimensions, out _TotalLocalParmeters);
////
////            _NumEvents = NOM.getNumEvents();
////
////            // Get the L0 matrix
////            double[][] L0 = InitializeL0();
////
////
////            WriteClassHeader(sb);
////
////            WriteOutVariables(sb);
////
////            WriteOutSymbolTables(sb);
////
////            WriteResetEvents(sb, _NumEvents);
////
////            WriteSetConcentration(sb);
////
////            WriteGetConcentration(sb);
////
////            WriteConvertToAmounts(sb);
////
////            WriteConvertToConcentrations(sb);
////
////            WriteProperties(sb);
////
////            WriteAccessors(sb);
////
////            WriteUserDefinedFunctions(sb);
////
////            WriteSetInitialConditions(sb, _NumFloatingSpecies);
////
////            WriteSetBoundaryConditions(sb);
////
////            WriteSetCompartmentVolumes(sb);
////
////            WriteSetParameterValues(sb, _NumReactions);
////
////            WriteComputeConservedTotals(sb, _NumFloatingSpecies, _NumDependentSpecies);
////
////            WriteUpdateDependentSpecies(sb, _NumIndependentSpecies, _NumDependentSpecies, L0);
////
////            int numOfRules = WriteComputeRules(sb, _NumReactions);
////
////            WriteComputeAllRatesOfChange(sb, _NumIndependentSpecies, _NumDependentSpecies, L0);
////
////            WriteComputeReactionRates(sb, _NumReactions);
////
////            WriteEvalModel(sb, _NumReactions, _NumIndependentSpecies, _NumFloatingSpecies, numOfRules);
////
////            WriteEvalEvents(sb, _NumEvents, _NumFloatingSpecies);
////
////            WriteEventAssignments(sb, _NumReactions, _NumEvents);
////
////            WriteEvalInitialAssignments(sb, _NumReactions);
////
////            WriteTestConstraints(sb);
////
////            sb.AppendFormat("}}{0}{0}", NL());
////
////            return sb.ToString();
////        }
////    }


string ModelGenerator::convertSpeciesToY(const string& speciesName)
{
    int index;
    if (floatingSpeciesConcentrationList.find(speciesName, index))
    {
        return "_y[" + ToString(index) + "]";
    }
    throw new SBWApplicationException("Internal Error: Unable to locate species: " + speciesName);
}

string ModelGenerator::convertSpeciesToBc(const string& speciesName)
{
    int index;
    if (boundarySpeciesList.find(speciesName, index))
    {
        return "_bc[" + ToString(index) + "]";
    }
	throw SBWApplicationException("Internal Error: Unable to locate species: " + speciesName);
}


string ModelGenerator::convertCompartmentToC(const string& compartmentName)
{
    int index;
    if (compartmentList.find(compartmentName, index))
    {
        return "_c[" + ToString(index) + "]";
    }

    throw RRException("Internal Error: Unable to locate compartment: " + compartmentName);
}

string ModelGenerator::convertSymbolToGP(const string& parameterName)
{
    int index;
    if (globalParameterList.find(parameterName, index))
    {
        return "_gp[" + ToString(index) + "]";
    }
      throw SBWApplicationException("Internal Error: Unable to locate parameter: " + parameterName);
}

string ModelGenerator::convertSymbolToC(const string& compartmentName)
{
	int index;
    if (compartmentList.find(compartmentName, index))
    {
        return "_c[" + ToString(index) + "]";
    }
      throw SBWApplicationException("Internal Error: Unable to locate compartment: " + compartmentName);
}

StringList ModelGenerator::getCompartmentList()
{
    StringList tmp;
    for (int i = 0; i < compartmentList.size(); i++)
    {
        tmp.Add(compartmentList[i].name);
    }
    return tmp;
}

string ModelGenerator::convertUserFunctionExpression(const string& equation)
{
	if(!equation.size())
    {
    	Log(lError)<<"The equation string supplied to "<<__FUNCTION__<<" is empty";
        return "";
    }
    Scanner s;
    stringstream ss;
    ss<<equation;
    s.AssignStream(ss);
    s.startScanner();
    s.nextToken();
    StringBuilder  sb;

    try
    {
		while (s.token() != CodeTypes::tEndOfStreamToken)
       	{
        	string theToken = s.tokenString;
           	switch (s.token())
           	{
            	case CodeTypes::tWordToken:
					if(theToken == "pow")
					{
                    	sb.Append("Math.Pow");
                    }
					else if(theToken == "sqrt")
                    {
                        sb.Append("Math.Sqrt");
                  	}
                    else if(theToken == "log")
                    {
                    	sb.Append("supportFunctions._log");
                    }
                    else if(theToken == "log10")
                    {
                        sb.Append("Math.Log10");
                    }
                    else if(theToken == "floor")
                    {
                        sb.Append("Math.Floor");
                    }
                    else if(theToken == "ceil")
                    {
                    	sb.Append("Math.Ceiling");
                    }
                    else if(theToken == "factorial")
                    {
                    	sb.Append("supportFunctions._factorial");
                    }
                    else if(theToken == "exp")
                    {
                    	sb.Append("Math.Exp");
                    }
                    else if(theToken == "sin")
                    {
                    	sb.Append("Math.Sin");
                    }
                    else if(theToken == "cos")
                    {
                        sb.Append("Math.Cos");
                    }
                    else if(theToken == "tan")
                    {
                        sb.Append("Math.Tan");
                    }
                    else if(theToken == "abs")
                    {
                        sb.Append("Math.Abs");
                    }
                    else if(theToken == "asin")
                    {
                        sb.Append("Math.Asin");
                    }
                    else if(theToken == "acos")
                    {
                        sb.Append("Math.Acos");
                    }
                    else if(theToken == "atan")
                    {
                    	sb.Append("Math.Atan");
                    }
                    else if(theToken == "sec")
                    {
                        sb.Append("MathKGI.Sec");
                    }
                    else if(theToken == "csc")
                    {
                        sb.Append("MathKGI.Csc");
                    }
                    else if(theToken == "cot")
                    {
                        sb.Append("MathKGI.Cot");
                    }
                    else if(theToken == "arcsec")
                    {
                        sb.Append("MathKGI.Asec");
                    }
                    else if(theToken == "arccsc")
                    {
                        sb.Append("MathKGI.Acsc");
                    }
                    else if(theToken == "arccot")
                    {
                        sb.Append("MathKGI.Acot");
                    }
                    else if(theToken == "sinh")
                    {
                        sb.Append("Math.Sinh");
                    }
                    else if(theToken == "cosh")
                    {
                        sb.Append("Math.Cosh");
                    }
                    else if(theToken == "tanh")
                    {
                        sb.Append("Math.Tanh");
                    }
                    else if(theToken == "arcsinh")
                    {
                        sb.Append("MathKGI.Asinh");
                    }
                    else if(theToken == "arccosh")
                    {
                        sb.Append("MathKGI.Acosh");
                    }
                    else if(theToken == "arctanh")
                    {
                        sb.Append("MathKGI.Atanh");
                    }
                    else if(theToken == "sech")
                    {
                        sb.Append("MathKGI.Sech");
                    }
                    else if(theToken == "csch")
                    {
                        sb.Append("MathKGI.Csch");
                    }
                    else if(theToken == "coth")
                    {
                        sb.Append("MathKGI.Coth");
                    }
                    else if(theToken == "arcsech")
                    {
                        sb.Append("MathKGI.Asech");
                    }
                    else if(theToken == "arccsch")
                    {
                        sb.Append("MathKGI.Acsch");
                    }
                    else if(theToken == "arccoth")
                    {
                               sb.Append("MathKGI.Acoth");
                    }
                    else if(theToken == "pi")
                    {
                        sb.Append("Math.PI");
                    }
                    else if(theToken == "exponentiale")
                    {
                        sb.Append("Math.E");
                    }
                    else if(theToken == "avogadro")
                    {
                        sb.Append("6.02214179e23");
                    }
                    else if(theToken == "true")
                    {
                               //sb.Append("true");
                        sb.Append("1.0");
                    }
                    else if(theToken == "false")
                    {
                               //sb.Append("false");
                        sb.Append("0.0");
                    }
                    else if(theToken == "gt")
                    {
                        sb.Append("supportFunctions._gt");
                    }
                    else if(theToken == "lt")
                    {
                        sb.Append("supportFunctions._lt");
                    }
                    else if(theToken == "eq")
                    {
                        sb.Append("supportFunctions._eq");
                    }
                    else if(theToken == "neq")
                    {
                        sb.Append("supportFunctions._neq");
                    }
                    else if(theToken == "geq")
                    {
                        sb.Append("supportFunctions._geq");
                    }
                    else if(theToken == "leq")
                    {
                        sb.Append("supportFunctions._leq");
                    }
                    else if(theToken == "and")
                    {
                        sb.Append("supportFunction._and");
                    }
                    else if(theToken == "or")
                    {
                        sb.Append("supportFunction._or");
                    }
                    else if(theToken == "not")
                    {
                        sb.Append("supportFunction._not");
                    }
                    else if(theToken == "xor")
                    {
                        sb.Append("supportFunction._xor");
                    }
                    else if(theToken == "root")
                    {
                        sb.Append("supportFunctions._root");
                    }
                    else if(theToken == "piecewise")
                    {
                        sb.Append("supportFunctions._piecewise");
                    }
                    else if (!_functionParameters.Contains(s.tokenString))
                    {
                    	throw Exception("Token '" + s.tokenString + "' not recognized.");
                    }
                    else
                    {
                    	sb.Append(s.tokenString);
                	}

				break; //Word token

               	case CodeTypes::tDoubleToken:
                   	sb.Append(WriteDouble(s.tokenDouble));
                   	break;
               	case CodeTypes::tIntToken:
                	sb.Append((int) s.tokenInteger);
                   	break;
               	case CodeTypes::tPlusToken:
                   sb.Append("+");
                   break;
               	case CodeTypes::tMinusToken:
                   sb.Append("-");
                   break;
               	case CodeTypes::tDivToken:
                   sb.Append("/");
                   break;
               	case CodeTypes::tMultToken:
                   sb.Append(STR_FixAmountCompartments);
                   break;
               	case CodeTypes::tPowerToken:
                   sb.Append("^");
                   break;
               	case CodeTypes::tLParenToken:
                   sb.Append("(");
                   break;
               	case CodeTypes::tRParenToken:
                   sb.Append(")");
                   break;
               	case CodeTypes::tCommaToken:
                   sb.Append(",");
                   break;
               	case CodeTypes::tEqualsToken:
                   sb.Append(" = ");
                   break;
               	case CodeTypes::tTimeWord1:
                   sb.Append("time");
                   break;
               	case CodeTypes::tTimeWord2:
                   sb.Append("time");
                   break;
               	case CodeTypes::tTimeWord3:
                   sb.Append("time");
                   break;
               	case CodeTypes::tAndToken:
                   sb.Append("supportFunctions._and");
                   break;
               	case CodeTypes::tOrToken:
                   sb.Append("supportFunctions._or");
                   break;
               	case CodeTypes::tNotToken:
                   sb.Append("supportFunctions._not");
                   break;
               	case CodeTypes::tLessThanToken:
                   sb.Append("supportFunctions._lt");
                   break;
               	case CodeTypes::tLessThanOrEqualToken:
                   sb.Append("supportFunctions._leq");
                   break;
               	case CodeTypes::tMoreThanOrEqualToken:
                   sb.Append("supportFunctions._geq");
                   break;
               	case CodeTypes::tMoreThanToken:
                   sb.Append("supportFunctions._gt");
                   break;
               	case CodeTypes::tXorToken:
                   sb.Append("supportFunctions._xor");
                   break;
               	default:
                   stringstream msg;
                   msg<< "Unknown token in convertUserFunctionExpression: " << s.tokenToString(s.token()) <<
                           "Exception raised in Module:roadRunner, Method:convertUserFunctionExpression";
                   throw Exception(msg.str());
           	}
           	s.nextToken();
		}
	}
    catch (SBWApplicationException)
    {
       throw;
    }
    catch (Exception e)
    {
       throw new SBWApplicationException(e.Message);
    }
	return sb.ToString();
}

string ModelGenerator::substituteTerms(const int& numReactions, const string& reactionName, const string& equation)
{
    return substituteTerms(reactionName, equation, false);
}

void ModelGenerator::SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& sb)
{
	string theToken(s.tokenString);
    if(theToken == "pow")
    {
        sb.Append("Math.Pow");
    }
    else if(theToken == "sqrt")
    {
        sb.Append("Math.Sqrt");
    }
    else if(theToken == "log")
    {
        sb.Append("supportFunctions._log");
    }
    else if(theToken == "floor")
    {
        sb.Append("Math.Floor");
    }
    else if(theToken == "ceil")
    {
        sb.Append("Math.Ceiling");
    }
    else if(theToken == "factorial")
    {
        sb.Append("supportFunctions._factorial");
    }
    else if(theToken == "log10")
    {
        sb.Append("Math.Log10");
    }
    else if(theToken == "exp")
    {
        sb.Append("Math.Exp");
    }
    else if(theToken == "abs")
    {
        sb.Append("Math.Abs");
    }
    else if(theToken == "sin")
    {
        sb.Append("Math.Sin");
    }
    else if(theToken == "cos")
    {
        sb.Append("Math.Cos");
    }
    else if(theToken == "tan")
    {
        sb.Append("Math.Tan");
    }
    else if(theToken == "asin")
    {
        sb.Append("Math.Asin");
    }
    else if(theToken == "acos")
    {
        sb.Append("Math.Acos");
    }
    else if(theToken == "atan")
    {
        sb.Append("Math.Atan");
    }
    else if(theToken == "sec")
    {
        sb.Append("MathKGI.Sec");
    }
    else if(theToken == "csc")
    {
        sb.Append("MathKGI.Csc");
    }
    else if(theToken == "cot")
    {
        sb.Append("MathKGI.Cot");
    }
    else if(theToken == "arcsec")
    {
        sb.Append("MathKGI.Asec");
    }
    else if(theToken == "arccsc")
    {
        sb.Append("MathKGI.Acsc");
    }
    else if(theToken == "arccot")
    {
        sb.Append("MathKGI.Acot");
    }
    else if(theToken == "sinh")
    {
        sb.Append("Math.Sinh");
    }
    else if(theToken == "cosh")
    {
        sb.Append("Math.Cosh");
    }
    else if(theToken == "tanh")
    {
        sb.Append("Math.Tanh");
    }
    else if(theToken == "arcsinh")
    {
        sb.Append("MathKGI.Asinh");
    }
    else if(theToken == "arccosh")
    {
        sb.Append("MathKGI.Acosh");
    }
    else if(theToken == "arctanh")
    {
        sb.Append("MathKGI.Atanh");
    }
    else if(theToken == "sech")
    {
        sb.Append("MathKGI.Sech");
    }
    else if(theToken == "csch")
    {
        sb.Append("MathKGI.Csch");
    }
    else if(theToken == "coth")
    {
        sb.Append("MathKGI.Coth");
    }
    else if(theToken == "arcsech")
    {
        sb.Append("MathKGI.Asech");
    }
    else if(theToken == "arccsch")
    {
        sb.Append("MathKGI.Acsch");
    }
    else if(theToken == "arccoth")
    {
        sb.Append("MathKGI.Acoth");
    }
    else if(theToken == "pi")
    {
        sb.Append("Math.PI");
    }
    else if(theToken == "avogadro")
    {
        sb.Append("6.02214179e23");
    }
    else if(theToken == "exponentiale")
    {
        sb.Append("Math.E");
    }
    else if(theToken == "true")
    {
        //sb.Append("true");
        sb.Append("1.0");
    }
    else if(theToken == "false")
    {
        //sb.Append("false");
        sb.Append("0.0");
    }
    else if(theToken == "NaN")
    {
        sb.Append("double.NaN");
    }
    else if(theToken == "INF")
    {
        sb.Append("double.PositiveInfinity");
    }
    else if(theToken == "geq")
    {
        sb.Append("supportFunctions._geq");
    }
    else if(theToken == "leq")
    {
        sb.Append("supportFunctions._leq");
    }
    else if(theToken == "gt")
    {
        sb.Append("supportFunctions._gt");
    }
    else if(theToken == "lt")
    {
        sb.Append("supportFunctions._lt");
    }
    else if(theToken == "eq")
    {
        sb.Append("supportFunctions._eq");
    }
    else if(theToken == "neq")
    {
        sb.Append("supportFunctions._neq");
    }
    else if(theToken == "and")
    {
        sb.Append("supportFunction._and");
    }
    else if(theToken == "or")
    {
        sb.Append("supportFunction._or");
    }
    else if(theToken == "not")
    {
        sb.Append("supportFunction._not");
    }
    else if(theToken == "xor")
    {
        sb.Append("supportFunction._xor");
    }
    else if(theToken == "root")
    {
        sb.Append("supportFunctions._root");
    }
    else if(theToken == "piecewise")
    {
        sb.Append("supportFunctions._piecewise");
    }
    else if(theToken == "delay")
    {
        sb.Append("supportFunctions._delay");
        Warnings.Add("RoadRunner does not yet support delay differential equations in SBML, they will be ignored (i.e. treated as delay = 0).");
    }
    else
    {
        bool bReplaced = false;
        int index;
        if (reactionList.find(reactionName, index))
        {
            int nParamIndex = 0;
            if (localParameterList[index].find(s.tokenString, nParamIndex))
            {
                sb.Append("_lp[" + ToString(index) + "][" + ToString(nParamIndex) + "]");
                bReplaced = true;
            }
        }

        if (boundarySpeciesList.find(s.tokenString, index))
        {
            sb.Append("_bc[" + ToString(index) + "]");
            bReplaced = true;
        }
        if (!bReplaced &&
            (_functionParameters.size() != 0 && !_functionParameters.Contains(s.tokenString)))
        {
            throw Exception("Token '" + s.tokenString + "' not recognized.");
        }
    }
}

void ModelGenerator::SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb)
{
    // Global parameters have priority
    int index;
    if (globalParameterList.find(s.tokenString, index))
    {
        sb.AppendFormat("_gp[{0}]", index);
    }
    else if (boundarySpeciesList.find(s.tokenString, index))
    {
        sb.AppendFormat("_bc[{0}]", index);

        Symbol symbol = boundarySpeciesList[index];
        if (symbol.hasOnlySubstance)
        {
            // we only store concentration for the boundary so we better
            // fix that.
            int nCompIndex = 0;
            if (compartmentList.find(symbol.compartmentName, nCompIndex))
            {
                sb.AppendFormat("{0}_c[{1}]", STR_FixAmountCompartments, nCompIndex);
            }
        }


        //if (!bFixAmounts) return;
        //
        //string compartmentId = "";
        //if (NOM.MultiplyCompartment(s.tokenString, out compartmentId))
        //{
        //    int nCompIndex = 0;
        //    if (compartmentId != NULL && compartmentList.find(compartmentId, out nCompIndex))
        //    {
        //        sb.AppendFormat("{0}_c[{1}]", STR_FixAmountCompartments, nCompIndex);
        //    }
        //}
    }
    else if (floatingSpeciesConcentrationList.find(s.tokenString, index))
    {
        Symbol floating1 = floatingSpeciesConcentrationList[index];
        if (floating1.hasOnlySubstance)
        {
            sb.AppendFormat("amounts[{0}]", index);
        }
        else
        {
            sb.AppendFormat("_y[{0}]", index);
        }

        //if (!bFixAmounts) return;
        //
        //
        //string compartmentId = "";
        //if (NOM.MultiplyCompartment(s.tokenString, out compartmentId))
        //{
        //    int nCompIndex = 0;
        //    if (compartmentId != NULL && compartmentList.find(compartmentId, out nCompIndex))
        //    {
        //        sb.AppendFormat("{0}_c[{1}]", STR_FixAmountCompartments, nCompIndex);
        //    }
        //}
    }
    else if (compartmentList.find(s.tokenString, index))
    {
        sb.AppendFormat("_c[{0}]", index);
    }
    else if (_functionNames.Contains(s.tokenString))
    {
        sb.AppendFormat("{0} ", s.tokenString);
    }
    else if (ModifiableSpeciesReferenceList.find(s.tokenString, index))
    {
        sb.AppendFormat("_sr[{0}]", index);
    }
    else if (reactionList.find(s.tokenString, index))
    {
        sb.AppendFormat("_rates[{0}]", index);
    }
    else
    {
        SubstituteEquation(reactionName, s, sb);
	}
}

void ModelGenerator::SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sb)
{
	string aToken = s.tokenString;
	CodeTypes codeType = s.token();
    switch(codeType)
    {
        case CodeTypes::tWordToken:
        case CodeTypes::tExternalToken:
        case CodeTypes::tExtToken:
            SubstituteWords(reactionName, bFixAmounts, s, sb);
            break;

        case CodeTypes::tDoubleToken:
            sb.Append("(double)" + WriteDouble(s.tokenDouble));
            break;
        case CodeTypes::tIntToken:
            sb.Append("(double)" + WriteDouble((double)s.tokenInteger));
            break;
        case CodeTypes::tPlusToken:
            sb.AppendFormat("+{0}\t", NL());
            break;
        case CodeTypes::tMinusToken:
            sb.AppendFormat("-{0}\t", NL());
            break;
        case CodeTypes::tDivToken:
            sb.AppendFormat("/{0}\t", NL());
            break;
        case CodeTypes::tMultToken:
            sb.AppendFormat("*{0}\t", NL());
            break;
        case CodeTypes::tPowerToken:
            sb.AppendFormat("^{0}\t", NL());
            break;
        case CodeTypes::tLParenToken:
            sb.Append("(");
            break;
        case CodeTypes::tRParenToken:
            sb.AppendFormat("){0}\t", NL());
            break;
        case CodeTypes::tCommaToken:
            sb.Append(",");
            break;
        case CodeTypes::tEqualsToken:
            sb.AppendFormat(" = {0}\t", NL());
            break;
      case CodeTypes::tTimeWord1:
            sb.Append("time");
            break;
        case CodeTypes::tTimeWord2:
            sb.Append("time");
            break;
        case CodeTypes::tTimeWord3:
            sb.Append("time");
            break;
        case CodeTypes::tAndToken:
            sb.AppendFormat("{0}supportFunctions._and", NL());
            break;
        case CodeTypes::tOrToken:
            sb.AppendFormat("{0}supportFunctions._or", NL());
            break;
        case CodeTypes::tNotToken:
            sb.AppendFormat("{0}supportFunctions._not", NL());
            break;
        case CodeTypes::tLessThanToken:
            sb.AppendFormat("{0}supportFunctions._lt", NL());
            break;
        case CodeTypes::tLessThanOrEqualToken:
            sb.AppendFormat("{0}supportFunctions._leq", NL());
            break;
        case CodeTypes::tMoreThanOrEqualToken:
            sb.AppendFormat("{0}supportFunctions._geq", NL());
            break;
        case CodeTypes::tMoreThanToken:
            sb.AppendFormat("{0}supportFunctions._gt", NL());
            break;
        case CodeTypes::tXorToken:
            sb.AppendFormat("{0}supportFunctions._xor", NL());
            break;
        default:
        string aToken = s.tokenToString(s.token());
        Exception ae = Exception(
                 Format("Unknown token in substituteTerms: {0}", aToken,
                 "Exception raised in Module:roadRunner, Method:substituteTerms"));
         throw ae;
    }
}


ASTNode* ModelGenerator::CleanEquation(ASTNode* astP)
{
	ASTNode& ast = *astP; //For convenience...

	if (ast.getType() == AST_PLUS && ast.getNumChildren() == 0)
    {
        ASTNode* result = new ASTNode(AST_INTEGER);
        result->setValue(0);
        return result;
    }
    else if (ast.getType() == AST_TIMES && ast.getNumChildren() == 0)
    {
        ASTNode* result = new ASTNode(AST_INTEGER);
        result->setValue(1);
        return result;
    }
    else if (ast.getType() == AST_PLUS && ast.getNumChildren() == 1)
    {
        return ast.getChild(0);
    }
    else if (ast.getType() == AST_TIMES && ast.getNumChildren() == 1)
    {
        return ast.getChild(0);
    }

    for (long i = ast.getNumChildren() - 1; i >= 0; i--)
    {
        ast.replaceChild(i, CleanEquation(ast.getChild(i)));
    }

    return astP;
}

string ModelGenerator::CleanEquation(const string& eqn)
{
    if (eqn.size() < 1)
    {
    	return "0";
    }
	string equation(eqn);
    if (equation == " + ")
    {
    	return "0";
    }

    if (equation == " * ")
    {
    	return "1";
    }

    ASTNode* ast = SBML_parseFormula(equation.c_str());
    if (ast == NULL)
    {
        // we are in trouble!
        if (EndsWith(equation, "* "))
        {
          	equation = equation.substr(0, equation.size() - 2);
        }

       	string sought("*  +");
        if(equation.find(sought) != string::npos)
        {
        	equation.replace(equation.find(sought), sought.size(), string("+"));
        }
        sought = ("*  -");
        if(equation.find(sought) != string::npos)
        {
	        equation = equation.replace(equation.find(sought), sought.size(), "-");
        }

        ast = SBML_parseFormula(equation.c_str());
        if (ast == NULL)
        {
        	return equation;
        }
    }

    ast = CleanEquation(ast);
    return SBML_formulaToString(ast);
}

string ModelGenerator::substituteTerms(const string& reactionName, const string& inputEquation, bool bFixAmounts)
{
	string equation = CleanEquation(inputEquation);
    if (equation.size() < 1)
    {
    	return string("0");
    }

     Scanner s;
     stringstream ss;
     ss<<equation;

     s.AssignStream(ss);
     s.startScanner();
     s.nextToken();
     StringBuilder sb;

    try
    {
    	while (s.token() != CodeTypes::tEndOfStreamToken)
       	{
        	SubstituteToken(reactionName, bFixAmounts, s, sb);
        	s.nextToken();
       	}
    }
    catch (SBWApplicationException)
    {
    	throw;
    }
    catch (Exception e)
    {
       throw new SBWApplicationException(e.Message);
    }
    return sb.ToString();
}

string ModelGenerator::NL()
{
	stringstream newLine;
    newLine << endl;
    return newLine.str();
}


double* ModelGenerator::InitializeL0(int& nrRows, int& nrCols)
{
	double* L0;
    try
    {
        if (_NumDependentSpecies > 0)
        {
        	vector<string> RowLabels;
            vector<string> ColumnLabels; //Todo: Filling these out here is meaningless?
            L0 = mStructAnalysis.GetL0Matrix(RowLabels, ColumnLabels);
            nrRows = RowLabels.size();
            nrCols = ColumnLabels.size();
        }
        else
        {
        	L0 = new double[1];//.Allocate(1,1);// = new double[0][];
		    nrRows = nrCols = 1;
        }
    }
    catch (Exception)
    {
	    nrRows = nrCols = 0;
        L0 = NULL;
    }
    return L0;
}

void ModelGenerator::WriteOutSymbolTables(StringBuilder& sb)
{
    sb.Append("\tvoid loadSymbolTables() {" + NL());

    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sb.AppendFormat("\t\tvariableTable[{0}] = \"{1}\";{2}", i, floatingSpeciesConcentrationList[i].name, NL());
    }

    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        sb.AppendFormat("\t\tboundaryTable[{0}] = \"{1}\";{2}", i, boundarySpeciesList[i].name, NL());
    }

	for (int i = 0; i < globalParameterList.size(); i++)
    {
		string name = globalParameterList[i].name;
       	sb.AppendFormat("\t\tglobalParameterTable[{0}] = \"{1}\";{2}", i, globalParameterList[i].name, NL());
    }
    sb.AppendFormat("\t}{0}{0}", NL());
}

int ModelGenerator::ReadFloatingSpecies()
{
    // Load a reordered list into the variable list.
    StringList reOrderedList;
    if ((RoadRunner::_bComputeAndAssignConservationLaws))
	{
       reOrderedList = mStructAnalysis.GetReorderedSpeciesIds();
	}
	else
	{
    	reOrderedList = mStructAnalysis.GetSpeciesIds();
    }

    StringListContainer oFloatingSpecies = mNOM.getListOfFloatingSpecies();

	for (int i = 0; i < reOrderedList.size(); i++)
    {
    	for (int j = 0; j < oFloatingSpecies.size(); j++)
        {
        	StringList oTempList = oFloatingSpecies[j];
          	if(reOrderedList[i] != (const string&) oTempList[0])
          	{
          		continue;
          	}

			string compartmentName = mNOM.getNthFloatingSpeciesCompartmentName(j);
            bool bIsConcentration  = ToBool(oTempList[2]);
            double dValue = ToDouble(oTempList[1]);
            if (IsNaN(dValue))
            {
                  dValue = 0;
            }

            Symbol *symbol = NULL;
            if (bIsConcentration)
            {
              symbol = new Symbol(reOrderedList[i], dValue, compartmentName);
            }
            else
            {
              int nCompartmentIndex;
              compartmentList.find(compartmentName, nCompartmentIndex);

              double dVolume = compartmentList[nCompartmentIndex].value;
              if (IsNaN(dVolume))
              {
              	dVolume = 1;
              }

              stringstream formula;
              formula<<ToString(dValue,STR_DoubleFormat)<<"/ _c["<<nCompartmentIndex<<"]";

              symbol = new Symbol(reOrderedList[i],
                  dValue / dVolume,
                  compartmentName,
                  formula.str());
            }

            if(mNOM.GetModel())
            {
	            Species *aSpecies = mNOM.GetModel()->getSpecies(reOrderedList[i]);
                if(aSpecies)
                {
                    symbol->hasOnlySubstance = aSpecies->getHasOnlySubstanceUnits();
	                symbol->constant = aSpecies->getConstant();
                }
            }
            else
            {
                //TODO: How to report error...?
                //Log an error...
                symbol->hasOnlySubstance = false;
            }
			Log(lDebug5)<<"Adding symbol to floatingSpeciesConcentrationList:"<<(*symbol);
			floatingSpeciesConcentrationList.Add(*(symbol));
            break;
          }
          //throw RRException("Reordered Species " + reOrderedList[i] + " not found.");
      }
      return oFloatingSpecies.size();
}

int ModelGenerator::ReadBoundarySpecies()
{
	int numBoundarySpecies;
    StringListContainer oBoundarySpecies = mNOM.getListOfBoundarySpecies();
    numBoundarySpecies = oBoundarySpecies.size(); // sp1.size();
    for (int i = 0; i < numBoundarySpecies; i++)
    {
        StringList oTempList 	= oBoundarySpecies[i];
        string sName 			= oTempList[0];
        string compartmentName 	= mNOM.getNthBoundarySpeciesCompartmentName(i);
        bool bIsConcentration 	= ToBool(oTempList[2]);
        double dValue 			= ToDouble(oTempList[1]);
        if (IsNaN(dValue))
        {
        	dValue = 0;
        }

        Symbol *symbol = NULL;
        if (bIsConcentration)
        {
            symbol = new Symbol(sName, dValue, compartmentName);
        }
        else
        {
        	int nCompartmentIndex;
            double dVolume;
            if(compartmentList.find(compartmentName, nCompartmentIndex))
            {
            	dVolume = compartmentList[nCompartmentIndex].value;
            }
            else
            {
                if (IsNaN(dVolume))
                {
                    dVolume = 1;
                }
            }
            stringstream formula;
            formula<<ToString(dValue, STR_DoubleFormat)<<"/ _c["<<nCompartmentIndex<<"]";
            symbol = new Symbol(sName,
            					dValue / dVolume,
                                compartmentName,
                                formula.str());
        }

        if(mNOM.GetModel())
        {
			Species* species = mNOM.GetModel()->getSpecies(sName);
        	if(species)
            {
		        symbol->hasOnlySubstance = species->getHasOnlySubstanceUnits();
	            symbol->constant = species->getConstant();
            }
        }
        else
        {
        	//TODO: How to report error...?
			//Log an error...
            symbol->hasOnlySubstance = false;

        }
        boundarySpeciesList.Add(*symbol);
    }
    return numBoundarySpecies;
}

int ModelGenerator::ReadGlobalParameters()
{
    int numGlobalParameters;
    ArrayList oParameters = mNOM.getListOfParameters();
    numGlobalParameters = oParameters.Count();
    for (int i = 0; i < numGlobalParameters; i++)
    {
    	StringList parameter = oParameters[i];

        string name 	= parameter[0];
        double value 	= ToDouble(parameter[1]);
        Symbol aSymbol(name, value);
        Log(lDebug5)<<"Adding symbol"<<aSymbol<<" to global parameters";

        globalParameterList.Add(aSymbol);
    }
    return numGlobalParameters;
}

//Todo: totalLocalParmeters is not used
void ModelGenerator::ReadLocalParameters(const int& numReactions,  vector<int>& localParameterDimensions, int& totalLocalParmeters)
{
    string name;
    double value;
    int numLocalParameters;
    totalLocalParmeters = 0;
    string reactionName;
	localParameterDimensions.resize(numReactions);
    for (int i = 0; i < numReactions; i++)
    {
        numLocalParameters = mNOM.getNumParameters(i);
        reactionName = mNOM.getNthReactionId(i);
        reactionList.Add(Symbol(reactionName, 0.0));
        SymbolList newList;
        //localParameterList[i] = new SymbolList(); //TK redid the logic here a little
        for (int j = 0; j < numLocalParameters; j++)
        {
            localParameterDimensions[i] = numLocalParameters;
            name = mNOM.getNthParameterId(i, j);
            value = mNOM.getNthParameterValue(i, j);
            //localParameterList[i].Add(new Symbol(reactionName, name, value));
            newList.Add(Symbol(reactionName, name, value));
        }
        localParameterList.push_back(newList);
    }
}

void ModelGenerator::WriteComputeAllRatesOfChange(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    sb.Append("\t// Uses the equation: dSd/dt = L0 dSi/dt" + NL());
    sb.Append("\tpublic void computeAllRatesOfChange ()" + NL());
    sb.Append("\t{" + NL());
    sb.Append("\t\tdouble[] dTemp = new double[amounts.Length + rateRules.Length];" + NL());
    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        sb.AppendFormat("\t\tdTemp[{0}] = {1};{2}", i, _oMapRateRule[i], NL());
    }
    //sb.Append("\t\trateRules.CopyTo(dTemp, 0);" + NL());
    sb.Append("\t\tamounts.CopyTo(dTemp, rateRules.Length);" + NL());
    sb.Append("\t\tevalModel (time, dTemp);" + NL());
    bool isThereAnEntry = false;
    for (int i = 0; i < numDependentSpecies; i++)
    {
        sb.AppendFormat("\t\t_dydt[{0}] = ", (numIndependentSpecies + i));
        isThereAnEntry = false;
        for (int j = 0; j < numIndependentSpecies; j++)
        {
            string dyName = Format("_dydt[{0}]", j);

            if (L0(i,j) > 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == 1)
                {
                    sb.AppendFormat(" + {0}{1}", dyName, NL());
                }
                else
                {
                    sb.AppendFormat(" + (double){0}{1}{2}{3}", WriteDouble(L0(i,j)), STR_FixAmountCompartments, dyName, NL());
                }
            }
            else if (L0(i,j) < 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == -1)
                {
                    sb.AppendFormat(" - {0}{1}", dyName, NL());
                }
                else
                {
                    sb.AppendFormat(" - (double){0}{1}{2}{3}", WriteDouble(fabs(L0(i,j))), STR_FixAmountCompartments, dyName, NL());
                }
            }
        }
        if (!isThereAnEntry)
        {
            sb.Append("0");
        }
        sb.AppendFormat(";{0}", NL());
    }

    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteComputeConservedTotals(StringBuilder& sb, const int& numFloatingSpecies, const int& numDependentSpecies)
{
    sb.Append("\t// Uses the equation: C = Sd - L0*Si" + NL());
    sb.Append("\tpublic void computeConservedTotals ()" + NL());
    sb.Append("\t{" + NL());
    if (numDependentSpecies > 0)
    {
        string factor;
        double* matPtr = mStructAnalysis.GetGammaMatrix();

        DoubleMatrix gamma(matPtr, numDependentSpecies, numFloatingSpecies);
        for (int i = 0; i < numDependentSpecies; i++)
        {
            sb.AppendFormat("\t\t_ct[{0}] = ", i);
            for (int j = 0; j < numFloatingSpecies; j++)
            {
                double current = (matPtr != NULL) ? gamma(i,j) : 1.0;	//Todo: This is a bug? We should not be here if the matrix i NULL.. Triggered by model 00029

                if ( current != 0.0 )
                {
                    if (!matPtr)//IsNaN(current)) //C# code is doing one of these.. factor = "" .. ??
                    {
                        // TODO: fix this
                        factor = "";
                    }
                    else if (fabs(current) == 1.0)
                    {
                        factor = "";
                    }
                    else
                    {
                        factor = WriteDouble(fabs(current)) +
                                 STR_FixAmountCompartments;
                    }

                    if (current > 0)
                    {
                    	string cYY = convertSpeciesToY(floatingSpeciesConcentrationList[j].name);
                        string cTC = convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName);
                        sb.Append(" + " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                    else
                    {
                        sb.Append(" - " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                }
            }
            sb.Append(";" + NL());
            conservationList.Add(Symbol("CSUM" + ToString(i))); //TODO: how to deal with this?
        }
    }
    sb.Append("	}" + NL() + NL());
}

void ModelGenerator::WriteUpdateDependentSpecies(StringBuilder& sb, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    sb.Append("\t// Compute values of dependent species " + NL());
    sb.Append("\t// Uses the equation: Sd = C + L0*Si" + NL());
    sb.Append("\tpublic void updateDependentSpeciesValues (double[] y)" + NL());
    sb.Append("\t{" + NL());

    if (numDependentSpecies > 0)
    {
        // Use the equation: Sd = C + L0*Si to compute dependent concentrations
        if (numDependentSpecies > 0)
        {
            for (int i = 0; i < numDependentSpecies; i++)
            {
                sb.AppendFormat("\t\t_y[{0}] = {1}\t", (i + numIndependentSpecies), NL());
                sb.AppendFormat("(_ct[{0}]", i);
                string cLeftName =
                    convertCompartmentToC(
                        floatingSpeciesConcentrationList[i + numIndependentSpecies].compartmentName);

                for (int j = 0; j < numIndependentSpecies; j++)
                {
                    string yName = Format("y[{0}]", j);
                    string cName = convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName);
                    double* mat = L0.GetPointer();
                    double matElementValue = L0(i,j);

                    if (L0(i,j) > 0) // In C# code there is no checking for index out of bound..
                    {
                        if (L0(i,j) == 1)
                        {
                            sb.AppendFormat(" + {0}\t{1}{2}{3}{0}\t",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            sb.AppendFormat("{0}\t + (double){1}{2}{3}{2}{4}",
                                NL(),
                                WriteDouble(L0(i,j)),
                                STR_FixAmountCompartments,
                                yName,
                                cName);
                        }
                    }
                    else if (L0(i,j) < 0)
                    {
                        if (L0(i,j) == -1)
                        {
                            sb.AppendFormat("{0}\t - {1}{2}{3}",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            sb.AppendFormat("{0}\t - (double){1}{2}{3}{2}{4}",
                                NL(),
                                WriteDouble(fabs(L0(i,j))),
                                STR_FixAmountCompartments,
                                yName,
                                cName);
                        }
                    }
                }
                sb.AppendFormat(")/{0};{1}", cLeftName, NL());
            }
        }
    }
    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteUserDefinedFunctions(StringBuilder& sb)
{
	for (int i = 0; i < mNOM.getNumFunctionDefinitions(); i++)
    {
    	try
        {
        	StringListContainer oList = mNOM.getNthFunctionDefinition(i);
            StringList aList = oList[0];

          	string sName = aList[0];
          	//sName.Trim();
            _functionNames.Add(sName);
            StringList oArguments = oList[1];
            StringList list2 = oList[2];
            string sBody = list2[0];

            sb.AppendFormat("\t// User defined function:  {0}{1}", sName, NL());
            sb.AppendFormat("\tpublic double {0} (", sName);

            for (int j = 0; j < oArguments.size(); j++)
            {
                sb.Append("double " + (string)oArguments[j]);
                _functionParameters.Add((string)oArguments[j]);
                if (j < oArguments.size() - 1)
                    sb.Append(", ");
            }
            sb.Append(")" + NL() + "\t{" + NL() + "\t\t return " +
                      convertUserFunctionExpression(sBody)
                      + ";" + NL() + "\t}" + NL() + NL());
        }
        catch (const Exception& ex)
        {
        	StringBuilder msg;
            msg<<"Error while trying to get Function Definition #" << i <<ex.what() << "\r\n\r\n";
            throw Exception(msg.ToString());

        }
    }
}

void ModelGenerator::WriteResetEvents(StringBuilder& sb, const int& numEvents)
{
      sb.AppendFormat("{0}\tpublic void resetEvents() {{0}", NL());
      for (int i = 0; i < numEvents; i++)
      {
          sb.AppendFormat("\t\t_eventStatusArray[{0}] = false;{1}", i, NL());
          sb.AppendFormat("\t\t_previousEventStatusArray[{0}] = false;{1}", i, NL());
      }
      sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteSetConcentration(StringBuilder& sb)
{
    sb.AppendFormat("\tpublic void setConcentration(int index, double value) {{0}", NL());
    sb.AppendFormat("\t\tdouble volume = 0.0;{0}", NL());
    sb.AppendFormat("\t\t_y[index] = value;{0}", NL());
    sb.AppendFormat("\t\tswitch (index) {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
    	sb.AppendFormat("\t\t\tcase {0}: volume = {1};{2}",
          i,
          convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
          NL());
      sb.AppendFormat("\t\t\t\tbreak;{0}", NL());
    }
    sb.AppendFormat("\t\t}{0}", NL());
    sb.AppendFormat("\t\t_amounts[index] = _y[index]*volume;{0}", NL());
    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteGetConcentration(StringBuilder& sb)
{
    sb.AppendFormat("\tpublic double getConcentration(int index) {{0}", NL());
    sb.AppendFormat("\t\treturn _y[index];{0}", NL());
    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteConvertToAmounts(StringBuilder& sb)
{
    sb.AppendFormat("\tpublic void convertToAmounts() {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sb.AppendFormat("\t\t_amounts[{0}] = _y[{0}]*{1};{2}",
            i,
            convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
            NL());
    }
    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteConvertToConcentrations(StringBuilder& sb)
{
    sb.Append("\tpublic void convertToConcentrations() {" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sb<<"\t\t_y[" << i << "] = _amounts[" << i << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }
    sb.Append("\t}" + NL() + NL());
}

void ModelGenerator::WriteProperties(StringBuilder& sb)
{
    sb.Append("\tpublic double[] y {" + NL());
    sb.Append("\t\tget { return _y; } " + NL());
    sb.Append("\t\tset { _y = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] init_y {" + NL());
    sb.Append("\t\tget { return _init_y; } " + NL());
    sb.Append("\t\tset { _init_y = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] amounts {" + NL());
    sb.Append("\t\tget { return _amounts; } " + NL());
    sb.Append("\t\tset { _amounts = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] bc {" + NL());
    sb.Append("\t\tget { return _bc; } " + NL());
    sb.Append("\t\tset { _bc = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] gp {" + NL());
    sb.Append("\t\tget { return _gp; } " + NL());
    sb.Append("\t\tset { _gp = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] sr {" + NL());
    sb.Append("\t\tget { return _sr; } " + NL());
    sb.Append("\t\tset { _sr = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[][] lp {" + NL());
    sb.Append("\t\tget { return _lp; } " + NL());
    sb.Append("\t\tset { _lp = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] c {" + NL());
    sb.Append("\t\tget { return _c; } " + NL());
    sb.Append("\t\tset { _c = value; } " + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] dydt {" + NL());
    sb.Append("\t\tget { return _dydt; }" + NL());
    sb.Append("\t\tset { _dydt = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] rateRules {" + NL());
    sb.Append("\t\tget { return _rateRules; }" + NL());
    sb.Append("\t\tset { _rateRules = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] rates {" + NL());
    sb.Append("\t\tget { return _rates; }" + NL());
    sb.Append("\t\tset { _rates = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] ct {" + NL());
    sb.Append("\t\tget { return _ct; }" + NL());
    sb.Append("\t\tset { _ct = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] eventTests {" + NL());
    sb.Append("\t\tget { return _eventTests; }" + NL());
    sb.Append("\t\tset { _eventTests = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic TEventDelayDelegate[] eventDelay {" + NL());
    sb.Append("\t\tget { return _eventDelay; }" + NL());
    sb.Append("\t\tset { _eventDelay = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic bool[] eventType {" + NL());
    sb.Append("\t\tget { return _eventType; }" + NL());
    sb.Append("\t\tset { _eventType = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic bool[] eventPersistentType {" + NL());
    sb.Append("\t\tget { return _eventPersistentType; }" + NL());
    sb.Append("\t\tset { _eventPersistentType = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic bool[] eventStatusArray {" + NL());
    sb.Append("\t\tget { return _eventStatusArray; }" + NL());
    sb.Append("\t\tset { _eventStatusArray = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic bool[] previousEventStatusArray {" + NL());
    sb.Append("\t\tget { return _previousEventStatusArray; }" + NL());
    sb.Append("\t\tset { _previousEventStatusArray = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double[] eventPriorities {" + NL());
    sb.Append("\t\tget { return _eventPriorities; }" + NL());
    sb.Append("\t\tset { _eventPriorities = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic TEventAssignmentDelegate[] eventAssignments {" + NL());
    sb.Append("\t\tget { return _eventAssignments; }" + NL());
    sb.Append("\t\tset { _eventAssignments = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic TComputeEventAssignmentDelegate[] computeEventAssignments {" + NL());
    sb.Append("\t\tget { return _computeEventAssignments; }" + NL());
    sb.Append("\t\tset { _computeEventAssignments = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic TPerformEventAssignmentDelegate[] performEventAssignments {" + NL());
    sb.Append("\t\tget { return _performEventAssignments; }" + NL());
    sb.Append("\t\tset { _performEventAssignments = value; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic double time {" + NL());
    sb.Append("\t\tget { return _time; }" + NL());
    sb.Append("\t\tset { _time = value; }" + NL());
    sb.Append("\t}" + NL() + NL());
}

void ModelGenerator::WriteAccessors(StringBuilder& sb)
{
    sb.Append("\tpublic int getNumIndependentVariables {" + NL());
    sb.Append("\t\tget { return numIndependentVariables; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumDependentVariables {" + NL());
    sb.Append("\t\tget { return numDependentVariables; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumTotalVariables {" + NL());
    sb.Append("\t\tget { return numTotalVariables; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumBoundarySpecies {" + NL());
    sb.Append("\t\tget { return numBoundaryVariables; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumGlobalParameters {" + NL());
    sb.Append("\t\tget { return numGlobalParameters; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumLocalParameters(int reactionId)" + NL());
    sb.Append("\t{" + NL());
    sb.Append("\t\treturn localParameterDimensions[reactionId];" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumCompartments {" + NL());
    sb.Append("\t\tget { return numCompartments; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumReactions {" + NL());
    sb.Append("\t\tget { return numReactions; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumEvents {" + NL());
    sb.Append("\t\tget { return numEvents; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic int getNumRules {" + NL());
    sb.Append("\t\tget { return numRules; }" + NL());
    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic List<string> Warnings {" + NL());
    sb.Append("\t\tget { return _Warnings; }" + NL());
    sb.Append("\t\tset { _Warnings = value; }" + NL());
    sb.Append("\t}" + NL() + NL());
}

 void ModelGenerator::WriteOutVariables(StringBuilder& sb)
{
      sb.Append("\tprivate List<string> _Warnings = new List<string>();" + NL());
      sb.Append("\tprivate double[] _gp = new double[" + ToString(_NumGlobalParameters + _TotalLocalParmeters) +
                "];           // Vector containing all the global parameters in the System  " + NL());
      sb.Append("\tprivate double[] _sr = new double[" + ToString(_NumModifiableSpeciesReferences) +
                "];           // Vector containing all the modifiable species references  " + NL());
      sb.Append("\tprivate double[][] _lp = new double[" + ToString(_NumReactions) +
                "][];       // Vector containing all the local parameters in the System  " + NL());

      sb.Append("\tprivate double[] _y = new double[", floatingSpeciesConcentrationList.size(),
                "];            // Vector containing the concentrations of all floating species ",  NL());

      //sb.Append(String.Format("\tprivate double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count, NL()));
      sb.AppendFormat("\tprivate double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count(), NL());

      sb.Append("\tprivate double[] _amounts = new double[", floatingSpeciesConcentrationList.size(),
                "];      // Vector containing the amounts of all floating species ", NL());

      sb.Append("\tprivate double[] _bc = new double[", _NumBoundarySpecies,
                "];           // Vector containing all the boundary species concentration values   " , NL());

      sb.Append("\tprivate double[] _c = new double[" , _NumCompartments ,
                "];            // Vector containing all the compartment values   " + NL());

      sb.Append("\tprivate double[] _dydt = new double[" , floatingSpeciesConcentrationList.size() ,
                "];         // Vector containing rates of changes of all species   " , NL());

      sb.Append("\tprivate double[] _rates = new double[" , _NumReactions ,
                "];        // Vector containing the rate laws of all reactions    " , NL());

      sb.Append("\tprivate double[] _ct = new double[" , _NumDependentSpecies ,
                "];           // Vector containing values of all conserved sums      " , NL());

      sb.Append("\tprivate double[] _eventTests = new double[" , _NumEvents ,
                "];   // Vector containing results of any event tests        " , NL());

      sb.Append("\tprivate TEventDelayDelegate[] _eventDelay = new TEventDelayDelegate[" , _NumEvents ,
                "]; // array of trigger function pointers" , NL());

      sb.Append("\tprivate bool[] _eventType = new bool[" , _NumEvents ,
                "]; // array holding the status whether events are useValuesFromTriggerTime or not" , NL());

      sb.Append("\tprivate bool[] _eventPersistentType = new bool[" , _NumEvents ,
                "]; // array holding the status whether events are persitstent or not" , NL());

      sb.Append("\tprivate double _time;" , NL());
      sb.Append("\tprivate int numIndependentVariables;" , NL());
      sb.Append("\tprivate int numDependentVariables;" , NL());
      sb.Append("\tprivate int numTotalVariables;" , NL());
      sb.Append("\tprivate int numBoundaryVariables;" , NL());
      sb.Append("\tprivate int numGlobalParameters;" , NL());
      sb.Append("\tprivate int numCompartments;" , NL());
      sb.Append("\tprivate int numReactions;" , NL());
      sb.Append("\tprivate int numRules;" , NL());
      sb.Append("\tprivate int numEvents;" , NL());
      sb.Append("\tstring[] variableTable = new string[" , floatingSpeciesConcentrationList.size() , "];" , NL());
      sb.Append("\tstring[] boundaryTable = new string[" , boundarySpeciesList.size() , "];" , NL());
      sb.Append("\tstring[] globalParameterTable = new string[" , globalParameterList.size() , "];" , NL());
      sb.Append("\tint[] localParameterDimensions = new int[" , _NumReactions , "];" , NL());
      sb.Append("\tprivate TEventAssignmentDelegate[] _eventAssignments;" , NL());
      sb.Append("\tprivate double[] _eventPriorities;" , NL());
      sb.Append("\tprivate TComputeEventAssignmentDelegate[] _computeEventAssignments;" , NL());
      sb.Append("\tprivate TPerformEventAssignmentDelegate[] _performEventAssignments;" , NL());
      sb.Append("\tprivate bool[] _eventStatusArray = new bool[" , _NumEvents , "];" , NL());
      sb.Append("\tprivate bool[] _previousEventStatusArray = new bool[" , _NumEvents , "];" , NL());
      sb.Append(NL());
      sb.Append("\tpublic TModel ()  " , NL());
      sb.Append("\t{" , NL());

      sb.Append("\t\tnumIndependentVariables = " , _NumIndependentSpecies , ";" , NL());
      sb.Append("\t\tnumDependentVariables = " , _NumDependentSpecies , ";" , NL());
      sb.Append("\t\tnumTotalVariables = " , _NumFloatingSpecies , ";" , NL());
      sb.Append("\t\tnumBoundaryVariables = " , _NumBoundarySpecies , ";" , NL());
      sb.Append("\t\tnumGlobalParameters = " , globalParameterList.size() , ";" , NL());
      sb.Append("\t\tnumCompartments = " , compartmentList.size() , ";" , NL());
      sb.Append("\t\tnumReactions = " , reactionList.size() , ";" , NL());
      sb.Append("\t\tnumEvents = " , _NumEvents , ";" , NL());
      sb.Append("\t\tInitializeDelays();" , NL());

      // Declare any eventAssignment delegates
      if (_NumEvents > 0)
      {
          sb.Append("\t\t_eventAssignments = new TEventAssignmentDelegate[numEvents];" , NL());
          sb.Append("\t\t_eventPriorities = new double[numEvents];" , NL());
          sb.Append("\t\t_computeEventAssignments= new TComputeEventAssignmentDelegate[numEvents];" , NL());
          sb.Append("\t\t_performEventAssignments= new TPerformEventAssignmentDelegate[numEvents];" , NL());

          for (int i = 0; i < _NumEvents; i++)
          {
          	string iStr = ToString(i);
              sb.Append("\t\t_eventAssignments[" + iStr + "] = new TEventAssignmentDelegate (eventAssignment_" + iStr +
                        ");" + NL());
              sb.Append("\t\t_computeEventAssignments[" + iStr +
                        "] = new TComputeEventAssignmentDelegate (computeEventAssignment_" + iStr + ");" + NL());
              sb.Append("\t\t_performEventAssignments[" + iStr +
                        "] = new TPerformEventAssignmentDelegate (performEventAssignment_" + iStr + ");" + NL());
          }

          sb.Append("\t\tresetEvents();" + NL());
          sb.Append(NL());
      }

      if (_NumModifiableSpeciesReferences > 0)
      {
          for (int i = 0; i < ModifiableSpeciesReferenceList.size(); i++)
          {
              sb.Append("\t\t_sr[" + ToString(i) + "]  = " + WriteDouble(ModifiableSpeciesReferenceList[i].value) + ";" + NL());
          }
          sb.Append(NL());
      }

      // Declare space for local parameters
      for (int i = 0; i < _NumReactions; i++)
      {
          sb.Append("\t\tlocalParameterDimensions[" + ToString(i) + "] = " , _LocalParameterDimensions[i] , ";" + NL());
          sb.Append("\t\t_lp[" + ToString(i) + "] = new double[" , _LocalParameterDimensions[i] , "];" , NL());
      }

      sb.Append("\t}" + NL() + NL());
}

void ModelGenerator::WriteClassHeader(StringBuilder& sb)
{
    sb.Append("using System;" + NL());
    sb.Append("using System.IO;" + NL());
    sb.Append("using System.Collections;" + NL());
    sb.Append("using System.Collections.Generic;" + NL());
    sb.Append("using LibRoadRunner;" + NL());

    sb.Append(" " + NL() + NL());
    sb.Append(NL());
    sb.AppendFormat("class TModel : IModel{0}", NL());
    sb.Append("{" + NL());
    sb.AppendFormat("\t// Symbol Mappings{0}{0}", NL());
	for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {

    	sb<<"\t// y["<<i<<"] = "<<floatingSpeciesConcentrationList[i].name<<endl;//{2}", NL());
    }
    sb.Append(NL());
}

string ModelGenerator::FindSymbol(const string& varName)
{
      int index = 0;
      if (floatingSpeciesConcentrationList.find(varName, index))
      {
          return Format("\t\t_y[{0}]", index);
      }
      else if (globalParameterList.find(varName, index))
      {
          return Format("\t\t_gp[{0}]", index);
      }
      else if (boundarySpeciesList.find(varName, index))
      {
          return Format("\t\t_bc[{0}]", index);
      }
      else if (compartmentList.find(varName, index))
      {
          return Format("\t\t_c[{0}]", index);
      }
      else if (ModifiableSpeciesReferenceList.find(varName, index))
          return Format("\t\t_sr[{0}]", index);

      else
          throw Exception(Format("Unable to locate lefthand side symbol in assignment[{0}]", varName));
}

void ModelGenerator::WriteTestConstraints(StringBuilder& sb)
{
    sb.Append("\tpublic void testConstraints()" + NL());
    sb.Append("\t{" + NL());

    for (int i = 0; i < mNOM.getNumConstraints(); i++)
    {
        string sMessage;
        string sCheck = mNOM.getNthConstraint(i, sMessage);

        sb.Append("\t\tif (" + substituteTerms(mNOM.getNumReactions(), "", sCheck) + " == 0.0 )" + NL());
        sb.Append("\t\t\tthrow new Exception(\"" + sMessage + "\");" + NL());
    }

    sb.Append("\t}" + NL() + NL());
}

bool ModelGenerator::ExpressionContainsSymbol(ASTNode *ast, const string& symbol)
{
    if (ast == NULL || IsNullOrEmpty(symbol))
    {
    	return false;
    }

    if (ast->getType() == libsbml::AST_NAME && Trim(ast->getName()) == Trim(symbol))
    {
        return true;
    }

    for (int i = 0; i < ast->getNumChildren(); i++)
    {
        if (ExpressionContainsSymbol(ast->getChild(i), symbol))
        {
            return true;
        }
    }

    return false;

}

bool ModelGenerator::ExpressionContainsSymbol(const string& expression,const string& symbol)
{
      if (IsNullOrEmpty(expression) || IsNullOrEmpty(symbol))
      {
      	return false;
      }
      ASTNode *ast = SBML_parseFormula(expression.c_str());
      return ExpressionContainsSymbol(ast, symbol);
}


void ModelGenerator::WriteEvalInitialAssignments(StringBuilder& sb, const int& numReactions)
{
    sb.Append("\tpublic void evalInitialAssignments()" + NL());
    sb.Append("\t{" + NL());

    int numInitialAssignments = mNOM.getNumInitialAssignments();

    if (numInitialAssignments > 0)
    {
        vector< pair<string, string> > oList;// = new List<Pair<string, string>>();
        for (int i = 0; i < numInitialAssignments; i++)
        {
			pair<string, string> pair = mNOM.getNthInitialAssignmentPair(i);
            oList.push_back(mNOM.getNthInitialAssignmentPair(i));
        }

        // sort them ...
        bool bChange = true;
        int nIndex = -1;
        while (bChange)
        {
            bChange = false;

            for (int i = 0; i < oList.size(); i++)
            {
                pair<string, string> current = oList[i];
                for (int j = i + 1; j < oList.size(); j++)
                {
                    if (ExpressionContainsSymbol(current.second, oList[j].first))
                    {
                        bChange = true;
                        nIndex = j;
                        break;
                    }
                }
                if (bChange)
                {
                	break;
                }
            }

            if (bChange)
            {
                pair<string, string> pairToMove = oList[nIndex];
                oList.erase(oList.begin() + nIndex);
                //oList.RemoveAt(nIndex);
				oList.insert(oList.begin(), pairToMove);	//Todo: check it this is correct...
            }
        }

		vector< pair<string, string> >::iterator iter;
        for(iter = oList.begin(); iter < oList.end(); iter++)
        {
			pair<string, string>& pair = (*iter);
            string leftSideRule = FindSymbol(pair.first);
            string rightSideRule = pair.second;
            if (leftSideRule.size())
            {
                sb.Append(leftSideRule + " = ");
                sb.Append(substituteTerms(numReactions, "", rightSideRule) + ";" + NL());
            }
        }
    }
    for (int i = 0; i < mNOM.GetModel()->getNumEvents(); i++)
    {
        Event *current = mNOM.GetModel()->getEvent(i);
        string initialTriggerValue = ToString(current->getTrigger()->getInitialValue());//.ToString().ToLowerInvariant();
        sb.Append("\t\t_eventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
        sb.Append("\t\t_previousEventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
    }
    sb.Append("\t}" + NL() + NL());
}

int ModelGenerator::WriteComputeRules(StringBuilder& sb, const int& numReactions)
{
    IntStringHashTable mapVariables;
    int numRateRules = 0;
    int numOfRules = mNOM.getNumRules();

    sb.Append("\tpublic void computeRules(double[] y) {" + NL());
    // ------------------------------------------------------------------------------
    for (int i = 0; i < numOfRules; i++)
    {
        try
        {
            string leftSideRule = "";
            string rightSideRule = "";
            string ruleType = mNOM.getNthRuleType(i);

            // We only support assignment and ode rules at the moment
            string eqnRule = mNOM.getNthRule(i);
            RRRule aRule(eqnRule, ruleType);
            string varName =  Trim(aRule.GetLHS());	//eqnRule.Substring(0, index).Trim();
            string rightSide = Trim(aRule.GetRHS());	//eqnRule.Substring(index + 1).Trim();

            bool isRateRule = false;

            switch (aRule.GetType())
            {
                case rtAlgebraic:
                    Warnings.Add("RoadRunner does not yet support algebraic rules in SBML, they will be ignored.");
                    leftSideRule = "";//NULL;
                break;

                case rtAssignment:
                    leftSideRule = FindSymbol(varName);
                break;

                case rtRate:
                    isRateRule = true;
                    int index;
                    if (floatingSpeciesConcentrationList.find(varName,  index))
                    {
                        leftSideRule = Format("\t\t_dydt[{0}]", index);
                        floatingSpeciesConcentrationList[index].rateRule = true;
                    }
                    else
                    {
                        leftSideRule = "\t\t_rateRules[" + ToString(numRateRules) + "]";
                        _oMapRateRule[numRateRules] = FindSymbol(varName);
                        mapVariables[numRateRules] = varName;
                        numRateRules++;
                    }

                break;
            }

            // Run the equation through MathML to carry out any conversions (eg ^ to Pow)
            string rightSideMathml = mNOM.convertStringToMathML(rightSide);
            rightSideRule = mNOM.convertMathMLToString(rightSideMathml);
            if (leftSideRule.size())// != NULL)
            {
                sb.Append(leftSideRule + " = ");
                int speciesIndex;
                bool isSpecies = floatingSpeciesConcentrationList.find(varName, speciesIndex);

                Symbol* symbol = (speciesIndex != -1) ? &(floatingSpeciesConcentrationList[speciesIndex]) : NULL;
                string sCompartment;

////                            !rightSide.Contains(sCompartment)

                if(isRateRule && mNOM.MultiplyCompartment(varName, sCompartment) && (rightSide.find(sCompartment) == string::npos))
                {
                    sb.AppendFormat("({0}) * {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(sCompartment), NL());
                }
                else
                {
                    if (isSpecies && !isRateRule && symbol != NULL && symbol->hasOnlySubstance && symbol->compartmentName.size() != 0)
                    {
                        sb.AppendFormat("({0}) / {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(symbol->compartmentName), NL());
                    }
                    else
                    {
                        sb.AppendFormat("{0};{1}", substituteTerms(numReactions, "", rightSideRule), NL());
                    }
                }

                // RateRules and species ! again
                //
                // sb.Append(String.Format("{0};{1}", substituteTerms(numReactions, "", rightSideRule), NL()));

                if (mNOM.IsCompartment(varName))
                {
                    sb.Append("\t\tconvertToConcentrations();");
                }
            }
        }
        catch (const Exception& ex)
        {
            throw new SBWApplicationException("Error while trying to get Rule #" + ToString(i) + ex.Message);
        }
    }

    sb.Append("\t}" + NL() + NL());
    sb.Append("\tprivate double[] _rateRules = new double[" + ToString(numRateRules) +
              "];           // Vector containing values of additional rate rules      " + NL());

    sb.Append("\tpublic void InitializeRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < numRateRules; i++)
    {
        sb<<"\t\t_rateRules[" << i << "] = " << _oMapRateRule[i] << ";" << NL();
    }

    sb.Append("\t}" + NL() + NL());
    sb.Append("\tpublic void AssignRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sb<<(string)_oMapRateRule[i] << " = _rateRules[" << i << "];" << NL();
    }

    sb.Append("\t}" + NL() + NL());

    sb.Append("\tpublic void InitializeRateRuleSymbols()" + NL() + "\t{" + NL());
    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        string varName = (string)mapVariables[i];
        double value = mNOM.getValue(varName);
        if (!IsNaN(value))
        {
            sb<< _oMapRateRule[i] << " = " << ToString(value, STR_DoubleFormat) << ";" << NL();
        }
    }

    sb.Append("\t}" + NL() + NL());
    sb.Append("\tpublic void AssignRates(double[] oRates)" + NL() + "\t{" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sb<< _oMapRateRule[i] << " = oRates[" << i << "];" << NL();
    }

    sb.Append("\t}" + NL() + NL());
    sb.Append("\tpublic double[] GetCurrentValues()" + NL() + "\t{" + NL());
    sb.Append("\t\tdouble[] dResult = new double[" + ToString(NumAdditionalRates()) + "];" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sb<<"\t\tdResult[" << i << "] = " << _oMapRateRule[i] << ";" << NL();
    }
    sb.Append("\t\treturn dResult;" + NL());

    sb.Append("\t}" + NL() + NL());
    return numOfRules;
}

void ModelGenerator::WriteComputeReactionRates(StringBuilder& sb, const int& numReactions)
{
    sb.Append("\t// Compute the reaction rates" + NL());
    sb.Append("\tpublic void computeReactionRates (double time, double[] y)" + NL());
    sb.Append("\t{" + NL());


    for (int i = 0; i < numReactions; i++)
    {
        string kineticLaw = mNOM.getKineticLaw(i);

        // The following code is for the case when the kineticLaw contains a ^ in place
        // of pow for exponent handling. It would not be needed in the case when there is
        // no ^ in the kineticLaw.
        string subKineticLaw;
//        if (kineticLaw.IndexOf("^", System.StringComparison.Ordinal) > 0) //Todo: fix this...
//        {
//            string kineticLaw_mathml = mNOM.convertStringToMathML(kineticLaw);
//            subKineticLaw = mNOM.convertMathMLToString(kineticLaw_mathml);
//        }
//        else
        {
            subKineticLaw = kineticLaw;
        }

        string modKineticLaw = substituteTerms(reactionList[i].name, subKineticLaw, true) + ";";

        // modify to use current y ...
        modKineticLaw = Substitute(modKineticLaw, "_y[", "y[");
        sb.AppendFormat("\t\t_rates[{0}] = {1}{2}", i, modKineticLaw, NL());
    }
    sb.AppendFormat("\t}{0}{0}", NL());
}

void ModelGenerator::WriteEvalEvents(StringBuilder& sb, const int& numEvents, const int& numFloatingSpecies)
{
    sb.Append("\t// Event handling function" + NL());
    sb.Append("\tpublic void evalEvents (double timeIn, double[] oAmounts)" + NL());
    sb.Append("\t{" + NL());

    if (numEvents > 0)
    {
        for (int i = 0; i < NumAdditionalRates(); i++)
        {
            sb<<(string) _oMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
        }
        for (int i = 0; i < numFloatingSpecies; i++)
        {
            sb<<"\t\t_y[" << i << "] = oAmounts[" << (i + NumAdditionalRates()) << "]/" <<
                      convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
        }
    }

    sb.Append("\t\t_time = timeIn;  // Don't remove" + NL());
    sb.Append("\t\tupdateDependentSpeciesValues(_y);" + NL());
    sb.Append("\t\tcomputeRules (_y);" + NL());

    for (int i = 0; i < numEvents; i++)
    {
        ArrayList ev = mNOM.getNthEvent(i);
        StringList tempList = ev[0];
        string eventString = tempList[0];

        eventString = substituteTerms(0, "", eventString);
        sb<<"\t\tpreviousEventStatusArray[" << i << "] = eventStatusArray[" << i << "];" << NL();
        sb.Append("\t\tif (" + eventString + " == 1.0) {" + NL());
        sb.Append("\t\t     eventStatusArray[" + ToString(i) + "] = true;" + NL());
        sb.Append("\t\t     eventTests[" + ToString(i) + "] = 1;" + NL());
        sb.Append("\t\t} else {" + NL());
        sb.Append("\t\t     eventStatusArray[" + ToString(i) + "] = false;" + NL());
        sb.Append("\t\t     eventTests[" + ToString(i) + "] = -1;" + NL());
        sb.Append("\t\t}" + NL());
    }
    sb.Append("\t}" + NL() + NL());
}

void ModelGenerator::WriteEvalModel(StringBuilder& sb, const int& numReactions, const int& numIndependentSpecies, const int& numFloatingSpecies, const int& numOfRules)
{
    sb.Append("\t// Model Function" + NL());
    sb.Append("\tpublic void evalModel (double timein, double[] oAmounts)" + NL());
    sb.Append("\t{" + NL());

    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        sb<<(string)_oMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
    }

    for (int i = 0; i < numFloatingSpecies; i++)
    {
    	sb<<"\t\t_y[" << i << "] = oAmounts[" << i + NumAdditionalRates() << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    sb.Append(NL());
    sb.Append("\t\tconvertToAmounts();" + NL());
    sb.Append("\t\t_time = timein;  // Don't remove" + NL());
    sb.Append("\t\tupdateDependentSpeciesValues (_y);" + NL());

    if (numOfRules > 0)
    {
        sb.Append("\t\tcomputeRules (_y);" + NL());
    }

    sb.Append("\t\tcomputeReactionRates (time, _y);" + NL());

    // Write out the ODE equations
    string stoich;
    for (int i = 0; i < numIndependentSpecies; i++)
    {
        StringBuilder eqnBuilder;// = new StringBuilder(" ");
        string floatingSpeciesName = independentSpeciesList[i];
        for (int j = 0; j < numReactions; j++)
        {
            Reaction *oReaction = mNOM.GetModel()->getReaction(j);
            int numProducts = (int) oReaction->getNumProducts();
            double productStoichiometry;
            for (int k1 = 0; k1 < numProducts; k1++)
            {
                SpeciesReference* product = oReaction->getProduct(k1);

                string productName = product->getSpecies();
                if (floatingSpeciesName == productName)
                {
                    productStoichiometry = product->getStoichiometry();

                    if (product->isSetId() && product->getLevel() > 2)
                    {
                        stoich = "(" +
                             substituteTerms(numReactions, "",
                                product->getId()) +
                             ") * ";
                    }
                    else if (product->isSetStoichiometry())
                    {
                        if (productStoichiometry != 1)
                        {
                            int denom = product->getDenominator();
                            if (denom != 1)
                            {
                                stoich = Format("((double){0}/(double){1})*", WriteDouble(productStoichiometry), denom);
                            }
                            else
                            {
                                stoich = WriteDouble(productStoichiometry) + '*';
                            }
                        }
                        else
                        {
                            stoich = "";
                        }
                    }
                    else
                    {
                        if (product->isSetStoichiometryMath() && product->getStoichiometryMath()->isSetMath())
                        {
                            stoich = "(" +
                                     substituteTerms(numReactions, "",
                                        SBML_formulaToString(product->getStoichiometryMath()->getMath())) +
                                     ") * ";
                        }
                        else
                        {
                            stoich = "";
                        }
                    }
                    eqnBuilder.AppendFormat(" + {0}_rates[{1}]", stoich, j);
                }
            }

            int numReactants = (int)oReaction->getNumReactants();
            double reactantStoichiometry;
            for (int k1 = 0; k1 < numReactants; k1++)
            {
                SpeciesReference *reactant = oReaction->getReactant(k1);
                string reactantName = reactant->getSpecies();
                if (floatingSpeciesName == reactantName)
                {
                    reactantStoichiometry = reactant->getStoichiometry();

                    if (reactant->isSetId() && reactant->getLevel() > 2)
                    {
                        stoich = Format("({0}) * ", substituteTerms(numReactions, "", reactant->getId()));
                    }
                    else if (reactant->isSetStoichiometry())
                    {
                        if (reactantStoichiometry != 1)
                        {
                            int denom = reactant->getDenominator();
                            if (denom != 1)
                            {
                                stoich = Format("((double){0}/(double){1})*", WriteDouble(reactantStoichiometry), denom);
                            }
                            else
                            {
                                stoich = WriteDouble(reactantStoichiometry) + "*";
                            }
                        }
                        else
                        {
                            stoich = "";
                        }
                    }

                    else
                    {
                        if (reactant->isSetStoichiometryMath() && reactant->getStoichiometryMath()->isSetMath())
                        {
                            stoich = "(" +
                                     substituteTerms(numReactions, "",
                                        SBML_formulaToString(reactant->getStoichiometryMath()->getMath())) +
                                     ") * ";
                        }
                        else
                        {
                            stoich = "";
                        }

                    }

                    eqnBuilder.Append(Format(" - {0}_rates[{1}]", stoich, j));
                }
            }
        }

        string final = eqnBuilder.ToString();//.Trim();

        if (IsNullOrEmpty(final))
        {
            final = "    0.0";
        }

        if (mNOM.GetSBMLDocument()->getLevel() > 2)
        {
            // remember to take the conversion factor into account
            string factor = "";
            Species* species = mNOM.GetModel()->getSpecies(floatingSpeciesName);
            if (species != NULL)
            {
                if (species->isSetConversionFactor())
                {
                    factor = species->getConversionFactor();
                }
                else if (mNOM.GetModel()->isSetConversionFactor())
                {
                    factor = mNOM.GetModel()->getConversionFactor();
                }
            }

            if (!IsNullOrEmpty(factor))
            {
                final = FindSymbol(factor) + " * (" + final + ")";
            }
        }

        // If the floating species has a raterule then prevent the dydt
        // in the model function from overriding it. I think this is expected behavior.
        if (!floatingSpeciesConcentrationList[i].rateRule)
        {
            sb<<"\t\t_dydt[" << i << "] =" << final << ";" << NL();
        }
    }

    sb.Append("\t\tconvertToAmounts ();" + NL());
    sb.Append("\t}" + NL() + NL());
}

Symbol* ModelGenerator::GetSpecies(const string& id)
{
	int index;
    if (floatingSpeciesConcentrationList.find(id, index))
    {
    	return &(floatingSpeciesConcentrationList[index]);
    }

    if (boundarySpeciesList.find(id, index))
    {
    	return &(boundarySpeciesList[index]);
    }
    return NULL;
}

void ModelGenerator::WriteEventAssignments(StringBuilder& sb, const int& numReactions, const int& numEvents)
{
	StringList delays;
    vector<bool> eventType;
    vector<bool> eventPersistentType;
    if (numEvents > 0)
    {
        sb.Append("\t// Event assignments" + NL());
        for (int i = 0; i < numEvents; i++)
        {
            ArrayList ev = mNOM.getNthEvent(i);
            eventType.push_back(mNOM.getNthUseValuesFromTriggerTime(i));
            eventPersistentType.push_back(mNOM.GetModel()->getEvent(i)->getTrigger()->getPersistent());

            StringList event = ev[1];
            int numItems = event.size();
            string str = substituteTerms(numReactions, "", event[0]);
            delays.Add(str);

            sb.AppendFormat("\tpublic void eventAssignment_{0} () {{1}", i, NL());
            sb.AppendFormat("\t\tperformEventAssignment_{0}( computeEventAssignment_{0}() );{1}", i, NL());
            sb.Append("\t}" + NL());
            sb.AppendFormat("\tpublic double[] computeEventAssignment_{0} () {{1}", i, NL());
            StringList oTemp;
            StringList oValue;
            int nCount = 0;
            int numAssignments = ev.size() - 2;
            sb.AppendFormat("\t\tdouble[] values = new double[ {0}];{1}", numAssignments, NL());
            for (int j = 2; j < ev.size(); j++)
            {
                StringList asgn = (StringList) ev[j];
                //string assignmentVar = substituteTerms(numReactions, "", (string)asgn[0]);
                string assignmentVar = FindSymbol((string)asgn[0]);
                string str;
                Symbol *species = GetSpecies(assignmentVar);


                if (species != NULL && species->hasOnlySubstance)
                {
                    str = Format("{0} = ({1}) / {2}", assignmentVar, substituteTerms(numReactions, "", (string)asgn[1]), FindSymbol(species->compartmentName));
                }
                else
                {
                    str = Format("{0} = {1}", assignmentVar, substituteTerms(numReactions, "", (string) asgn[1]));
                }

                string sTempVar = Format("values[{0}]", nCount);

                oTemp.Add(assignmentVar);
                oValue.Add(sTempVar);

                str = sTempVar+ str.substr(str.find(" ="));
                nCount++;
                sb.AppendFormat("\t\t{0};{1}", str, NL());
            }
            sb.Append("\t\treturn values;" + NL());
            sb.Append("\t}" + NL());
            sb.AppendFormat("\tpublic void performEventAssignment_{0} (double[] values) {{1}", i, NL());

            for (int j = 0; j < oTemp.size(); j++)
            {
                sb.AppendFormat("\t\t{0} = values[{1}];{2}", oTemp[j], j, NL());
                string aStr = (string) oTemp[j];
                aStr = Trim(aStr);

                if (StartsWith(aStr, "_c[")) //Todo:May have to trim?
                {
                    sb.Append("\t\tconvertToConcentrations();" + NL());
                }
            }

            sb.Append("\t}" + NL());
        }
        sb.Append("\t" + NL());
    }

    sb.AppendFormat("{0}{0}\tprivate void InitializeDelays() { {0}", NL());
    for (int i = 0; i < delays.size(); i++)
    {
        sb.AppendFormat("\t\t_eventDelay[{0}] = new TEventDelayDelegate(delegate { return {1}; } );{2}", i, delays[i], NL());
        sb.AppendFormat("\t\t_eventType[{0}] = {1};{2}", i, ToString((eventType[i] ? true : false)), NL());
        sb.AppendFormat("\t\t_eventPersistentType[{0}] = {1};{2}", i, (eventPersistentType[i] ? "true" : "false"), NL());
    }
    sb.AppendFormat("\t}{0}{0}", NL());

    sb.AppendFormat("{0}{0}\tpublic void computeEventPriorites() { {0}", NL());
    for (int i = 0; i < numEvents; i++)
    {
        Event* current = mNOM.GetModel()->getEvent(i);

        if (current->isSetPriority() && current->getPriority()->isSetMath())
        {
            string priority = SBML_formulaToString(current->getPriority()->getMath());
            sb.AppendFormat("\t\t_eventPriorities[{0}] = {1};{2}", i, substituteTerms(numReactions, "", priority), NL());
        }
        else
        {
            sb.AppendFormat("\t\t_eventPriorities[{0}] = 0f;{1}", i, NL());
        }
    }
    sb.AppendFormat("\t}{0}{0}", NL());
}


string ModelGenerator::WriteDouble(const double& value)
{
	return ToString(value, STR_DoubleFormat);
}

void ModelGenerator::WriteSetParameterValues(StringBuilder& sb, const int& numReactions)
{
    sb.Append("\tpublic void setParameterValues ()" + NL());
    sb.Append("\t{" + NL());

    for (int i = 0; i < globalParameterList.size(); i++)
    {
        sb.AppendFormat("\t\t{0} = (double){1};{2}",
                      convertSymbolToGP(globalParameterList[i].name),
                      WriteDouble(globalParameterList[i].value),
                      NL());
    }

    // Initialize local parameter values
    for (int i = 0; i < numReactions; i++)
    {
        for (int j = 0; j < localParameterList[i].size(); j++)
            sb.AppendFormat("\t\t_lp[{0}][{1}] = (double){2};{3}",
                          i,
                          j,
                          WriteDouble(localParameterList[i][j].value),
                          NL());
    }

    sb.Append("\t}" + NL() + NL());
}


void ModelGenerator::WriteSetCompartmentVolumes(StringBuilder& sb)
{
    sb.Append("\tpublic void setCompartmentVolumes ()" + NL());
    sb.Append("\t{" + NL());
    for (int i = 0; i < compartmentList.size(); i++)
    {
        sb.Append("\t\t" + convertSymbolToC(compartmentList[i].name) + " = (double)" +
                  WriteDouble(compartmentList[i].value) + ";" + NL());

        // at this point we also have to take care of all initial assignments for compartments as well as
        // the assignment rules on compartments ... otherwise we are in trouble :)
		stack<string> initializations = mNOM.GetMatchForSymbol(compartmentList[i].name);
        while (initializations.size() > 0)
        {
        	string term(initializations.top());
            string sub = substituteTerms(_NumReactions, "", term);
            sb.Append("\t\t" + sub + ";" + NL());
            initializations.pop();
        }
    }

    sb.Append("\t}" + NL() + NL());
}

void ModelGenerator::WriteSetBoundaryConditions(StringBuilder& sb)
{
    sb.Append("\tpublic void setBoundaryConditions ()" + NL());
    sb.Append("\t{" + NL());
    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        if (IsNullOrEmpty(boundarySpeciesList[i].formula))
        {
            sb.Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      WriteDouble(boundarySpeciesList[i].value) + ";" + NL());
        }
        else
        {
            sb.Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      boundarySpeciesList[i].formula + ";" + NL());
        }
    }
    sb.Append("\t}" + NL() + NL());
}


void ModelGenerator::WriteSetInitialConditions(StringBuilder& sb, const int& numFloatingSpecies)
{
    sb.Append("\tpublic void initializeInitialConditions ()" + NL());
    sb.Append("\t{" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        if (IsNullOrEmpty(floatingSpeciesConcentrationList[i].formula))
        {
            sb.Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      WriteDouble(floatingSpeciesConcentrationList[i].value) + ";" + NL());
        }
        else
        {
            sb.Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      floatingSpeciesConcentrationList[i].formula + ";" + NL());
        }
    }
    sb.Append(NL());

    sb.Append("\t}" + NL() + NL());

    // ------------------------------------------------------------------------------
    sb.Append("\tpublic void setInitialConditions ()" + NL());
    sb.Append("\t{" + NL());

    for (int i = 0; i < numFloatingSpecies; i++)
    {
        sb<<"\t\t_y[" << i << "] =  _init_y[" << i << "];" << NL();
        sb<<"\t\t_amounts[" << i << "] = _y[" << i << "]*" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    sb.Append(NL());
	sb.Append("\t}" + NL() + NL());
}

int ModelGenerator::ReadCompartments()
{
      int numCompartments = mNOM.getNumCompartments();
      for (int i = 0; i < numCompartments; i++)
      {
          string sCompartmentId = mNOM.getNthCompartmentId(i);

          double value = mNOM.getValue(sCompartmentId);

          if(IsNaN(value))
          {
			 value = 1;
          }
          compartmentList.Add(Symbol(sCompartmentId, value));
      }
      return numCompartments;
}

int ModelGenerator::ReadModifiableSpeciesReferences()
{
	if(!mNOM.GetSBMLDocument())
    {
    	return -1;
    }
    SBMLDocument &SBMLDoc = *mNOM.GetSBMLDocument();
    Model &SbmlModel  = *mNOM.GetModel();

	if(mNOM.GetSBMLDocument()->getLevel() < 3)
    {
    	return 0;
    }

    string id;
    double value;
    int numReactions = SbmlModel.getNumReactions();
    for (int i = 0; i < numReactions; i++)
    {
        Reaction &reaction = *(SbmlModel.getReaction(i));
        for (int j = 0; j < reaction.getNumReactants(); j++)
        {
            SpeciesReference &reference = *(reaction.getReactant(j));
            id = reference.getId();
            if (!(id.size()))
            {
            	continue;
            }
            value = reference.getStoichiometry();
            if (IsNaN(value))
                value = 1;

            if (reference.isSetId())
            {
                ModifiableSpeciesReferenceList.Add(Symbol(id, value));
            }
        }
        for (int j = 0; j < reaction.getNumProducts(); j++)
        {
            SpeciesReference &reference = *(reaction.getProduct(j));
            id = reference.getId();
            if (IsNullOrEmpty(id))
            {
            	continue;
            }
            value = reference.getStoichiometry();
            if (IsNaN(value))
            {
                value = 1;
            }

            if (reference.isSetId())
            {
                ModifiableSpeciesReferenceList.Add(Symbol(id, value));
            }
        }
    }
    return ModifiableSpeciesReferenceList.size();
}

}//rr namespace

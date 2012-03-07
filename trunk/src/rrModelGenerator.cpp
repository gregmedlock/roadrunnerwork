#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include <cmath>
#include <stack>
#include "sbml/SBMLDocument.h"
#include "rrModelGenerator.h"
#include "rrLogger.h"
#include "rrStringUtils.h"
#include "rrException.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------

using namespace std;
//using namespace LIB_STRUCTURAL;

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

StringList ModelGenerator::getCompartmentList()
{
    StringList tmp;
    for (u_int i = 0; i < compartmentList.size(); i++)
    {
        tmp.Add(compartmentList[i].name);
    }
    return tmp;
}

string ModelGenerator::substituteTerms(const int& numReactions, const string& reactionName, const string& equation)
{
    return substituteTerms(reactionName, equation, false);
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

int ModelGenerator::ReadGlobalParameters()
{
    int numGlobalParameters;
    ArrayList oParameters = mNOM.getListOfParameters();
    numGlobalParameters = oParameters.Count();
    for (u_int i = 0; i < numGlobalParameters; i++)
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
        for (u_int j = 0; j < numLocalParameters; j++)
        {
            localParameterDimensions[i] = numLocalParameters;
            name = mNOM.getNthParameterId(i, j);
            value = mNOM.getNthParameterValue(i, j);
            newList.Add(Symbol(reactionName, name, value));
        }
        localParameterList.push_back(newList);
    }
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

    for (u_int i = 0; i < ast->getNumChildren(); i++)
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

string ModelGenerator::WriteDouble(const double& value)
{
	return ToString(value, STR_DoubleFormat);
}

int ModelGenerator::ReadCompartments()
{
    int numCompartments = mNOM.getNumCompartments();
    for (u_int i = 0; i < numCompartments; i++)
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
    for (u_int i = 0; i < numReactions; i++)
    {
        Reaction &reaction = *(SbmlModel.getReaction(i));
        for (u_int j = 0; j < reaction.getNumReactants(); j++)
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
        for (u_int j = 0; j < reaction.getNumProducts(); j++)
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

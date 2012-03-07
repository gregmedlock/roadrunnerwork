#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "sbml/Model.h"
#include "sbml/SBMLDocument.h"
#include "rrCSharpGenerator.h"
#include "libstructural.h"
#include "rrStringListContainer.h"
#include "rrStringUtils.h"
#include "rrUtils.h"
#include "rrRule.h"
#include "rrScanner.h"
#include "rrLogger.h"
#include "rrRoadRunner.h"
#include "rrException.h"
#include "rrCGenerator.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

using namespace std;
using namespace LIB_STRUCTURAL;

namespace rr
{
CGenerator::CGenerator(){}

CGenerator::~CGenerator(){}

string CGenerator::GetHeaderCode()
{
	return mHeaderCode;
}

string CGenerator::GetSourceCode()
{
	return mSourceCode;
}

// Generates the Model Code from the SBML string
string CGenerator::generateModelCode(const string& sbmlStr)
{
	Log(lDebug4)<<"Entering CGenerators generateModelCode(string) function";

    StringList  	Warnings;
    StringBuilder 	sbh;		//Generate header file
    StringBuilder 	sbc;        //Generate source file

	mNOM.Reset();
    string sASCII = mNOM.convertTime(sbmlStr, "time");

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


    //Write model to String builder...
	WriteClassHeader(sbh);
    WriteOutVariables(sbh);
//    WriteOutSymbolTables(sbh);
//    WriteResetEvents(sbh, _NumEvents);
//    WriteSetConcentration(sbh);
//    WriteGetConcentration(sbh);
//    WriteConvertToAmounts(sbh);
//    WriteConvertToConcentrations(sbh);
//    WriteProperties(sbh);
//    WriteAccessors(sbh);
//    WriteUserDefinedFunctions(sbh);
//    WriteSetInitialConditions(sbh, _NumFloatingSpecies);
//    WriteSetBoundaryConditions(sbh);
//    WriteSetCompartmentVolumes(sbh);
//    WriteSetParameterValues(sbh, _NumReactions);
//   	WriteComputeConservedTotals(sbh, _NumFloatingSpecies, _NumDependentSpecies);
//
//
//    // Get the L0 matrix
//    int nrRows;
//    int nrCols;
//    double* aL0 = InitializeL0(nrRows, nrCols); 	//Todo: What is this doing? answer.. it is used below..
//    DoubleMatrix L0(aL0,nrRows, nrCols); 		//How many rows and cols?? We need to know that in order to use the matrix properly!
//
//    WriteUpdateDependentSpecies(sbh, _NumIndependentSpecies, _NumDependentSpecies, L0);
//    int numOfRules = WriteComputeRules(sbh, _NumReactions);
//    WriteComputeAllRatesOfChange(sbh, _NumIndependentSpecies, _NumDependentSpecies, L0);
//    WriteComputeReactionRates(sbh, _NumReactions);
//    WriteEvalModel(sbh, _NumReactions, _NumIndependentSpecies, _NumFloatingSpecies, numOfRules);
//    WriteEvalEvents(sbh, _NumEvents, _NumFloatingSpecies);
//    WriteEventAssignments(sbh, _NumReactions, _NumEvents);
//    WriteEvalInitialAssignments(sbh, _NumReactions);
//    WriteTestConstraints(sbh);

	sbh.AppendFormat("} gTheModel;\t//This is global data in the DLL{0}", NL());


	WriteInitFunction(sbh, sbc);
    sbh<<"#endif //modelH"<<NL();
  	sbh<<"//End of generated model code"<<endl;

    mHeaderCode = sbh.ToString();
    mSourceCode = sbc.ToString();

	return mHeaderCode + mSourceCode;
}

void CGenerator::WriteClassHeader(StringBuilder& sbh)
{
	//Create c code header file....
    sbh.Append("#ifndef modelH" + NL());
    sbh.Append("#define modelH" + NL());
    sbh.Append("#include <stdio.h>" + NL());
    sbh	<<"#if defined(BUILD_MODEL_DLL)\n"
    	<<"#define D_S __declspec(dllexport)\n"
        <<"#else\n"
        <<"#define D_S __declspec(dllimport)\n"
        <<"#endif\n";

    sbh.Append("//************************************************************************** " + NL());
    sbh.AppendFormat("\t// Model Symbol Mappings{0}{0}", NL());
	for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
    	sbh<<"\t// y["<<i<<"] = "<<floatingSpeciesConcentrationList[i].name<<endl;//{2}", NL());
    }

    sbh.Append("//************************************************************************** " + NL());
    sbh.Append(NL());
    sbh.Append(NL());
    sbh.AppendFormat("D_S struct TModel{0}", NL());
    sbh.Append("{" + NL());
}

void CGenerator::WriteOutVariables(StringBuilder& sbh)
{
    sbh.Append("\tchar** _Warnings;" + NL());
	sbh<<"\tdouble _gp["<<(_NumGlobalParameters + _TotalLocalParmeters)<<"];\t\t// Vector containing all the global parameters in the System  "<<NL();
	if(_NumModifiableSpeciesReferences)
    {
      sbh<<"\tdouble _sr["<<_NumModifiableSpeciesReferences<<"];           // Vector containing all the modifiable species references  "<<endl;
    }
//      sbh.Append("\t double[][] _lp = new double[" + ToString(_NumReactions) +
//                "][];       // Vector containing all the local parameters in the System  " + NL());
//
//      sbh.Append("\t double[] _y = new double[", floatingSpeciesConcentrationList.size(),
//                "];            // Vector containing the concentrations of all floating species ",  NL());
//
//      //sbh.Append(String.Format("\t double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count, NL()));
//      sbh.AppendFormat("\t double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count(), NL());
//
//      sbh.Append("\t double[] _amounts = new double[", floatingSpeciesConcentrationList.size(),
//                "];      // Vector containing the amounts of all floating species ", NL());
//
//      sbh.Append("\t double[] _bc = new double[", _NumBoundarySpecies,
//                "];           // Vector containing all the boundary species concentration values   " , NL());
//
//      sbh.Append("\t double[] _c = new double[" , _NumCompartments ,
//                "];            // Vector containing all the compartment values   " + NL());
//
//      sbh.Append("\t double[] _dydt = new double[" , floatingSpeciesConcentrationList.size() ,
//                "];         // Vector containing rates of changes of all species   " , NL());
//
//      sbh.Append("\t double[] _rates = new double[" , _NumReactions ,
//                "];        // Vector containing the rate laws of all reactions    " , NL());
//
//      sbh.Append("\t double[] _ct = new double[" , _NumDependentSpecies ,
//                "];           // Vector containing values of all conserved sums      " , NL());
//
//      sbh.Append("\t double[] _eventTests = new double[" , _NumEvents ,
//                "];   // Vector containing results of any event tests        " , NL());
//
//      sbh.Append("\t TEventDelayDelegate[] _eventDelay = new TEventDelayDelegate[" , _NumEvents ,
//                "]; // array of trigger function pointers" , NL());
//
//      sbh.Append("\t bool[] _eventType = new bool[" , _NumEvents ,
//                "]; // array holding the status whether events are useValuesFromTriggerTime or not" , NL());
//
//      sbh.Append("\t bool[] _eventPersistentType = new bool[" , _NumEvents ,
//                "]; // array holding the status whether events are persitstent or not" , NL());
//
//      sbh.Append("\t double _time;" , NL());
//      sbh.Append("\t int numIndependentVariables;" , NL());
//      sbh.Append("\t int numDependentVariables;" , NL());
//      sbh.Append("\t int numTotalVariables;" , NL());
//      sbh.Append("\t int numBoundaryVariables;" , NL());
//      sbh.Append("\t int numGlobalParameters;" , NL());
//      sbh.Append("\t int numCompartments;" , NL());
//      sbh.Append("\t int numReactions;" , NL());
//      sbh.Append("\t int numRules;" , NL());
//      sbh.Append("\t int numEvents;" , NL());
//      sbh.Append("\tstring[] variableTable = new string[" , floatingSpeciesConcentrationList.size() , "];" , NL());
//      sbh.Append("\tstring[] boundaryTable = new string[" , boundarySpeciesList.size() , "];" , NL());
//      sbh.Append("\tstring[] globalParameterTable = new string[" , globalParameterList.size() , "];" , NL());
//      sbh.Append("\tint[] localParameterDimensions = new int[" , _NumReactions , "];" , NL());
//      sbh.Append("\t TEventAssignmentDelegate[] _eventAssignments;" , NL());
//      sbh.Append("\t double[] _eventPriorities;" , NL());
//      sbh.Append("\t TComputeEventAssignmentDelegate[] _computeEventAssignments;" , NL());
//      sbh.Append("\t TPerformEventAssignmentDelegate[] _performEventAssignments;" , NL());
//      sbh.Append("\t bool[] _eventStatusArray = new bool[" , _NumEvents , "];" , NL());
//      sbh.Append("\t bool[] _previousEventStatusArray = new bool[" , _NumEvents , "];" , NL());
//      sbh.Append(NL());
//      sbh.Append("\t TModel ()  " , NL());
//      sbh.Append("\t{" , NL());
//
//      sbh.Append("\t\tnumIndependentVariables = " , _NumIndependentSpecies , ";" , NL());
//      sbh.Append("\t\tnumDependentVariables = " , _NumDependentSpecies , ";" , NL());
//      sbh.Append("\t\tnumTotalVariables = " , _NumFloatingSpecies , ";" , NL());
//      sbh.Append("\t\tnumBoundaryVariables = " , _NumBoundarySpecies , ";" , NL());
//      sbh.Append("\t\tnumGlobalParameters = " , globalParameterList.size() , ";" , NL());
//      sbh.Append("\t\tnumCompartments = " , compartmentList.size() , ";" , NL());
//      sbh.Append("\t\tnumReactions = " , reactionList.size() , ";" , NL());
//      sbh.Append("\t\tnumEvents = " , _NumEvents , ";" , NL());
//      sbh.Append("\t\tInitializeDelays();" , NL());
//
//      // Declare any eventAssignment delegates
//      if (_NumEvents > 0)
//      {
//          sbh.Append("\t\t_eventAssignments = new TEventAssignmentDelegate[numEvents];" , NL());
//          sbh.Append("\t\t_eventPriorities = new double[numEvents];" , NL());
//          sbh.Append("\t\t_computeEventAssignments= new TComputeEventAssignmentDelegate[numEvents];" , NL());
//          sbh.Append("\t\t_performEventAssignments= new TPerformEventAssignmentDelegate[numEvents];" , NL());
//
//          for (int i = 0; i < _NumEvents; i++)
//          {
//          	string iStr = ToString(i);
//              sbh.Append("\t\t_eventAssignments[" + iStr + "] = new TEventAssignmentDelegate (eventAssignment_" + iStr +
//                        ");" + NL());
//              sbh.Append("\t\t_computeEventAssignments[" + iStr +
//                        "] = new TComputeEventAssignmentDelegate (computeEventAssignment_" + iStr + ");" + NL());
//              sbh.Append("\t\t_performEventAssignments[" + iStr +
//                        "] = new TPerformEventAssignmentDelegate (performEventAssignment_" + iStr + ");" + NL());
//          }
//
//          sbh.Append("\t\tresetEvents();" + NL());
//          sbh.Append(NL());
//      }
//
//      if (_NumModifiableSpeciesReferences > 0)
//      {
//          for (int i = 0; i < ModifiableSpeciesReferenceList.size(); i++)
//          {
//              sbh.Append("\t\t_sr[" + ToString(i) + "]  = " + WriteDouble(ModifiableSpeciesReferenceList[i].value) + ";" + NL());
//          }
//          sbh.Append(NL());
//      }
//
//      // Declare space for local parameters
//      for (int i = 0; i < _NumReactions; i++)
//      {
//          sbh.Append("\t\tlocalParameterDimensions[" + ToString(i) + "] = " , _LocalParameterDimensions[i] , ";" + NL());
//          sbh.Append("\t\t_lp[" + ToString(i) + "] = new double[" , _LocalParameterDimensions[i] , "];" , NL());
//      }
//
//      sbh.Append("\t}" + NL() + NL());
}


void CGenerator::WriteComputeAllRatesOfChange(StringBuilder& sbh, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    sbh.Append("\t// Uses the equation: dSd/dt = L0 dSi/dt" + NL());
    sbh.Append("\t void computeAllRatesOfChange ()" + NL());
    sbh.Append("\t{" + NL());
    sbh.Append("\t\tdouble[] dTemp = new double[amounts.Length + rateRules.Length];" + NL());
    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        sbh.AppendFormat("\t\tdTemp[{0}] = {1};{2}", i, _oMapRateRule[i], NL());
    }
    //sbh.Append("\t\trateRules.CopyTo(dTemp, 0);" + NL());
    sbh.Append("\t\tamounts.CopyTo(dTemp, rateRules.Length);" + NL());
    sbh.Append("\t\tevalModel (time, dTemp);" + NL());
    bool isThereAnEntry = false;
    for (int i = 0; i < numDependentSpecies; i++)
    {
        sbh.AppendFormat("\t\t_dydt[{0}] = ", (numIndependentSpecies + i));
        isThereAnEntry = false;
        for (int j = 0; j < numIndependentSpecies; j++)
        {
            string dyName = Format("_dydt[{0}]", j);

            if (L0(i,j) > 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == 1)
                {
                    sbh.AppendFormat(" + {0}{1}", dyName, NL());
                }
                else
                {
                    sbh.AppendFormat(" + (double){0}{1}{2}{3}", WriteDouble(L0(i,j)), STR_FixAmountCompartments, dyName, NL());
                }
            }
            else if (L0(i,j) < 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == -1)
                {
                    sbh.AppendFormat(" - {0}{1}", dyName, NL());
                }
                else
                {
                    sbh.AppendFormat(" - (double){0}{1}{2}{3}", WriteDouble(fabs(L0(i,j))), STR_FixAmountCompartments, dyName, NL());
                }
            }
        }
        if (!isThereAnEntry)
        {
            sbh.Append("0");
        }
        sbh.AppendFormat(";{0}", NL());
    }

    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteComputeConservedTotals(StringBuilder& sbh, const int& numFloatingSpecies, const int& numDependentSpecies)
{
    sbh.Append("\t// Uses the equation: C = Sd - L0*Si" + NL());
    sbh.Append("\t void computeConservedTotals ()" + NL());
    sbh.Append("\t{" + NL());
    if (numDependentSpecies > 0)
    {
        string factor;
        double* matPtr = mStructAnalysis.GetGammaMatrix();

        DoubleMatrix gamma(matPtr, numDependentSpecies, numFloatingSpecies);
        for (int i = 0; i < numDependentSpecies; i++)
        {
            sbh.AppendFormat("\t\t_ct[{0}] = ", i);
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
                        sbh.Append(" + " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                    else
                    {
                        sbh.Append(" - " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                }
            }
            sbh.Append(";" + NL());
            conservationList.Add(Symbol("CSUM" + ToString(i))); //TODO: how to deal with this?
        }
    }
    sbh.Append("	}" + NL() + NL());
}

void CGenerator::WriteUpdateDependentSpecies(StringBuilder& sbh, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    sbh.Append("\t// Compute values of dependent species " + NL());
    sbh.Append("\t// Uses the equation: Sd = C + L0*Si" + NL());
    sbh.Append("\t void updateDependentSpeciesValues (double[] y)" + NL());
    sbh.Append("\t{" + NL());

    if (numDependentSpecies > 0)
    {
        // Use the equation: Sd = C + L0*Si to compute dependent concentrations
        if (numDependentSpecies > 0)
        {
            for (int i = 0; i < numDependentSpecies; i++)
            {
                sbh.AppendFormat("\t\t_y[{0}] = {1}\t", (i + numIndependentSpecies), NL());
                sbh.AppendFormat("(_ct[{0}]", i);
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
                            sbh.AppendFormat(" + {0}\t{1}{2}{3}{0}\t",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            sbh.AppendFormat("{0}\t + (double){1}{2}{3}{2}{4}",
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
                            sbh.AppendFormat("{0}\t - {1}{2}{3}",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            sbh.AppendFormat("{0}\t - (double){1}{2}{3}{2}{4}",
                                NL(),
                                WriteDouble(fabs(L0(i,j))),
                                STR_FixAmountCompartments,
                                yName,
                                cName);
                        }
                    }
                }
                sbh.AppendFormat(")/{0};{1}", cLeftName, NL());
            }
        }
    }
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteUserDefinedFunctions(StringBuilder& sbh)
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

            sbh.AppendFormat("\t// User defined function:  {0}{1}", sName, NL());
            sbh.AppendFormat("\t double {0} (", sName);

            for (int j = 0; j < oArguments.size(); j++)
            {
                sbh.Append("double " + (string)oArguments[j]);
                _functionParameters.Add((string)oArguments[j]);
                if (j < oArguments.size() - 1)
                    sbh.Append(", ");
            }
            sbh.Append(")" + NL() + "\t{" + NL() + "\t\t return " +
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

void CGenerator::WriteResetEvents(StringBuilder& sbh, const int& numEvents)
{
      sbh.AppendFormat("{0}\t void resetEvents() {{0}", NL());
      for (int i = 0; i < numEvents; i++)
      {
          sbh.AppendFormat("\t\t_eventStatusArray[{0}] = false;{1}", i, NL());
          sbh.AppendFormat("\t\t_previousEventStatusArray[{0}] = false;{1}", i, NL());
      }
      sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteSetConcentration(StringBuilder& sbh)
{
    sbh.AppendFormat("\t void setConcentration(int index, double value) {{0}", NL());
    sbh.AppendFormat("\t\tdouble volume = 0.0;{0}", NL());
    sbh.AppendFormat("\t\t_y[index] = value;{0}", NL());
    sbh.AppendFormat("\t\tswitch (index) {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
    	sbh.AppendFormat("\t\t\tcase {0}: volume = {1};{2}",
          i,
          convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
          NL());
      sbh.AppendFormat("\t\t\t\tbreak;{0}", NL());
    }
    sbh.AppendFormat("\t\t}{0}", NL());
    sbh.AppendFormat("\t\t_amounts[index] = _y[index]*volume;{0}", NL());
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteGetConcentration(StringBuilder& sbh)
{
    sbh.AppendFormat("\t double getConcentration(int index) {{0}", NL());
    sbh.AppendFormat("\t\treturn _y[index];{0}", NL());
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteConvertToAmounts(StringBuilder& sbh)
{
    sbh.AppendFormat("\t void convertToAmounts() {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sbh.AppendFormat("\t\t_amounts[{0}] = _y[{0}]*{1};{2}",
            i,
            convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
            NL());
    }
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteConvertToConcentrations(StringBuilder& sbh)
{
    sbh.Append("\t void convertToConcentrations() {" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sbh<<"\t\t_y[" << i << "] = _amounts[" << i << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }
    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteProperties(StringBuilder& sbh)
{
//    sbh.Append("\t double[] y {" + NL());
//    sbh.Append("\t\tget { return _y; } " + NL());
//    sbh.Append("\t\tset { _y = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] init_y {" + NL());
//    sbh.Append("\t\tget { return _init_y; } " + NL());
//    sbh.Append("\t\tset { _init_y = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] amounts {" + NL());
//    sbh.Append("\t\tget { return _amounts; } " + NL());
//    sbh.Append("\t\tset { _amounts = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] bc {" + NL());
//    sbh.Append("\t\tget { return _bc; } " + NL());
//    sbh.Append("\t\tset { _bc = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] gp {" + NL());
//    sbh.Append("\t\tget { return _gp; } " + NL());
//    sbh.Append("\t\tset { _gp = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] sr {" + NL());
//    sbh.Append("\t\tget { return _sr; } " + NL());
//    sbh.Append("\t\tset { _sr = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[][] lp {" + NL());
//    sbh.Append("\t\tget { return _lp; } " + NL());
//    sbh.Append("\t\tset { _lp = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] c {" + NL());
//    sbh.Append("\t\tget { return _c; } " + NL());
//    sbh.Append("\t\tset { _c = value; } " + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] dydt {" + NL());
//    sbh.Append("\t\tget { return _dydt; }" + NL());
//    sbh.Append("\t\tset { _dydt = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] rateRules {" + NL());
//    sbh.Append("\t\tget { return _rateRules; }" + NL());
//    sbh.Append("\t\tset { _rateRules = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] rates {" + NL());
//    sbh.Append("\t\tget { return _rates; }" + NL());
//    sbh.Append("\t\tset { _rates = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] ct {" + NL());
//    sbh.Append("\t\tget { return _ct; }" + NL());
//    sbh.Append("\t\tset { _ct = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] eventTests {" + NL());
//    sbh.Append("\t\tget { return _eventTests; }" + NL());
//    sbh.Append("\t\tset { _eventTests = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t TEventDelayDelegate[] eventDelay {" + NL());
//    sbh.Append("\t\tget { return _eventDelay; }" + NL());
//    sbh.Append("\t\tset { _eventDelay = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t bool[] eventType {" + NL());
//    sbh.Append("\t\tget { return _eventType; }" + NL());
//    sbh.Append("\t\tset { _eventType = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t bool[] eventPersistentType {" + NL());
//    sbh.Append("\t\tget { return _eventPersistentType; }" + NL());
//    sbh.Append("\t\tset { _eventPersistentType = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t bool[] eventStatusArray {" + NL());
//    sbh.Append("\t\tget { return _eventStatusArray; }" + NL());
//    sbh.Append("\t\tset { _eventStatusArray = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t bool[] previousEventStatusArray {" + NL());
//    sbh.Append("\t\tget { return _previousEventStatusArray; }" + NL());
//    sbh.Append("\t\tset { _previousEventStatusArray = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double[] eventPriorities {" + NL());
//    sbh.Append("\t\tget { return _eventPriorities; }" + NL());
//    sbh.Append("\t\tset { _eventPriorities = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t TEventAssignmentDelegate[] eventAssignments {" + NL());
//    sbh.Append("\t\tget { return _eventAssignments; }" + NL());
//    sbh.Append("\t\tset { _eventAssignments = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t TComputeEventAssignmentDelegate[] computeEventAssignments {" + NL());
//    sbh.Append("\t\tget { return _computeEventAssignments; }" + NL());
//    sbh.Append("\t\tset { _computeEventAssignments = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t TPerformEventAssignmentDelegate[] performEventAssignments {" + NL());
//    sbh.Append("\t\tget { return _performEventAssignments; }" + NL());
//    sbh.Append("\t\tset { _performEventAssignments = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
//
//    sbh.Append("\t double time {" + NL());
//    sbh.Append("\t\tget { return _time; }" + NL());
//    sbh.Append("\t\tset { _time = value; }" + NL());
//    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteAccessors(StringBuilder& sbh)
{
    sbh.Append("\t int getNumIndependentVariables {" + NL());
    sbh.Append("\t\tget { return numIndependentVariables; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumDependentVariables {" + NL());
    sbh.Append("\t\tget { return numDependentVariables; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumTotalVariables {" + NL());
    sbh.Append("\t\tget { return numTotalVariables; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumBoundarySpecies {" + NL());
    sbh.Append("\t\tget { return numBoundaryVariables; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumGlobalParameters {" + NL());
    sbh.Append("\t\tget { return numGlobalParameters; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumLocalParameters(int reactionId)" + NL());
    sbh.Append("\t{" + NL());
    sbh.Append("\t\treturn localParameterDimensions[reactionId];" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumCompartments {" + NL());
    sbh.Append("\t\tget { return numCompartments; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumReactions {" + NL());
    sbh.Append("\t\tget { return numReactions; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumEvents {" + NL());
    sbh.Append("\t\tget { return numEvents; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t int getNumRules {" + NL());
    sbh.Append("\t\tget { return numRules; }" + NL());
    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t List<string> Warnings {" + NL());
    sbh.Append("\t\tget { return _Warnings; }" + NL());
    sbh.Append("\t\tset { _Warnings = value; }" + NL());
    sbh.Append("\t}" + NL() + NL());
}



string CGenerator::FindSymbol(const string& varName)
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

void CGenerator::WriteTestConstraints(StringBuilder& sbh)
{
    sbh.Append("\t void testConstraints()" + NL());
    sbh.Append("\t{" + NL());

    for (int i = 0; i < mNOM.getNumConstraints(); i++)
    {
        string sMessage;
        string sCheck = mNOM.getNthConstraint(i, sMessage);

        sbh.Append("\t\tif (" + substituteTerms(mNOM.getNumReactions(), "", sCheck) + " == 0.0 )" + NL());
        sbh.Append("\t\t\tthrow new Exception(\"" + sMessage + "\");" + NL());
    }

    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteEvalInitialAssignments(StringBuilder& sbh, const int& numReactions)
{
    sbh.Append("\t void evalInitialAssignments()" + NL());
    sbh.Append("\t{" + NL());

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
                sbh.Append(leftSideRule + " = ");
                sbh.Append(substituteTerms(numReactions, "", rightSideRule) + ";" + NL());
            }
        }
    }
    for (int i = 0; i < mNOM.GetModel()->getNumEvents(); i++)
    {
        Event *current = mNOM.GetModel()->getEvent(i);
        string initialTriggerValue = ToString(current->getTrigger()->getInitialValue());//.ToString().ToLowerInvariant();
        sbh.Append("\t\t_eventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
        sbh.Append("\t\t_previousEventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
    }
    sbh.Append("\t}" + NL() + NL());
}

int CGenerator::WriteComputeRules(StringBuilder& sbh, const int& numReactions)
{
    IntStringHashTable mapVariables;
    int numRateRules = 0;
    int numOfRules = mNOM.getNumRules();

    sbh.Append("\t void computeRules(double[] y) {" + NL());
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
                sbh.Append(leftSideRule + " = ");
                int speciesIndex;
                bool isSpecies = floatingSpeciesConcentrationList.find(varName, speciesIndex);

                Symbol* symbol = (speciesIndex != -1) ? &(floatingSpeciesConcentrationList[speciesIndex]) : NULL;
                string sCompartment;

                if(isRateRule && mNOM.MultiplyCompartment(varName, sCompartment) && (rightSide.find(sCompartment) == string::npos))
                {
                    sbh.AppendFormat("({0}) * {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(sCompartment), NL());
                }
                else
                {
                    if (isSpecies && !isRateRule && symbol != NULL && symbol->hasOnlySubstance && symbol->compartmentName.size() != 0)
                    {
                        sbh.AppendFormat("({0}) / {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(symbol->compartmentName), NL());
                    }
                    else
                    {
                        sbh.AppendFormat("{0};{1}", substituteTerms(numReactions, "", rightSideRule), NL());
                    }
                }

                if (mNOM.IsCompartment(varName))
                {
                    sbh.Append("\t\tconvertToConcentrations();");
                }
            }
        }
        catch (const Exception& ex)
        {
            throw new SBWApplicationException("Error while trying to get Rule #" + ToString(i) + ex.Message);
        }
    }

    sbh.Append("\t}" + NL() + NL());
    sbh.Append("\t double[] _rateRules = new double[" + ToString(numRateRules) +
              "];           // Vector containing values of additional rate rules      " + NL());

    sbh.Append("\t void InitializeRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < numRateRules; i++)
    {
        sbh<<"\t\t_rateRules[" << i << "] = " << _oMapRateRule[i] << ";" << NL();
    }

    sbh.Append("\t}" + NL() + NL());
    sbh.Append("\t void AssignRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sbh<<(string)_oMapRateRule[i] << " = _rateRules[" << i << "];" << NL();
    }

    sbh.Append("\t}" + NL() + NL());

    sbh.Append("\t void InitializeRateRuleSymbols()" + NL() + "\t{" + NL());
    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        string varName = (string)mapVariables[i];
        double value = mNOM.getValue(varName);
        if (!IsNaN(value))
        {
            sbh<< _oMapRateRule[i] << " = " << ToString(value, STR_DoubleFormat) << ";" << NL();
        }
    }

    sbh.Append("\t}" + NL() + NL());
    sbh.Append("\t void AssignRates(double[] oRates)" + NL() + "\t{" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sbh<< _oMapRateRule[i] << " = oRates[" << i << "];" << NL();
    }

    sbh.Append("\t}" + NL() + NL());
    sbh.Append("\t double[] GetCurrentValues()" + NL() + "\t{" + NL());
    sbh.Append("\t\tdouble[] dResult = new double[" + ToString(NumAdditionalRates()) + "];" + NL());

    for (int i = 0; i < _oMapRateRule.size(); i++)
    {
        sbh<<"\t\tdResult[" << i << "] = " << _oMapRateRule[i] << ";" << NL();
    }
    sbh.Append("\t\treturn dResult;" + NL());

    sbh.Append("\t}" + NL() + NL());
    return numOfRules;
}

void CGenerator::WriteComputeReactionRates(StringBuilder& sbh, const int& numReactions)
{
    sbh.Append("\t// Compute the reaction rates" + NL());
    sbh.Append("\t void computeReactionRates (double time, double[] y)" + NL());
    sbh.Append("\t{" + NL());


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
        sbh.AppendFormat("\t\t_rates[{0}] = {1}{2}", i, modKineticLaw, NL());
    }
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteEvalEvents(StringBuilder& sbh, const int& numEvents, const int& numFloatingSpecies)
{
    sbh.Append("\t// Event handling function" + NL());
    sbh.Append("\t void evalEvents (double timeIn, double[] oAmounts)" + NL());
    sbh.Append("\t{" + NL());

    if (numEvents > 0)
    {
        for (int i = 0; i < NumAdditionalRates(); i++)
        {
            sbh<<(string) _oMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
        }
        for (int i = 0; i < numFloatingSpecies; i++)
        {
            sbh<<"\t\t_y[" << i << "] = oAmounts[" << (i + NumAdditionalRates()) << "]/" <<
                      convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
        }
    }

    sbh.Append("\t\t_time = timeIn;  // Don't remove" + NL());
    sbh.Append("\t\tupdateDependentSpeciesValues(_y);" + NL());
    sbh.Append("\t\tcomputeRules (_y);" + NL());

    for (int i = 0; i < numEvents; i++)
    {
        ArrayList ev = mNOM.getNthEvent(i);
        StringList tempList = ev[0];
        string eventString = tempList[0];

        eventString = substituteTerms(0, "", eventString);
        sbh<<"\t\tpreviousEventStatusArray[" << i << "] = eventStatusArray[" << i << "];" << NL();
        sbh.Append("\t\tif (" + eventString + " == 1.0) {" + NL());
        sbh.Append("\t\t     eventStatusArray[" + ToString(i) + "] = true;" + NL());
        sbh.Append("\t\t     eventTests[" + ToString(i) + "] = 1;" + NL());
        sbh.Append("\t\t} else {" + NL());
        sbh.Append("\t\t     eventStatusArray[" + ToString(i) + "] = false;" + NL());
        sbh.Append("\t\t     eventTests[" + ToString(i) + "] = -1;" + NL());
        sbh.Append("\t\t}" + NL());
    }
    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteEvalModel(StringBuilder& sbh, const int& numReactions, const int& numIndependentSpecies, const int& numFloatingSpecies, const int& numOfRules)
{
    sbh.Append("\t// Model Function" + NL());
    sbh.Append("\t void evalModel (double timein, double[] oAmounts)" + NL());
    sbh.Append("\t{" + NL());

    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        sbh<<(string)_oMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
    }

    for (int i = 0; i < numFloatingSpecies; i++)
    {
    	sbh<<"\t\t_y[" << i << "] = oAmounts[" << i + NumAdditionalRates() << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    sbh.Append(NL());
    sbh.Append("\t\tconvertToAmounts();" + NL());
    sbh.Append("\t\t_time = timein;  // Don't remove" + NL());
    sbh.Append("\t\tupdateDependentSpeciesValues (_y);" + NL());

    if (numOfRules > 0)
    {
        sbh.Append("\t\tcomputeRules (_y);" + NL());
    }

    sbh.Append("\t\tcomputeReactionRates (time, _y);" + NL());

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
            sbh<<"\t\t_dydt[" << i << "] =" << final << ";" << NL();
        }
    }

    sbh.Append("\t\tconvertToAmounts ();" + NL());
    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteEventAssignments(StringBuilder& sbh, const int& numReactions, const int& numEvents)
{
	StringList delays;
    vector<bool> eventType;
    vector<bool> eventPersistentType;
    if (numEvents > 0)
    {
        sbh.Append("\t// Event assignments" + NL());
        for (int i = 0; i < numEvents; i++)
        {
            ArrayList ev = mNOM.getNthEvent(i);
            eventType.push_back(mNOM.getNthUseValuesFromTriggerTime(i));
            eventPersistentType.push_back(mNOM.GetModel()->getEvent(i)->getTrigger()->getPersistent());

            StringList event = ev[1];
            int numItems = event.size();
            string str = substituteTerms(numReactions, "", event[0]);
            delays.Add(str);

            sbh.AppendFormat("\t void eventAssignment_{0} () {{1}", i, NL());
            sbh.AppendFormat("\t\tperformEventAssignment_{0}( computeEventAssignment_{0}() );{1}", i, NL());
            sbh.Append("\t}" + NL());
            sbh.AppendFormat("\t double[] computeEventAssignment_{0} () {{1}", i, NL());
            StringList oTemp;
            StringList oValue;
            int nCount = 0;
            int numAssignments = ev.size() - 2;
            sbh.AppendFormat("\t\tdouble[] values = new double[ {0}];{1}", numAssignments, NL());
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
                sbh.AppendFormat("\t\t{0};{1}", str, NL());
            }
            sbh.Append("\t\treturn values;" + NL());
            sbh.Append("\t}" + NL());
            sbh.AppendFormat("\t void performEventAssignment_{0} (double[] values) {{1}", i, NL());

            for (int j = 0; j < oTemp.size(); j++)
            {
                sbh.AppendFormat("\t\t{0} = values[{1}];{2}", oTemp[j], j, NL());
                string aStr = (string) oTemp[j];
                aStr = Trim(aStr);

                if (StartsWith(aStr, "_c[")) //Todo:May have to trim?
                {
                    sbh.Append("\t\tconvertToConcentrations();" + NL());
                }
            }

            sbh.Append("\t}" + NL());
        }
        sbh.Append("\t" + NL());
    }

    sbh.AppendFormat("{0}{0}\t void InitializeDelays() { {0}", NL());
    for (int i = 0; i < delays.size(); i++)
    {
        sbh.AppendFormat("\t\t_eventDelay[{0}] = new TEventDelayDelegate(delegate { return {1}; } );{2}", i, delays[i], NL());
        sbh.AppendFormat("\t\t_eventType[{0}] = {1};{2}", i, ToString((eventType[i] ? true : false)), NL());
        sbh.AppendFormat("\t\t_eventPersistentType[{0}] = {1};{2}", i, (eventPersistentType[i] ? "true" : "false"), NL());
    }
    sbh.AppendFormat("\t}{0}{0}", NL());

    sbh.AppendFormat("{0}{0}\t void computeEventPriorites() { {0}", NL());
    for (int i = 0; i < numEvents; i++)
    {
        Event* current = mNOM.GetModel()->getEvent(i);

        if (current->isSetPriority() && current->getPriority()->isSetMath())
        {
            string priority = SBML_formulaToString(current->getPriority()->getMath());
            sbh.AppendFormat("\t\t_eventPriorities[{0}] = {1};{2}", i, substituteTerms(numReactions, "", priority), NL());
        }
        else
        {
            sbh.AppendFormat("\t\t_eventPriorities[{0}] = 0f;{1}", i, NL());
        }
    }
    sbh.AppendFormat("\t}{0}{0}", NL());
}

void CGenerator::WriteSetParameterValues(StringBuilder& sbh, const int& numReactions)
{
    sbh.Append("\t void setParameterValues ()" + NL());
    sbh.Append("\t{" + NL());

    for (int i = 0; i < globalParameterList.size(); i++)
    {
        sbh.AppendFormat("\t\t{0} = (double){1};{2}",
                      convertSymbolToGP(globalParameterList[i].name),
                      WriteDouble(globalParameterList[i].value),
                      NL());
    }

    // Initialize local parameter values
    for (int i = 0; i < numReactions; i++)
    {
        for (int j = 0; j < localParameterList[i].size(); j++)
            sbh.AppendFormat("\t\t_lp[{0}][{1}] = (double){2};{3}",
                          i,
                          j,
                          WriteDouble(localParameterList[i][j].value),
                          NL());
    }

    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteSetCompartmentVolumes(StringBuilder& sbh)
{
    sbh.Append("\t void setCompartmentVolumes ()" + NL());
    sbh.Append("\t{" + NL());
    for (int i = 0; i < compartmentList.size(); i++)
    {
        sbh.Append("\t\t" + convertSymbolToC(compartmentList[i].name) + " = (double)" +
                  WriteDouble(compartmentList[i].value) + ";" + NL());

        // at this point we also have to take care of all initial assignments for compartments as well as
        // the assignment rules on compartments ... otherwise we are in trouble :)
		stack<string> initializations = mNOM.GetMatchForSymbol(compartmentList[i].name);
        while (initializations.size() > 0)
        {
        	string term(initializations.top());
            string sub = substituteTerms(_NumReactions, "", term);
            sbh.Append("\t\t" + sub + ";" + NL());
            initializations.pop();
        }
    }

    sbh.Append("\t}" + NL() + NL());
}

void CGenerator::WriteSetBoundaryConditions(StringBuilder& sbh)
{
    sbh.Append("\t void setBoundaryConditions ()" + NL());
    sbh.Append("\t{" + NL());
    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        if (IsNullOrEmpty(boundarySpeciesList[i].formula))
        {
            sbh.Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      WriteDouble(boundarySpeciesList[i].value) + ";" + NL());
        }
        else
        {
            sbh.Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      boundarySpeciesList[i].formula + ";" + NL());
        }
    }
    sbh.Append("\t}" + NL() + NL());
}


void CGenerator::WriteSetInitialConditions(StringBuilder& sbh, const int& numFloatingSpecies)
{
    sbh.Append("\t void initializeInitialConditions ()" + NL());
    sbh.Append("\t{" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        if (IsNullOrEmpty(floatingSpeciesConcentrationList[i].formula))
        {
            sbh.Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      WriteDouble(floatingSpeciesConcentrationList[i].value) + ";" + NL());
        }
        else
        {
            sbh.Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      floatingSpeciesConcentrationList[i].formula + ";" + NL());
        }
    }
    sbh.Append(NL());

    sbh.Append("\t}" + NL() + NL());

    // ------------------------------------------------------------------------------
    sbh.Append("\t void setInitialConditions ()" + NL());
    sbh.Append("\t{" + NL());

    for (int i = 0; i < numFloatingSpecies; i++)
    {
        sbh<<"\t\t_y[" << i << "] =  _init_y[" << i << "];" << NL();
        sbh<<"\t\t_amounts[" << i << "] = _y[" << i << "]*" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    sbh.Append(NL());
	sbh.Append("\t}" + NL() + NL());
}



string CGenerator::convertSpeciesToY(const string& speciesName)
{
    int index;
    if (floatingSpeciesConcentrationList.find(speciesName, index))
    {
        return "_y[" + ToString(index) + "]";
    }
    throw new SBWApplicationException("Internal Error: Unable to locate species: " + speciesName);
}

string CGenerator::convertSpeciesToBc(const string& speciesName)
{
    int index;
    if (boundarySpeciesList.find(speciesName, index))
    {
        return "_bc[" + ToString(index) + "]";
    }
	throw SBWApplicationException("Internal Error: Unable to locate species: " + speciesName);
}

string CGenerator::convertCompartmentToC(const string& compartmentName)
{
    int index;
    if (compartmentList.find(compartmentName, index))
    {
        return "_c[" + ToString(index) + "]";
    }

    throw RRException("Internal Error: Unable to locate compartment: " + compartmentName);
}

string CGenerator::convertSymbolToGP(const string& parameterName)
{
    int index;
    if (globalParameterList.find(parameterName, index))
    {
        return "_gp[" + ToString(index) + "]";
    }
      throw SBWApplicationException("Internal Error: Unable to locate parameter: " + parameterName);
}

string CGenerator::convertSymbolToC(const string& compartmentName)
{
	int index;
    if (compartmentList.find(compartmentName, index))
    {
        return "_c[" + ToString(index) + "]";
    }
      throw SBWApplicationException("Internal Error: Unable to locate compartment: " + compartmentName);
}

string CGenerator::convertUserFunctionExpression(const string& equation)
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
    StringBuilder  sbh;

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
                    	sbh.Append("Math.Pow");
                    }
					else if(theToken == "sqrt")
                    {
                        sbh.Append("Math.Sqrt");
                  	}
                    else if(theToken == "log")
                    {
                    	sbh.Append("supportFunctions._log");
                    }
                    else if(theToken == "log10")
                    {
                        sbh.Append("Math.Log10");
                    }
                    else if(theToken == "floor")
                    {
                        sbh.Append("Math.Floor");
                    }
                    else if(theToken == "ceil")
                    {
                    	sbh.Append("Math.Ceiling");
                    }
                    else if(theToken == "factorial")
                    {
                    	sbh.Append("supportFunctions._factorial");
                    }
                    else if(theToken == "exp")
                    {
                    	sbh.Append("Math.Exp");
                    }
                    else if(theToken == "sin")
                    {
                    	sbh.Append("Math.Sin");
                    }
                    else if(theToken == "cos")
                    {
                        sbh.Append("Math.Cos");
                    }
                    else if(theToken == "tan")
                    {
                        sbh.Append("Math.Tan");
                    }
                    else if(theToken == "abs")
                    {
                        sbh.Append("Math.Abs");
                    }
                    else if(theToken == "asin")
                    {
                        sbh.Append("Math.Asin");
                    }
                    else if(theToken == "acos")
                    {
                        sbh.Append("Math.Acos");
                    }
                    else if(theToken == "atan")
                    {
                    	sbh.Append("Math.Atan");
                    }
                    else if(theToken == "sec")
                    {
                        sbh.Append("MathKGI.Sec");
                    }
                    else if(theToken == "csc")
                    {
                        sbh.Append("MathKGI.Csc");
                    }
                    else if(theToken == "cot")
                    {
                        sbh.Append("MathKGI.Cot");
                    }
                    else if(theToken == "arcsec")
                    {
                        sbh.Append("MathKGI.Asec");
                    }
                    else if(theToken == "arccsc")
                    {
                        sbh.Append("MathKGI.Acsc");
                    }
                    else if(theToken == "arccot")
                    {
                        sbh.Append("MathKGI.Acot");
                    }
                    else if(theToken == "sinh")
                    {
                        sbh.Append("Math.Sinh");
                    }
                    else if(theToken == "cosh")
                    {
                        sbh.Append("Math.Cosh");
                    }
                    else if(theToken == "tanh")
                    {
                        sbh.Append("Math.Tanh");
                    }
                    else if(theToken == "arcsinh")
                    {
                        sbh.Append("MathKGI.Asinh");
                    }
                    else if(theToken == "arccosh")
                    {
                        sbh.Append("MathKGI.Acosh");
                    }
                    else if(theToken == "arctanh")
                    {
                        sbh.Append("MathKGI.Atanh");
                    }
                    else if(theToken == "sech")
                    {
                        sbh.Append("MathKGI.Sech");
                    }
                    else if(theToken == "csch")
                    {
                        sbh.Append("MathKGI.Csch");
                    }
                    else if(theToken == "coth")
                    {
                        sbh.Append("MathKGI.Coth");
                    }
                    else if(theToken == "arcsech")
                    {
                        sbh.Append("MathKGI.Asech");
                    }
                    else if(theToken == "arccsch")
                    {
                        sbh.Append("MathKGI.Acsch");
                    }
                    else if(theToken == "arccoth")
                    {
                               sbh.Append("MathKGI.Acoth");
                    }
                    else if(theToken == "pi")
                    {
                        sbh.Append("Math.PI");
                    }
                    else if(theToken == "exponentiale")
                    {
                        sbh.Append("Math.E");
                    }
                    else if(theToken == "avogadro")
                    {
                        sbh.Append("6.02214179e23");
                    }
                    else if(theToken == "true")
                    {
                               //sbh.Append("true");
                        sbh.Append("1.0");
                    }
                    else if(theToken == "false")
                    {
                               //sbh.Append("false");
                        sbh.Append("0.0");
                    }
                    else if(theToken == "gt")
                    {
                        sbh.Append("supportFunctions._gt");
                    }
                    else if(theToken == "lt")
                    {
                        sbh.Append("supportFunctions._lt");
                    }
                    else if(theToken == "eq")
                    {
                        sbh.Append("supportFunctions._eq");
                    }
                    else if(theToken == "neq")
                    {
                        sbh.Append("supportFunctions._neq");
                    }
                    else if(theToken == "geq")
                    {
                        sbh.Append("supportFunctions._geq");
                    }
                    else if(theToken == "leq")
                    {
                        sbh.Append("supportFunctions._leq");
                    }
                    else if(theToken == "and")
                    {
                        sbh.Append("supportFunction._and");
                    }
                    else if(theToken == "or")
                    {
                        sbh.Append("supportFunction._or");
                    }
                    else if(theToken == "not")
                    {
                        sbh.Append("supportFunction._not");
                    }
                    else if(theToken == "xor")
                    {
                        sbh.Append("supportFunction._xor");
                    }
                    else if(theToken == "root")
                    {
                        sbh.Append("supportFunctions._root");
                    }
                    else if(theToken == "piecewise")
                    {
                        sbh.Append("supportFunctions._piecewise");
                    }
                    else if (!_functionParameters.Contains(s.tokenString))
                    {
                    	throw Exception("Token '" + s.tokenString + "' not recognized.");
                    }
                    else
                    {
                    	sbh.Append(s.tokenString);
                	}

				break; //Word token

               	case CodeTypes::tDoubleToken:
                   	sbh.Append(WriteDouble(s.tokenDouble));
                   	break;
               	case CodeTypes::tIntToken:
                	sbh.Append((int) s.tokenInteger);
                   	break;
               	case CodeTypes::tPlusToken:
                   sbh.Append("+");
                   break;
               	case CodeTypes::tMinusToken:
                   sbh.Append("-");
                   break;
               	case CodeTypes::tDivToken:
                   sbh.Append("/");
                   break;
               	case CodeTypes::tMultToken:
                   sbh.Append(STR_FixAmountCompartments);
                   break;
               	case CodeTypes::tPowerToken:
                   sbh.Append("^");
                   break;
               	case CodeTypes::tLParenToken:
                   sbh.Append("(");
                   break;
               	case CodeTypes::tRParenToken:
                   sbh.Append(")");
                   break;
               	case CodeTypes::tCommaToken:
                   sbh.Append(",");
                   break;
               	case CodeTypes::tEqualsToken:
                   sbh.Append(" = ");
                   break;
               	case CodeTypes::tTimeWord1:
                   sbh.Append("time");
                   break;
               	case CodeTypes::tTimeWord2:
                   sbh.Append("time");
                   break;
               	case CodeTypes::tTimeWord3:
                   sbh.Append("time");
                   break;
               	case CodeTypes::tAndToken:
                   sbh.Append("supportFunctions._and");
                   break;
               	case CodeTypes::tOrToken:
                   sbh.Append("supportFunctions._or");
                   break;
               	case CodeTypes::tNotToken:
                   sbh.Append("supportFunctions._not");
                   break;
               	case CodeTypes::tLessThanToken:
                   sbh.Append("supportFunctions._lt");
                   break;
               	case CodeTypes::tLessThanOrEqualToken:
                   sbh.Append("supportFunctions._leq");
                   break;
               	case CodeTypes::tMoreThanOrEqualToken:
                   sbh.Append("supportFunctions._geq");
                   break;
               	case CodeTypes::tMoreThanToken:
                   sbh.Append("supportFunctions._gt");
                   break;
               	case CodeTypes::tXorToken:
                   sbh.Append("supportFunctions._xor");
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
	return sbh.ToString();
}

void CGenerator::SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& sbh)
{
	string theToken(s.tokenString);
    if(theToken == "pow")
    {
        sbh.Append("Math.Pow");
    }
    else if(theToken == "sqrt")
    {
        sbh.Append("Math.Sqrt");
    }
    else if(theToken == "log")
    {
        sbh.Append("supportFunctions._log");
    }
    else if(theToken == "floor")
    {
        sbh.Append("Math.Floor");
    }
    else if(theToken == "ceil")
    {
        sbh.Append("Math.Ceiling");
    }
    else if(theToken == "factorial")
    {
        sbh.Append("supportFunctions._factorial");
    }
    else if(theToken == "log10")
    {
        sbh.Append("Math.Log10");
    }
    else if(theToken == "exp")
    {
        sbh.Append("Math.Exp");
    }
    else if(theToken == "abs")
    {
        sbh.Append("Math.Abs");
    }
    else if(theToken == "sin")
    {
        sbh.Append("Math.Sin");
    }
    else if(theToken == "cos")
    {
        sbh.Append("Math.Cos");
    }
    else if(theToken == "tan")
    {
        sbh.Append("Math.Tan");
    }
    else if(theToken == "asin")
    {
        sbh.Append("Math.Asin");
    }
    else if(theToken == "acos")
    {
        sbh.Append("Math.Acos");
    }
    else if(theToken == "atan")
    {
        sbh.Append("Math.Atan");
    }
    else if(theToken == "sec")
    {
        sbh.Append("MathKGI.Sec");
    }
    else if(theToken == "csc")
    {
        sbh.Append("MathKGI.Csc");
    }
    else if(theToken == "cot")
    {
        sbh.Append("MathKGI.Cot");
    }
    else if(theToken == "arcsec")
    {
        sbh.Append("MathKGI.Asec");
    }
    else if(theToken == "arccsc")
    {
        sbh.Append("MathKGI.Acsc");
    }
    else if(theToken == "arccot")
    {
        sbh.Append("MathKGI.Acot");
    }
    else if(theToken == "sinh")
    {
        sbh.Append("Math.Sinh");
    }
    else if(theToken == "cosh")
    {
        sbh.Append("Math.Cosh");
    }
    else if(theToken == "tanh")
    {
        sbh.Append("Math.Tanh");
    }
    else if(theToken == "arcsinh")
    {
        sbh.Append("MathKGI.Asinh");
    }
    else if(theToken == "arccosh")
    {
        sbh.Append("MathKGI.Acosh");
    }
    else if(theToken == "arctanh")
    {
        sbh.Append("MathKGI.Atanh");
    }
    else if(theToken == "sech")
    {
        sbh.Append("MathKGI.Sech");
    }
    else if(theToken == "csch")
    {
        sbh.Append("MathKGI.Csch");
    }
    else if(theToken == "coth")
    {
        sbh.Append("MathKGI.Coth");
    }
    else if(theToken == "arcsech")
    {
        sbh.Append("MathKGI.Asech");
    }
    else if(theToken == "arccsch")
    {
        sbh.Append("MathKGI.Acsch");
    }
    else if(theToken == "arccoth")
    {
        sbh.Append("MathKGI.Acoth");
    }
    else if(theToken == "pi")
    {
        sbh.Append("Math.PI");
    }
    else if(theToken == "avogadro")
    {
        sbh.Append("6.02214179e23");
    }
    else if(theToken == "exponentiale")
    {
        sbh.Append("Math.E");
    }
    else if(theToken == "true")
    {
        //sbh.Append("true");
        sbh.Append("1.0");
    }
    else if(theToken == "false")
    {
        //sbh.Append("false");
        sbh.Append("0.0");
    }
    else if(theToken == "NaN")
    {
        sbh.Append("double.NaN");
    }
    else if(theToken == "INF")
    {
        sbh.Append("double.PositiveInfinity");
    }
    else if(theToken == "geq")
    {
        sbh.Append("supportFunctions._geq");
    }
    else if(theToken == "leq")
    {
        sbh.Append("supportFunctions._leq");
    }
    else if(theToken == "gt")
    {
        sbh.Append("supportFunctions._gt");
    }
    else if(theToken == "lt")
    {
        sbh.Append("supportFunctions._lt");
    }
    else if(theToken == "eq")
    {
        sbh.Append("supportFunctions._eq");
    }
    else if(theToken == "neq")
    {
        sbh.Append("supportFunctions._neq");
    }
    else if(theToken == "and")
    {
        sbh.Append("supportFunction._and");
    }
    else if(theToken == "or")
    {
        sbh.Append("supportFunction._or");
    }
    else if(theToken == "not")
    {
        sbh.Append("supportFunction._not");
    }
    else if(theToken == "xor")
    {
        sbh.Append("supportFunction._xor");
    }
    else if(theToken == "root")
    {
        sbh.Append("supportFunctions._root");
    }
    else if(theToken == "piecewise")
    {
        sbh.Append("supportFunctions._piecewise");
    }
    else if(theToken == "delay")
    {
        sbh.Append("supportFunctions._delay");
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
                sbh.Append("_lp[" + ToString(index) + "][" + ToString(nParamIndex) + "]");
                bReplaced = true;
            }
        }

        if (boundarySpeciesList.find(s.tokenString, index))
        {
            sbh.Append("_bc[" + ToString(index) + "]");
            bReplaced = true;
        }
        if (!bReplaced &&
            (_functionParameters.size() != 0 && !_functionParameters.Contains(s.tokenString)))
        {
            throw Exception("Token '" + s.tokenString + "' not recognized.");
        }
    }
}

void CGenerator::SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sbh)
{
    // Global parameters have priority
    int index;
    if (globalParameterList.find(s.tokenString, index))
    {
        sbh.AppendFormat("_gp[{0}]", index);
    }
    else if (boundarySpeciesList.find(s.tokenString, index))
    {
        sbh.AppendFormat("_bc[{0}]", index);

        Symbol symbol = boundarySpeciesList[index];
        if (symbol.hasOnlySubstance)
        {
            // we only store concentration for the boundary so we better
            // fix that.
            int nCompIndex = 0;
            if (compartmentList.find(symbol.compartmentName, nCompIndex))
            {
                sbh.AppendFormat("{0}_c[{1}]", STR_FixAmountCompartments, nCompIndex);
            }
        }
    }
    else if (floatingSpeciesConcentrationList.find(s.tokenString, index))
    {
        Symbol floating1 = floatingSpeciesConcentrationList[index];
        if (floating1.hasOnlySubstance)
        {
            sbh.AppendFormat("amounts[{0}]", index);
        }
        else
        {
            sbh.AppendFormat("_y[{0}]", index);
        }
    }
    else if (compartmentList.find(s.tokenString, index))
    {
        sbh.AppendFormat("_c[{0}]", index);
    }
    else if (_functionNames.Contains(s.tokenString))
    {
        sbh.AppendFormat("{0} ", s.tokenString);
    }
    else if (ModifiableSpeciesReferenceList.find(s.tokenString, index))
    {
        sbh.AppendFormat("_sr[{0}]", index);
    }
    else if (reactionList.find(s.tokenString, index))
    {
        sbh.AppendFormat("_rates[{0}]", index);
    }
    else
    {
        SubstituteEquation(reactionName, s, sbh);
	}
}

void CGenerator::SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& sbh)
{
	string aToken = s.tokenString;
	CodeTypes codeType = s.token();
    switch(codeType)
    {
        case CodeTypes::tWordToken:
        case CodeTypes::tExternalToken:
        case CodeTypes::tExtToken:
            SubstituteWords(reactionName, bFixAmounts, s, sbh);
            break;

        case CodeTypes::tDoubleToken:
            sbh.Append("(double)" + WriteDouble(s.tokenDouble));
            break;
        case CodeTypes::tIntToken:
            sbh.Append("(double)" + WriteDouble((double)s.tokenInteger));
            break;
        case CodeTypes::tPlusToken:
            sbh.AppendFormat("+{0}\t", NL());
            break;
        case CodeTypes::tMinusToken:
            sbh.AppendFormat("-{0}\t", NL());
            break;
        case CodeTypes::tDivToken:
            sbh.AppendFormat("/{0}\t", NL());
            break;
        case CodeTypes::tMultToken:
            sbh.AppendFormat("*{0}\t", NL());
            break;
        case CodeTypes::tPowerToken:
            sbh.AppendFormat("^{0}\t", NL());
            break;
        case CodeTypes::tLParenToken:
            sbh.Append("(");
            break;
        case CodeTypes::tRParenToken:
            sbh.AppendFormat("){0}\t", NL());
            break;
        case CodeTypes::tCommaToken:
            sbh.Append(",");
            break;
        case CodeTypes::tEqualsToken:
            sbh.AppendFormat(" = {0}\t", NL());
            break;
      case CodeTypes::tTimeWord1:
            sbh.Append("time");
            break;
        case CodeTypes::tTimeWord2:
            sbh.Append("time");
            break;
        case CodeTypes::tTimeWord3:
            sbh.Append("time");
            break;
        case CodeTypes::tAndToken:
            sbh.AppendFormat("{0}supportFunctions._and", NL());
            break;
        case CodeTypes::tOrToken:
            sbh.AppendFormat("{0}supportFunctions._or", NL());
            break;
        case CodeTypes::tNotToken:
            sbh.AppendFormat("{0}supportFunctions._not", NL());
            break;
        case CodeTypes::tLessThanToken:
            sbh.AppendFormat("{0}supportFunctions._lt", NL());
            break;
        case CodeTypes::tLessThanOrEqualToken:
            sbh.AppendFormat("{0}supportFunctions._leq", NL());
            break;
        case CodeTypes::tMoreThanOrEqualToken:
            sbh.AppendFormat("{0}supportFunctions._geq", NL());
            break;
        case CodeTypes::tMoreThanToken:
            sbh.AppendFormat("{0}supportFunctions._gt", NL());
            break;
        case CodeTypes::tXorToken:
            sbh.AppendFormat("{0}supportFunctions._xor", NL());
            break;
        default:
        string aToken = s.tokenToString(s.token());
        Exception ae = Exception(
                 Format("Unknown token in substituteTerms: {0}", aToken,
                 "Exception raised in Module:roadRunner, Method:substituteTerms"));
         throw ae;
    }
}

void CGenerator::WriteOutSymbolTables(StringBuilder& sbh)
{
    sbh.Append("\tvoid loadSymbolTables() {" + NL());

    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        sbh.AppendFormat("\t\tvariableTable[{0}] = \"{1}\";{2}", i, floatingSpeciesConcentrationList[i].name, NL());
    }

    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        sbh.AppendFormat("\t\tboundaryTable[{0}] = \"{1}\";{2}", i, boundarySpeciesList[i].name, NL());
    }

	for (int i = 0; i < globalParameterList.size(); i++)
    {
		string name = globalParameterList[i].name;
       	sbh.AppendFormat("\t\tglobalParameterTable[{0}] = \"{1}\";{2}", i, globalParameterList[i].name, NL());
    }
    sbh.AppendFormat("\t}{0}{0}", NL());
}

int CGenerator::ReadFloatingSpecies()
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

int CGenerator::ReadBoundarySpecies()
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

void CGenerator::WriteInitFunction(StringBuilder& sbh, StringBuilder& sbc)
{
	sbh	<<"\n//Initialize DLL data, i.e. the TModel struct, and return integer indicating result\n"
    	<<"D_S "<<"int InitModel();\n";

	sbc	<<"\n//Initialize DLL data, i.e. the TModel struct, and return integer indicating result\n"
    	<<"D_S "<<"int InitModel()\n"
    	<<"{\n"
		<<"\tgTheModel._gp[0] = 1234;\n"
    	<<"\treturn 0;\n"
    	<<"}\n";
}
}//Namespace


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
	return mHeader.ToString();
}

string CGenerator::GetSourceCode()
{
	return mSource.ToString();
}

string CGenerator::GetHeaderCodeFileName()
{
	return mHeaderCodeFileName;
}

string CGenerator::GetSourceCodeFileName()
{
	return mSourceCodeFileName;
}

bool CGenerator::SaveSourceCodeToFolder(const string& folder)
{
    mHeaderCodeFileName = folder + string("\\") + mCurrentXMLModelFileName;
    mHeaderCodeFileName = ChangeFileNameExtensionTo(mHeaderCodeFileName, ".h");

    ofstream outFile(mHeaderCodeFileName.c_str());
    if(!outFile)
    {
        throw(Exception("Failed to open file:" + mHeaderCodeFileName));
    }
    outFile<<GetHeaderCode();
    Log(lInfo)<<"Wrote header to file: "<<mHeaderCodeFileName;
    outFile.close();

    mSourceCodeFileName = ChangeFileNameExtensionTo(mHeaderCodeFileName, ".c");
    outFile.open(mSourceCodeFileName.c_str());

    //We don't know the name of the file until here..
    //Write an include statement to it..
    vector<string> fNameParts = SplitString(mSourceCodeFileName,"\\");
    string headerFName = fNameParts[fNameParts.size() - 1];

    headerFName = ChangeFileNameExtensionTo(headerFName, ".h");
    outFile<<"#include \""<<headerFName<<"\"\n"<<endl;
    outFile<<GetSourceCode();
    outFile.close();
    Log(lInfo)<<"Wrote source code to file: "<<mSourceCodeFileName;

	return true;
}

// Generates the Model Code from the SBML string
string CGenerator::generateModelCode(const string& sbmlStr)
{
	Log(lDebug4)<<"Entering CGenerators generateModelCode(string) function";

    StringList  	Warnings;

	StringBuilder ignore; 	//The Write functions below are inherited with a stringbuilder in the
    						//prototype that is not to be used..

    //Clear header and source file objects...
	mHeader.Clear();
  	mSource.Clear();
	mNOM.Reset();
    string sASCII = mNOM.convertTime(sbmlStr, "time");

	Log(lDebug4)<<"Loading SBML into NOM";
	mNOM.loadSBML(sASCII.c_str(), "time");

    mModelName = mNOM.getModelName();
    if(!mModelName.size())
    {
        Log(lError)<<"Model name is empty! Exiting...";
    	return "";
    }

    Log(lDebug3)<<"Model name is "<<mModelName;
    mNumReactions = mNOM.getNumReactions();

    Log(lDebug3)<<"Number of reactions:"<<mNumReactions;

	globalParameterList.Clear();
    ModifiableSpeciesReferenceList.Clear();
    localParameterList.reserve(mNumReactions);
    reactionList.Clear();
    boundarySpeciesList.Clear();
    floatingSpeciesConcentrationList.Clear();
    floatingSpeciesAmountsList.Clear();
    compartmentList.Clear();
    conservationList.Clear();
    mfunctionNames.empty();
    mfunctionParameters.empty();

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

	if (RoadRunner::mbComputeAndAssignConservationLaws)
    {
        mNumIndependentSpecies = mStructAnalysis.GetNumIndependentSpecies();
        independentSpeciesList = mStructAnalysis.GetIndependentSpeciesIds();
        dependentSpeciesList   = mStructAnalysis.GetDependentSpeciesIds();
    }
    else
    {
        mNumIndependentSpecies = mStructAnalysis.GetNumSpecies();
        independentSpeciesList = mStructAnalysis.GetSpeciesIds();
    }

    // Load the compartment array (name and value)
	mNumCompartments 		= ReadCompartments();

    // Read FloatingSpecies
    mNumFloatingSpecies 	= ReadFloatingSpecies();
    mNumDependentSpecies 	= mNumFloatingSpecies - mNumIndependentSpecies;

    // Load the boundary species array (name and value)
	mNumBoundarySpecies 	= ReadBoundarySpecies();

    // Get all the parameters into a list, global and local
    mNumGlobalParameters 	= ReadGlobalParameters();
	mNumModifiableSpeciesReferences = ReadModifiableSpeciesReferences();

    // Load up local parameters next
	ReadLocalParameters(mNumReactions, mLocalParameterDimensions, mTotalLocalParmeters);
    mNumEvents = mNOM.getNumEvents();

    //Write model to String builder...
	WriteClassHeader(ignore);
    WriteOutVariables(ignore);
    WriteOutSymbolTables(ignore);
    WriteResetEvents(ignore, mNumEvents);
//    WriteSetConcentration(ignore);
//    WriteGetConcentration(ignore);
//    WriteConvertToAmounts(ignore);
//    WriteConvertToConcentrations(ignore);
//    WriteProperties(ignore);
//    WriteAccessors(ignore);
//    WriteUserDefinedFunctions(ignore);
//    WriteSetInitialConditions(ignore, mNumFloatingSpecies);
//    WriteSetBoundaryConditions(ignore);
//    WriteSetCompartmentVolumes(ignore);
//    WriteSetParameterValues(ignore, mNumReactions);
//   	WriteComputeConservedTotals(ignore, mNumFloatingSpecies, mNumDependentSpecies);
//
//
//    // Get the L0 matrix
//    int nrRows;
//    int nrCols;
//    double* aL0 = InitializeL0(nrRows, nrCols); 	//Todo: What is this doing? answer.. it is used below..
//    DoubleMatrix L0(aL0,nrRows, nrCols); 		//How many rows and cols?? We need to know that in order to use the matrix properly!
//
//    WriteUpdateDependentSpecies(ignore, mNumIndependentSpecies, mNumDependentSpecies, L0);
//    int numOfRules = WriteComputeRules(ignore, mNumReactions);
//    WriteComputeAllRatesOfChange(ignore, mNumIndependentSpecies, mNumDependentSpecies, L0);
//    WriteComputeReactionRates(ignore, mNumReactions);
//    WriteEvalModel(ignore, mNumReactions, mNumIndependentSpecies, mNumFloatingSpecies, numOfRules);
//    WriteEvalEvents(ignore, mNumEvents, mNumFloatingSpecies);
    WriteEventAssignments(ignore, mNumReactions, mNumEvents);
//    WriteEvalInitialAssignments(ignore, mNumReactions);
//    WriteTestConstraints(ignore);

	mHeader<<Format("} g;\t//This is global data in the DLL{0}", NL());


    ///// Write non exports
   	mHeader.NewLine("\n//NON - EXPORTS ========================================");
	mHeader<<"void"<<tabs(4)<<"InitializeDelays();";

    ///// Write exported functions
   	mHeader.NewLine("\n//EXPORTS ========================================");
    mHeader<<"D_S int"<<tabs(4)<<"InitModel();"<<endl;
    mHeader<<"D_S char*"<<tabs(3)<<"GetModelName();"<<endl;
   	mHeader.NewLine("\n");
    ///////////////


	WriteInitFunction(mHeader, mSource);

    mHeader<<"#endif //modelH"<<NL();
	return mHeader.ToString() + mSource.ToString();
}

void CGenerator::WriteClassHeader(StringBuilder& ignore)
{
	//Create c code header file....
    mHeader<<"#ifndef modelH"<<endl;
    mHeader<<"#define modelH"<<endl;
    mHeader<<"#include <stdio.h>"<<endl;
    mHeader<<"#include <stdbool.h>"<<endl;
    mHeader<<"\n#if defined(BUILD_MODEL_DLL)\n"
    	<<"#define D_S __declspec(dllexport)\n"
        <<"#else\n"
        <<"#define D_S __declspec(dllimport)\n"
        <<"#endif\n";

    mHeader<<Append("//************************************************************************** " + NL());
    mHeader<<Format("\t// Model Symbol Mappings{0}{0}", NL());
	for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
    	mHeader<<"\t// y["<<i<<"] = "<<floatingSpeciesConcentrationList[i].name<<endl;//{2}", NL());
    }

    mHeader<<Append("//************************************************************************** " + NL());
    mHeader<<Append(NL());
    mHeader<<Append(NL());
    mHeader<<Format("D_S struct TModel{0}", NL());
    mHeader<<Append("{" + NL());

    //Header of the source file...
    mSource<<"#include <stdio.h>"<<endl;

}

void CGenerator::WriteOutVariables(StringBuilder& ignore)
{
    mHeader.FormatVariable("char*", 								"mModelName");
    mHeader.FormatVariable("char**", 							"mWarnings");
	mHeader.FormatArray("double",								"_gp", 					mNumGlobalParameters +
    																				mTotalLocalParmeters, 						"Vector containing all the global parameters in the System  ");

	if(mNumModifiableSpeciesReferences)
    {
    	mHeader.FormatArray("double", 							"_sr", 					mNumModifiableSpeciesReferences, 			"Vector containing all the modifiable species references  ");
    }

	//Arrays
    mHeader.FormatArray("double*", 								"_lp",					mNumReactions, 								"Vector containing all the local parameters in the System  ");
	mHeader.FormatArray("double", 	                            "_y", 					floatingSpeciesConcentrationList.size(),	"Vector containing the concentrations of all floating species");
    mHeader.FormatArray("double",	                            "_init_y", 				floatingSpeciesConcentrationList.Count(), 	"Vector containing the initial concentrations of all floating species");
    mHeader.FormatArray("double",	                            "_amounts", 			floatingSpeciesConcentrationList.size(),  	"Vector containing the amounts of all floating species ");
    mHeader.FormatArray("double",	                            "_bc",					mNumBoundarySpecies,					 	"Vector containing all the boundary species concentration values");
    mHeader.FormatArray("double",	                            "_c",					mNumCompartments 						,  	"Vector containing all the compartment values   ");
    mHeader.FormatArray("double",	                            "_dydt",		 		floatingSpeciesConcentrationList.size() ,  	"Vector containing rates of changes of all species   ");
    mHeader.FormatArray("double",	                            "_rates",				mNumReactions 							, 	"Vector containing the rate laws of all reactions    ");
    mHeader.FormatArray("double",	                            "_ct",					mNumDependentSpecies 					,  	"Vector containing values of all conserved sums      ");
    mHeader.FormatArray("double",	                            "_eventTests",			mNumEvents 								, 	"Vector containing results of any event tests        ");
    mHeader.FormatArray("//TEventDelayDelegate",	                "_eventDelay",			mNumEvents 								, 	"Array of trigger function pointers");
    mHeader.FormatArray("bool",				  	                "_eventType",			mNumEvents								, 	"Array holding the status whether events are useValuesFromTriggerTime or not");
    mHeader.FormatArray("bool",				  	                "_eventPersistentType", mNumEvents								, 	"Array holding the status whether events are persitstent or not");

	mHeader.FormatVariable("double",  					    	"_time");
    mHeader.FormatVariable("int",	  						    "numIndependentVariables");
    mHeader.FormatVariable("int",	  						    "numDependentVariables");
    mHeader.FormatVariable("int",	  						    "numTotalVariables");
    mHeader.FormatVariable("int",	  						    "numBoundaryVariables");
    mHeader.FormatVariable("int",	  						    "numGlobalParameters");
    mHeader.FormatVariable("int",	  						    "numCompartments");
    mHeader.FormatVariable("int",	  						    "numReactions");
    mHeader.FormatVariable("int",	  						    "numRules");
    mHeader.FormatVariable("int",	  						    "numEvents");

    mHeader.FormatArray("char*",						        	"variableTable", 				floatingSpeciesConcentrationList.size());
    mHeader.FormatArray("char*",						        	"boundaryTable", 				boundarySpeciesList.size());
    mHeader.FormatArray("char*",						        	"globalParameterTable", 		globalParameterList.size());
    mHeader.FormatArray("int",							        "localParameterDimensions", 	mNumReactions );
    mHeader.FormatVariable("//TEventAssignmentDelegate",	    	"_eventAssignments","");
    mHeader.FormatVariable("double",						    	"_eventPriorities");
    mHeader.FormatVariable("//TComputeEventAssignmentDelegate",	"_computeEventAssignments");
    mHeader.FormatVariable("//TPerformEventAssignmentDelegate",	"_performEventAssignments");
    mHeader.FormatArray("bool",					            	"_eventStatusArray", 			mNumEvents);
    mHeader.FormatArray("bool",					            	"_previousEventStatusArray", 	mNumEvents);
}


void CGenerator::WriteComputeAllRatesOfChange(StringBuilder& ignore, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    mSource<<Append("\t// Uses the equation: dSd/dt = L0 dSi/dt" + NL());
    mSource<<Append("\t void computeAllRatesOfChange ()" + NL());
    mSource<<Append("\t{" + NL());
    mSource<<Append("\t\tdouble[] dTemp = new double[amounts.Length + rateRules.Length];" + NL());
    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        mSource<<Format("\t\tdTemp[{0}] = {1};{2}", i, mMapRateRule[i], NL());
    }
    //mSource<<Append("\t\trateRules.CopyTo(dTemp, 0);" + NL());
    mSource<<Append("\t\tamounts.CopyTo(dTemp, rateRules.Length);" + NL());
    mSource<<Append("\t\tevalModel (time, dTemp);" + NL());
    bool isThereAnEntry = false;
    for (int i = 0; i < numDependentSpecies; i++)
    {
        mSource<<Format("\t\t_dydt[{0}] = ", (numIndependentSpecies + i));
        isThereAnEntry = false;
        for (int j = 0; j < numIndependentSpecies; j++)
        {
            string dyName = Format("_dydt[{0}]", j);

            if (L0(i,j) > 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == 1)
                {
                    mSource<<Format(" + {0}{1}", dyName, NL());
                }
                else
                {
                    mSource<<Format(" + (double){0}{1}{2}{3}", WriteDouble(L0(i,j)), STR_FixAmountCompartments, dyName, NL());
                }
            }
            else if (L0(i,j) < 0)
            {
                isThereAnEntry = true;
                if (L0(i,j) == -1)
                {
                    mSource<<Format(" - {0}{1}", dyName, NL());
                }
                else
                {
                    mSource<<Format(" - (double){0}{1}{2}{3}", WriteDouble(fabs(L0(i,j))), STR_FixAmountCompartments, dyName, NL());
                }
            }
        }
        if (!isThereAnEntry)
        {
            mSource<<Append("0");
        }
        mSource<<Format(";{0}", NL());
    }

    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteComputeConservedTotals(StringBuilder& ignore, const int& numFloatingSpecies, const int& numDependentSpecies)
{
    mSource<<Append("\t// Uses the equation: C = Sd - L0*Si" + NL());
    mSource<<Append("\t void computeConservedTotals ()" + NL());
    mSource<<Append("\t{" + NL());
    if (numDependentSpecies > 0)
    {
        string factor;
        double* matPtr = mStructAnalysis.GetGammaMatrix();

        DoubleMatrix gamma(matPtr, numDependentSpecies, numFloatingSpecies);
        for (int i = 0; i < numDependentSpecies; i++)
        {
            mSource<<Format("\t\t_ct[{0}] = ", i);
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
                        mSource<<Append(" + " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                    else
                    {
                        mSource<<Append(" - " + factor + convertSpeciesToY(floatingSpeciesConcentrationList[j].name) +
                                  STR_FixAmountCompartments +
                                  convertCompartmentToC(floatingSpeciesConcentrationList[j].compartmentName) +
                                  NL());
                    }
                }
            }
            mSource<<Append(";" + NL());
            conservationList.Add(Symbol("CSUM" + ToString(i))); //TODO: how to deal with this?
        }
    }
    mSource<<Append("	}" + NL() + NL());
}

void CGenerator::WriteUpdateDependentSpecies(StringBuilder& ignore, const int& numIndependentSpecies, const int& numDependentSpecies, DoubleMatrix& L0)
{
    mSource<<Append("\t// Compute values of dependent species " + NL());
    mSource<<Append("\t// Uses the equation: Sd = C + L0*Si" + NL());
    mSource<<Append("\t void updateDependentSpeciesValues (double[] y)" + NL());
    mSource<<Append("\t{" + NL());

    if (numDependentSpecies > 0)
    {
        // Use the equation: Sd = C + L0*Si to compute dependent concentrations
        if (numDependentSpecies > 0)
        {
            for (int i = 0; i < numDependentSpecies; i++)
            {
                mSource<<Format("\t\t_y[{0}] = {1}\t", (i + numIndependentSpecies), NL());
                mSource<<Format("(_ct[{0}]", i);
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
                            mSource<<Format(" + {0}\t{1}{2}{3}{0}\t",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            mSource<<Format("{0}\t + (double){1}{2}{3}{2}{4}",
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
                            mSource<<Format("{0}\t - {1}{2}{3}",
                                NL(),
                                yName,
                                STR_FixAmountCompartments,
                                cName);
                        }
                        else
                        {
                            mSource<<Format("{0}\t - (double){1}{2}{3}{2}{4}",
                                NL(),
                                WriteDouble(fabs(L0(i,j))),
                                STR_FixAmountCompartments,
                                yName,
                                cName);
                        }
                    }
                }
                mSource<<Format(")/{0};{1}", cLeftName, NL());
            }
        }
    }
    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteUserDefinedFunctions(StringBuilder& ignore)
{
	for (int i = 0; i < mNOM.getNumFunctionDefinitions(); i++)
    {
    	try
        {
        	StringListContainer oList = mNOM.getNthFunctionDefinition(i);
            StringList aList = oList[0];

          	string sName = aList[0];
          	//sName.Trim();
            mfunctionNames.Add(sName);
            StringList oArguments = oList[1];
            StringList list2 = oList[2];
            string sBody = list2[0];

            mSource<<Format("\t// User defined function:  {0}{1}", sName, NL());
            mSource<<Format("\t double {0} (", sName);

            for (int j = 0; j < oArguments.size(); j++)
            {
                mSource<<Append("double " + (string)oArguments[j]);
                mfunctionParameters.Add((string)oArguments[j]);
                if (j < oArguments.size() - 1)
                    mSource<<Append(", ");
            }
            mSource<<Append(")" + NL() + "\t{" + NL() + "\t\t return " +
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

void CGenerator::WriteResetEvents(StringBuilder& ignore, const int& numEvents)
{
      mSource<<Format("{0}\t void resetEvents() {{0}", NL());
      for (int i = 0; i < numEvents; i++)
      {
          mSource<<Format("\t\t_eventStatusArray[{0}] = false;{1}", i, NL());
          mSource<<Format("\t\t_previousEventStatusArray[{0}] = false;{1}", i, NL());
      }
      mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteSetConcentration(StringBuilder& ignore)
{
    mSource<<Format("\t void setConcentration(int index, double value) {{0}", NL());
    mSource<<Format("\t\tdouble volume = 0.0;{0}", NL());
    mSource<<Format("\t\t_y[index] = value;{0}", NL());
    mSource<<Format("\t\tswitch (index) {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
    	mSource<<Format("\t\t\tcase {0}: volume = {1};{2}",
          i,
          convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
          NL());
      mSource<<Format("\t\t\t\tbreak;{0}", NL());
    }
    mSource<<Format("\t\t}{0}", NL());
    mSource<<Format("\t\t_amounts[index] = _y[index]*volume;{0}", NL());
    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteGetConcentration(StringBuilder& ignore)
{
    mSource<<Format("\t double getConcentration(int index) {{0}", NL());
    mSource<<Format("\t\treturn _y[index];{0}", NL());
    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteConvertToAmounts(StringBuilder& ignore)
{
    mSource<<Format("\t void convertToAmounts() {{0}", NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        mSource<<Format("\t\t_amounts[{0}] = _y[{0}]*{1};{2}",
            i,
            convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName),
            NL());
    }
    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteConvertToConcentrations(StringBuilder& ignore)
{
    mSource<<Append("\t void convertToConcentrations() {" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        mSource<<"\t\t_y[" << i << "] = _amounts[" << i << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }
    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteProperties(StringBuilder& ignore)
{
//    mSource<<Append("\t double[] y {" + NL());
//    mSource<<Append("\t\tget { return _y; } " + NL());
//    mSource<<Append("\t\tset { _y = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] init_y {" + NL());
//    mSource<<Append("\t\tget { return _init_y; } " + NL());
//    mSource<<Append("\t\tset { _init_y = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] amounts {" + NL());
//    mSource<<Append("\t\tget { return _amounts; } " + NL());
//    mSource<<Append("\t\tset { _amounts = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] bc {" + NL());
//    mSource<<Append("\t\tget { return _bc; } " + NL());
//    mSource<<Append("\t\tset { _bc = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] gp {" + NL());
//    mSource<<Append("\t\tget { return _gp; } " + NL());
//    mSource<<Append("\t\tset { _gp = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] sr {" + NL());
//    mSource<<Append("\t\tget { return _sr; } " + NL());
//    mSource<<Append("\t\tset { _sr = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[][] lp {" + NL());
//    mSource<<Append("\t\tget { return _lp; } " + NL());
//    mSource<<Append("\t\tset { _lp = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] c {" + NL());
//    mSource<<Append("\t\tget { return _c; } " + NL());
//    mSource<<Append("\t\tset { _c = value; } " + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] dydt {" + NL());
//    mSource<<Append("\t\tget { return _dydt; }" + NL());
//    mSource<<Append("\t\tset { _dydt = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] rateRules {" + NL());
//    mSource<<Append("\t\tget { return _rateRules; }" + NL());
//    mSource<<Append("\t\tset { _rateRules = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] rates {" + NL());
//    mSource<<Append("\t\tget { return _rates; }" + NL());
//    mSource<<Append("\t\tset { _rates = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] ct {" + NL());
//    mSource<<Append("\t\tget { return _ct; }" + NL());
//    mSource<<Append("\t\tset { _ct = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] eventTests {" + NL());
//    mSource<<Append("\t\tget { return _eventTests; }" + NL());
//    mSource<<Append("\t\tset { _eventTests = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t TEventDelayDelegate[] eventDelay {" + NL());
//    mSource<<Append("\t\tget { return _eventDelay; }" + NL());
//    mSource<<Append("\t\tset { _eventDelay = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t bool[] eventType {" + NL());
//    mSource<<Append("\t\tget { return _eventType; }" + NL());
//    mSource<<Append("\t\tset { _eventType = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t bool[] eventPersistentType {" + NL());
//    mSource<<Append("\t\tget { return _eventPersistentType; }" + NL());
//    mSource<<Append("\t\tset { _eventPersistentType = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t bool[] eventStatusArray {" + NL());
//    mSource<<Append("\t\tget { return _eventStatusArray; }" + NL());
//    mSource<<Append("\t\tset { _eventStatusArray = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t bool[] previousEventStatusArray {" + NL());
//    mSource<<Append("\t\tget { return _previousEventStatusArray; }" + NL());
//    mSource<<Append("\t\tset { _previousEventStatusArray = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double[] eventPriorities {" + NL());
//    mSource<<Append("\t\tget { return _eventPriorities; }" + NL());
//    mSource<<Append("\t\tset { _eventPriorities = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t TEventAssignmentDelegate[] eventAssignments {" + NL());
//    mSource<<Append("\t\tget { return _eventAssignments; }" + NL());
//    mSource<<Append("\t\tset { _eventAssignments = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t TComputeEventAssignmentDelegate[] computeEventAssignments {" + NL());
//    mSource<<Append("\t\tget { return _computeEventAssignments; }" + NL());
//    mSource<<Append("\t\tset { _computeEventAssignments = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t TPerformEventAssignmentDelegate[] performEventAssignments {" + NL());
//    mSource<<Append("\t\tget { return _performEventAssignments; }" + NL());
//    mSource<<Append("\t\tset { _performEventAssignments = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
//
//    mSource<<Append("\t double time {" + NL());
//    mSource<<Append("\t\tget { return _time; }" + NL());
//    mSource<<Append("\t\tset { _time = value; }" + NL());
//    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteAccessors(StringBuilder& mSource)
{
    mSource<<Append("\t int getNumIndependentVariables {" + NL());
    mSource<<Append("\t\tget { return numIndependentVariables; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumDependentVariables {" + NL());
    mSource<<Append("\t\tget { return numDependentVariables; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumTotalVariables {" + NL());
    mSource<<Append("\t\tget { return numTotalVariables; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumBoundarySpecies {" + NL());
    mSource<<Append("\t\tget { return numBoundaryVariables; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumGlobalParameters {" + NL());
    mSource<<Append("\t\tget { return numGlobalParameters; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumLocalParameters(int reactionId)" + NL());
    mSource<<Append("\t{" + NL());
    mSource<<Append("\t\treturn localParameterDimensions[reactionId];" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumCompartments {" + NL());
    mSource<<Append("\t\tget { return numCompartments; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumReactions {" + NL());
    mSource<<Append("\t\tget { return numReactions; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumEvents {" + NL());
    mSource<<Append("\t\tget { return numEvents; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t int getNumRules {" + NL());
    mSource<<Append("\t\tget { return numRules; }" + NL());
    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t List<string> Warnings {" + NL());
    mSource<<Append("\t\tget { return _Warnings; }" + NL());
    mSource<<Append("\t\tset { _Warnings = value; }" + NL());
    mSource<<Append("\t}" + NL() + NL());
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

void CGenerator::WriteTestConstraints(StringBuilder& mSource)
{
    mSource<<Append("\t void testConstraints()" + NL());
    mSource<<Append("\t{" + NL());

    for (int i = 0; i < mNOM.getNumConstraints(); i++)
    {
        string sMessage;
        string sCheck = mNOM.getNthConstraint(i, sMessage);

        mSource<<Append("\t\tif (" + substituteTerms(mNOM.getNumReactions(), "", sCheck) + " == 0.0 )" + NL());
        mSource<<Append("\t\t\tthrow new Exception(\"" + sMessage + "\");" + NL());
    }

    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteEvalInitialAssignments(StringBuilder& mSource, const int& numReactions)
{
    mSource<<Append("\t void evalInitialAssignments()" + NL());
    mSource<<Append("\t{" + NL());

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
                mSource<<Append(leftSideRule + " = ");
                mSource<<Append(substituteTerms(numReactions, "", rightSideRule) + ";" + NL());
            }
        }
    }
    for (int i = 0; i < mNOM.GetModel()->getNumEvents(); i++)
    {
        Event *current = mNOM.GetModel()->getEvent(i);
        string initialTriggerValue = ToString(current->getTrigger()->getInitialValue());//.ToString().ToLowerInvariant();
        mSource<<Append("\t\t_eventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
        mSource<<Append("\t\t_previousEventStatusArray[" + ToString(i) + "] = " + initialTriggerValue + ";" + NL());
    }
    mSource<<Append("\t}" + NL() + NL());
}

int CGenerator::WriteComputeRules(StringBuilder& mSource, const int& numReactions)
{
    IntStringHashTable mapVariables;
    int numRateRules = 0;
    int numOfRules = mNOM.getNumRules();

    mSource<<Append("\t void computeRules(double[] y) {" + NL());
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
                        mMapRateRule[numRateRules] = FindSymbol(varName);
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
                mSource<<Append(leftSideRule + " = ");
                int speciesIndex;
                bool isSpecies = floatingSpeciesConcentrationList.find(varName, speciesIndex);

                Symbol* symbol = (speciesIndex != -1) ? &(floatingSpeciesConcentrationList[speciesIndex]) : NULL;
                string sCompartment;

                if(isRateRule && mNOM.MultiplyCompartment(varName, sCompartment) && (rightSide.find(sCompartment) == string::npos))
                {
                    mSource<<Format("({0}) * {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(sCompartment), NL());
                }
                else
                {
                    if (isSpecies && !isRateRule && symbol != NULL && symbol->hasOnlySubstance && symbol->compartmentName.size() != 0)
                    {
                        mSource<<Format("({0}) / {1};{2}", substituteTerms(numReactions, "", rightSideRule), FindSymbol(symbol->compartmentName), NL());
                    }
                    else
                    {
                        mSource<<Format("{0};{1}", substituteTerms(numReactions, "", rightSideRule), NL());
                    }
                }

                if (mNOM.IsCompartment(varName))
                {
                    mSource<<Append("\t\tconvertToConcentrations();");
                }
            }
        }
        catch (const Exception& ex)
        {
            throw new SBWApplicationException("Error while trying to get Rule #" + ToString(i) + ex.Message);
        }
    }

    mSource<<Append("\t}" + NL() + NL());
    mSource<<Append("\t double[] _rateRules = new double[" + ToString(numRateRules) +
              "];           // Vector containing values of additional rate rules      " + NL());

    mSource<<Append("\t void InitializeRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < numRateRules; i++)
    {
        mSource<<"\t\t_rateRules[" << i << "] = " << mMapRateRule[i] << ";" << NL();
    }

    mSource<<Append("\t}" + NL() + NL());
    mSource<<Append("\t void AssignRates()" + NL() + "\t{" + NL());

    for (int i = 0; i < mMapRateRule.size(); i++)
    {
        mSource<<(string)mMapRateRule[i] << " = _rateRules[" << i << "];" << NL();
    }

    mSource<<Append("\t}" + NL() + NL());

    mSource<<Append("\t void InitializeRateRuleSymbols()" + NL() + "\t{" + NL());
    for (int i = 0; i < mMapRateRule.size(); i++)
    {
        string varName = (string)mapVariables[i];
        double value = mNOM.getValue(varName);
        if (!IsNaN(value))
        {
            mSource<< mMapRateRule[i] << " = " << ToString(value, STR_DoubleFormat) << ";" << NL();
        }
    }

    mSource<<Append("\t}" + NL() + NL());
    mSource<<Append("\t void AssignRates(double[] oRates)" + NL() + "\t{" + NL());

    for (int i = 0; i < mMapRateRule.size(); i++)
    {
        mSource<< mMapRateRule[i] << " = oRates[" << i << "];" << NL();
    }

    mSource<<Append("\t}" + NL() + NL());
    mSource<<Append("\t double[] GetCurrentValues()" + NL() + "\t{" + NL());
    mSource<<Append("\t\tdouble[] dResult = new double[" + ToString(NumAdditionalRates()) + "];" + NL());

    for (int i = 0; i < mMapRateRule.size(); i++)
    {
        mSource<<"\t\tdResult[" << i << "] = " << mMapRateRule[i] << ";" << NL();
    }
    mSource<<Append("\t\treturn dResult;" + NL());

    mSource<<Append("\t}" + NL() + NL());
    return numOfRules;
}

void CGenerator::WriteComputeReactionRates(StringBuilder& mSource, const int& numReactions)
{
    mSource<<Append("\t// Compute the reaction rates" + NL());
    mSource<<Append("\t void computeReactionRates (double time, double[] y)" + NL());
    mSource<<Append("\t{" + NL());


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
        mSource<<Format("\t\t_rates[{0}] = {1}{2}", i, modKineticLaw, NL());
    }
    mSource<<Format("\t}{0}{0}", NL());
}

void CGenerator::WriteEvalEvents(StringBuilder& mSource, const int& numEvents, const int& numFloatingSpecies)
{
    mSource<<Append("\t// Event handling function" + NL());
    mSource<<Append("\t void evalEvents (double timeIn, double[] oAmounts)" + NL());
    mSource<<Append("\t{" + NL());

    if (numEvents > 0)
    {
        for (int i = 0; i < NumAdditionalRates(); i++)
        {
            mSource<<(string) mMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
        }
        for (int i = 0; i < numFloatingSpecies; i++)
        {
            mSource<<"\t\t_y[" << i << "] = oAmounts[" << (i + NumAdditionalRates()) << "]/" <<
                      convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
        }
    }

    mSource<<Append("\t\t_time = timeIn;  // Don't remove" + NL());
    mSource<<Append("\t\tupdateDependentSpeciesValues(_y);" + NL());
    mSource<<Append("\t\tcomputeRules (_y);" + NL());

    for (int i = 0; i < numEvents; i++)
    {
        ArrayList ev = mNOM.getNthEvent(i);
        StringList tempList = ev[0];
        string eventString = tempList[0];

        eventString = substituteTerms(0, "", eventString);
        mSource<<"\t\tpreviousEventStatusArray[" << i << "] = eventStatusArray[" << i << "];" << NL();
        mSource<<Append("\t\tif (" + eventString + " == 1.0) {" + NL());
        mSource<<Append("\t\t     eventStatusArray[" + ToString(i) + "] = true;" + NL());
        mSource<<Append("\t\t     eventTests[" + ToString(i) + "] = 1;" + NL());
        mSource<<Append("\t\t} else {" + NL());
        mSource<<Append("\t\t     eventStatusArray[" + ToString(i) + "] = false;" + NL());
        mSource<<Append("\t\t     eventTests[" + ToString(i) + "] = -1;" + NL());
        mSource<<Append("\t\t}" + NL());
    }
    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteEvalModel(StringBuilder& mSource, const int& numReactions, const int& numIndependentSpecies, const int& numFloatingSpecies, const int& numOfRules)
{
    mSource<<Append("\t// Model Function" + NL());
    mSource<<Append("\t void evalModel (double timein, double[] oAmounts)" + NL());
    mSource<<Append("\t{" + NL());

    for (int i = 0; i < NumAdditionalRates(); i++)
    {
        mSource<<(string)mMapRateRule[i] << " = oAmounts[" << i << "];" << NL();
    }

    for (int i = 0; i < numFloatingSpecies; i++)
    {
    	mSource<<"\t\t_y[" << i << "] = oAmounts[" << i + NumAdditionalRates() << "]/" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    mSource<<Append(NL());
    mSource<<Append("\t\tconvertToAmounts();" + NL());
    mSource<<Append("\t\t_time = timein;  // Don't remove" + NL());
    mSource<<Append("\t\tupdateDependentSpeciesValues (_y);" + NL());

    if (numOfRules > 0)
    {
        mSource<<Append("\t\tcomputeRules (_y);" + NL());
    }

    mSource<<Append("\t\tcomputeReactionRates (time, _y);" + NL());

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
                    eqnBuilder<<Format(" + {0}_rates[{1}]", stoich, j);
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

                    eqnBuilder<<Append(Format(" - {0}_rates[{1}]", stoich, j));
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
            mSource<<"\t\t_dydt[" << i << "] =" << final << ";" << NL();
        }
    }

    mSource<<Append("\t\tconvertToAmounts ();" + NL());
    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteEventAssignments(StringBuilder& ignore, const int& numReactions, const int& numEvents)
{
	StringList delays;
    vector<bool> eventType;
    vector<bool> eventPersistentType;
    if (numEvents > 0)
    {
        mSource<<Append("\t// Event assignments" + NL());
        for (int i = 0; i < numEvents; i++)
        {
            ArrayList ev = mNOM.getNthEvent(i);
            eventType.push_back(mNOM.getNthUseValuesFromTriggerTime(i));
            eventPersistentType.push_back(mNOM.GetModel()->getEvent(i)->getTrigger()->getPersistent());

            StringList event = ev[1];
            int numItems = event.size();
            string str = substituteTerms(numReactions, "", event[0]);
            delays.Add(str);

            mSource<<Format("\t void eventAssignment_{0} () {{1}", i, NL());
            mSource<<Format("\t\tperformEventAssignment_{0}( computeEventAssignment_{0}() );{1}", i, NL());
            mSource<<Append("\t}" + NL());
            mSource<<Format("\t double[] computeEventAssignment_{0} () {{1}", i, NL());
            StringList oTemp;
            StringList oValue;
            int nCount = 0;
            int numAssignments = ev.size() - 2;
            mSource<<Format("\t\tdouble[] values = new double[ {0}];{1}", numAssignments, NL());
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
                mSource<<Format("\t\t{0};{1}", str, NL());
            }
            mSource<<Append("\t\treturn values;" + NL());
            mSource<<Append("\t}" + NL());
            mSource<<Format("\t void performEventAssignment_{0} (double[] values) {{1}", i, NL());

            for (int j = 0; j < oTemp.size(); j++)
            {
                mSource<<Format("\t\t{0} = values[{1}];{2}", oTemp[j], j, NL());
                string aStr = (string) oTemp[j];
                aStr = Trim(aStr);

                if (StartsWith(aStr, "_c[")) //Todo:May have to trim?
                {
                    mSource<<Append("\t\tconvertToConcentrations();" + NL());
                }
            }

            mSource<<Append("\t}" + NL());
        }
        mSource<<Append("\t" + NL());
    }


    mSource<<"void InitializeDelays()\n{";
	mSource<<tab<<"\n\tprintf(\"In File %s \",__FILE__);"<<endl;
	mSource<<tab<<"printf(\"In Function %s \",__FUNCTION__);"<<endl;
	mSource<<tab<<"printf(\"At Line %d \",__LINE__);"<<endl;

    for (int i = 0; i < delays.size(); i++)
    {
        mSource<<Format("\t\t_eventDelay[{0}] = new TEventDelayDelegate(delegate { return {1}; } );{2}", i, delays[i], NL());
        mSource<<Format("\t\t_eventType[{0}] = {1};{2}", i, ToString((eventType[i] ? true : false)), NL());
        mSource<<Format("\t\t_eventPersistentType[{0}] = {1};{2}", i, (eventPersistentType[i] ? "true" : "false"), NL());
    }
    mSource<<"}\n\n";

    mSource<<"void computeEventPriorites()\n{"<<endl;
    for (int i = 0; i < numEvents; i++)
    {
        Event* current = mNOM.GetModel()->getEvent(i);

        if (current->isSetPriority() && current->getPriority()->isSetMath())
        {
            string priority = SBML_formulaToString(current->getPriority()->getMath());
            mSource<<Format("\t_eventPriorities[{0}] = {1};{2}", i, substituteTerms(numReactions, "", priority), NL());
        }
        else
        {
            mSource<<Format("\t_eventPriorities[{0}] = 0f;{1}", i, NL());
        }
    }
    mSource<<Format("}{0}{0}", NL());
}

void CGenerator::WriteSetParameterValues(StringBuilder& mSource, const int& numReactions)
{
    mSource<<Append("\t void setParameterValues ()" + NL());
    mSource<<Append("\t{" + NL());

    for (int i = 0; i < globalParameterList.size(); i++)
    {
        mSource<<Format("\t\t{0} = (double){1};{2}",
                      convertSymbolToGP(globalParameterList[i].name),
                      WriteDouble(globalParameterList[i].value),
                      NL());
    }

    // Initialize local parameter values
    for (int i = 0; i < numReactions; i++)
    {
        for (int j = 0; j < localParameterList[i].size(); j++)
            mSource<<Format("\t\t_lp[{0}][{1}] = (double){2};{3}",
                          i,
                          j,
                          WriteDouble(localParameterList[i][j].value),
                          NL());
    }

    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteSetCompartmentVolumes(StringBuilder& mSource)
{
    mSource<<Append("\t void setCompartmentVolumes ()" + NL());
    mSource<<Append("\t{" + NL());
    for (int i = 0; i < compartmentList.size(); i++)
    {
        mSource<<Append("\t\t" + convertSymbolToC(compartmentList[i].name) + " = (double)" +
                  WriteDouble(compartmentList[i].value) + ";" + NL());

        // at this point we also have to take care of all initial assignments for compartments as well as
        // the assignment rules on compartments ... otherwise we are in trouble :)
		stack<string> initializations = mNOM.GetMatchForSymbol(compartmentList[i].name);
        while (initializations.size() > 0)
        {
        	string term(initializations.top());
            string sub = substituteTerms(mNumReactions, "", term);
            mSource<<Append("\t\t" + sub + ";" + NL());
            initializations.pop();
        }
    }

    mSource<<Append("\t}" + NL() + NL());
}

void CGenerator::WriteSetBoundaryConditions(StringBuilder& mSource)
{
    mSource<<Append("\t void setBoundaryConditions ()" + NL());
    mSource<<Append("\t{" + NL());
    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        if (IsNullOrEmpty(boundarySpeciesList[i].formula))
        {
            mSource<<Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      WriteDouble(boundarySpeciesList[i].value) + ";" + NL());
        }
        else
        {
            mSource<<Append("\t\t" + convertSpeciesToBc(boundarySpeciesList[i].name) + " = (double)" +
                      boundarySpeciesList[i].formula + ";" + NL());
        }
    }
    mSource<<Append("\t}" + NL() + NL());
}


void CGenerator::WriteSetInitialConditions(StringBuilder& mSource, const int& numFloatingSpecies)
{
    mSource<<Append("\t void initializeInitialConditions ()" + NL());
    mSource<<Append("\t{" + NL());
    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        if (IsNullOrEmpty(floatingSpeciesConcentrationList[i].formula))
        {
            mSource<<Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      WriteDouble(floatingSpeciesConcentrationList[i].value) + ";" + NL());
        }
        else
        {
            mSource<<Append("\t\t_init" + convertSpeciesToY(floatingSpeciesConcentrationList[i].name) + " = (double)" +
                      floatingSpeciesConcentrationList[i].formula + ";" + NL());
        }
    }
    mSource<<Append(NL());

    mSource<<Append("\t}" + NL() + NL());

    // ------------------------------------------------------------------------------
    mSource<<Append("\t void setInitialConditions ()" + NL());
    mSource<<Append("\t{" + NL());

    for (int i = 0; i < numFloatingSpecies; i++)
    {
        mSource<<"\t\t_y[" << i << "] =  _init_y[" << i << "];" << NL();
        mSource<<"\t\t_amounts[" << i << "] = _y[" << i << "]*" <<
                  convertCompartmentToC(floatingSpeciesConcentrationList[i].compartmentName) << ";" << NL();
    }

    mSource<<Append(NL());
	mSource<<Append("\t}" + NL() + NL());
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
    StringBuilder  mSource;

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
                    	mSource<<Append("Math.Pow");
                    }
					else if(theToken == "sqrt")
                    {
                        mSource<<Append("Math.Sqrt");
                  	}
                    else if(theToken == "log")
                    {
                    	mSource<<Append("supportFunctions._log");
                    }
                    else if(theToken == "log10")
                    {
                        mSource<<Append("Math.Log10");
                    }
                    else if(theToken == "floor")
                    {
                        mSource<<Append("Math.Floor");
                    }
                    else if(theToken == "ceil")
                    {
                    	mSource<<Append("Math.Ceiling");
                    }
                    else if(theToken == "factorial")
                    {
                    	mSource<<Append("supportFunctions._factorial");
                    }
                    else if(theToken == "exp")
                    {
                    	mSource<<Append("Math.Exp");
                    }
                    else if(theToken == "sin")
                    {
                    	mSource<<Append("Math.Sin");
                    }
                    else if(theToken == "cos")
                    {
                        mSource<<Append("Math.Cos");
                    }
                    else if(theToken == "tan")
                    {
                        mSource<<Append("Math.Tan");
                    }
                    else if(theToken == "abs")
                    {
                        mSource<<Append("Math.Abs");
                    }
                    else if(theToken == "asin")
                    {
                        mSource<<Append("Math.Asin");
                    }
                    else if(theToken == "acos")
                    {
                        mSource<<Append("Math.Acos");
                    }
                    else if(theToken == "atan")
                    {
                    	mSource<<Append("Math.Atan");
                    }
                    else if(theToken == "sec")
                    {
                        mSource<<Append("MathKGI.Sec");
                    }
                    else if(theToken == "csc")
                    {
                        mSource<<Append("MathKGI.Csc");
                    }
                    else if(theToken == "cot")
                    {
                        mSource<<Append("MathKGI.Cot");
                    }
                    else if(theToken == "arcsec")
                    {
                        mSource<<Append("MathKGI.Asec");
                    }
                    else if(theToken == "arccsc")
                    {
                        mSource<<Append("MathKGI.Acsc");
                    }
                    else if(theToken == "arccot")
                    {
                        mSource<<Append("MathKGI.Acot");
                    }
                    else if(theToken == "sinh")
                    {
                        mSource<<Append("Math.Sinh");
                    }
                    else if(theToken == "cosh")
                    {
                        mSource<<Append("Math.Cosh");
                    }
                    else if(theToken == "tanh")
                    {
                        mSource<<Append("Math.Tanh");
                    }
                    else if(theToken == "arcsinh")
                    {
                        mSource<<Append("MathKGI.Asinh");
                    }
                    else if(theToken == "arccosh")
                    {
                        mSource<<Append("MathKGI.Acosh");
                    }
                    else if(theToken == "arctanh")
                    {
                        mSource<<Append("MathKGI.Atanh");
                    }
                    else if(theToken == "sech")
                    {
                        mSource<<Append("MathKGI.Sech");
                    }
                    else if(theToken == "csch")
                    {
                        mSource<<Append("MathKGI.Csch");
                    }
                    else if(theToken == "coth")
                    {
                        mSource<<Append("MathKGI.Coth");
                    }
                    else if(theToken == "arcsech")
                    {
                        mSource<<Append("MathKGI.Asech");
                    }
                    else if(theToken == "arccsch")
                    {
                        mSource<<Append("MathKGI.Acsch");
                    }
                    else if(theToken == "arccoth")
                    {
                               mSource<<Append("MathKGI.Acoth");
                    }
                    else if(theToken == "pi")
                    {
                        mSource<<Append("Math.PI");
                    }
                    else if(theToken == "exponentiale")
                    {
                        mSource<<Append("Math.E");
                    }
                    else if(theToken == "avogadro")
                    {
                        mSource<<Append("6.02214179e23");
                    }
                    else if(theToken == "true")
                    {
                               //mSource<<Append("true");
                        mSource<<Append("1.0");
                    }
                    else if(theToken == "false")
                    {
                               //mSource<<Append("false");
                        mSource<<Append("0.0");
                    }
                    else if(theToken == "gt")
                    {
                        mSource<<Append("supportFunctions._gt");
                    }
                    else if(theToken == "lt")
                    {
                        mSource<<Append("supportFunctions._lt");
                    }
                    else if(theToken == "eq")
                    {
                        mSource<<Append("supportFunctions._eq");
                    }
                    else if(theToken == "neq")
                    {
                        mSource<<Append("supportFunctions._neq");
                    }
                    else if(theToken == "geq")
                    {
                        mSource<<Append("supportFunctions._geq");
                    }
                    else if(theToken == "leq")
                    {
                        mSource<<Append("supportFunctions._leq");
                    }
                    else if(theToken == "and")
                    {
                        mSource<<Append("supportFunction._and");
                    }
                    else if(theToken == "or")
                    {
                        mSource<<Append("supportFunction._or");
                    }
                    else if(theToken == "not")
                    {
                        mSource<<Append("supportFunction._not");
                    }
                    else if(theToken == "xor")
                    {
                        mSource<<Append("supportFunction._xor");
                    }
                    else if(theToken == "root")
                    {
                        mSource<<Append("supportFunctions._root");
                    }
                    else if(theToken == "piecewise")
                    {
                        mSource<<Append("supportFunctions._piecewise");
                    }
                    else if (!mfunctionParameters.Contains(s.tokenString))
                    {
                    	throw Exception("Token '" + s.tokenString + "' not recognized.");
                    }
                    else
                    {
                    	mSource<<Append(s.tokenString);
                	}

				break; //Word token

               	case CodeTypes::tDoubleToken:
                   	mSource<<Append(WriteDouble(s.tokenDouble));
                   	break;
               	case CodeTypes::tIntToken:
                	mSource<<Append((int) s.tokenInteger);
                   	break;
               	case CodeTypes::tPlusToken:
                   mSource<<Append("+");
                   break;
               	case CodeTypes::tMinusToken:
                   mSource<<Append("-");
                   break;
               	case CodeTypes::tDivToken:
                   mSource<<Append("/");
                   break;
               	case CodeTypes::tMultToken:
                   mSource<<Append(STR_FixAmountCompartments);
                   break;
               	case CodeTypes::tPowerToken:
                   mSource<<Append("^");
                   break;
               	case CodeTypes::tLParenToken:
                   mSource<<Append("(");
                   break;
               	case CodeTypes::tRParenToken:
                   mSource<<Append(")");
                   break;
               	case CodeTypes::tCommaToken:
                   mSource<<Append(",");
                   break;
               	case CodeTypes::tEqualsToken:
                   mSource<<Append(" = ");
                   break;
               	case CodeTypes::tTimeWord1:
                   mSource<<Append("time");
                   break;
               	case CodeTypes::tTimeWord2:
                   mSource<<Append("time");
                   break;
               	case CodeTypes::tTimeWord3:
                   mSource<<Append("time");
                   break;
               	case CodeTypes::tAndToken:
                   mSource<<Append("supportFunctions._and");
                   break;
               	case CodeTypes::tOrToken:
                   mSource<<Append("supportFunctions._or");
                   break;
               	case CodeTypes::tNotToken:
                   mSource<<Append("supportFunctions._not");
                   break;
               	case CodeTypes::tLessThanToken:
                   mSource<<Append("supportFunctions._lt");
                   break;
               	case CodeTypes::tLessThanOrEqualToken:
                   mSource<<Append("supportFunctions._leq");
                   break;
               	case CodeTypes::tMoreThanOrEqualToken:
                   mSource<<Append("supportFunctions._geq");
                   break;
               	case CodeTypes::tMoreThanToken:
                   mSource<<Append("supportFunctions._gt");
                   break;
               	case CodeTypes::tXorToken:
                   mSource<<Append("supportFunctions._xor");
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
	return mSource.ToString();
}

void CGenerator::SubstituteEquation(const string& reactionName, Scanner& s, StringBuilder& mSource)
{
	string theToken(s.tokenString);
    if(theToken == "pow")
    {
        mSource<<Append("Math.Pow");
    }
    else if(theToken == "sqrt")
    {
        mSource<<Append("Math.Sqrt");
    }
    else if(theToken == "log")
    {
        mSource<<Append("supportFunctions._log");
    }
    else if(theToken == "floor")
    {
        mSource<<Append("Math.Floor");
    }
    else if(theToken == "ceil")
    {
        mSource<<Append("Math.Ceiling");
    }
    else if(theToken == "factorial")
    {
        mSource<<Append("supportFunctions._factorial");
    }
    else if(theToken == "log10")
    {
        mSource<<Append("Math.Log10");
    }
    else if(theToken == "exp")
    {
        mSource<<Append("Math.Exp");
    }
    else if(theToken == "abs")
    {
        mSource<<Append("Math.Abs");
    }
    else if(theToken == "sin")
    {
        mSource<<Append("Math.Sin");
    }
    else if(theToken == "cos")
    {
        mSource<<Append("Math.Cos");
    }
    else if(theToken == "tan")
    {
        mSource<<Append("Math.Tan");
    }
    else if(theToken == "asin")
    {
        mSource<<Append("Math.Asin");
    }
    else if(theToken == "acos")
    {
        mSource<<Append("Math.Acos");
    }
    else if(theToken == "atan")
    {
        mSource<<Append("Math.Atan");
    }
    else if(theToken == "sec")
    {
        mSource<<Append("MathKGI.Sec");
    }
    else if(theToken == "csc")
    {
        mSource<<Append("MathKGI.Csc");
    }
    else if(theToken == "cot")
    {
        mSource<<Append("MathKGI.Cot");
    }
    else if(theToken == "arcsec")
    {
        mSource<<Append("MathKGI.Asec");
    }
    else if(theToken == "arccsc")
    {
        mSource<<Append("MathKGI.Acsc");
    }
    else if(theToken == "arccot")
    {
        mSource<<Append("MathKGI.Acot");
    }
    else if(theToken == "sinh")
    {
        mSource<<Append("Math.Sinh");
    }
    else if(theToken == "cosh")
    {
        mSource<<Append("Math.Cosh");
    }
    else if(theToken == "tanh")
    {
        mSource<<Append("Math.Tanh");
    }
    else if(theToken == "arcsinh")
    {
        mSource<<Append("MathKGI.Asinh");
    }
    else if(theToken == "arccosh")
    {
        mSource<<Append("MathKGI.Acosh");
    }
    else if(theToken == "arctanh")
    {
        mSource<<Append("MathKGI.Atanh");
    }
    else if(theToken == "sech")
    {
        mSource<<Append("MathKGI.Sech");
    }
    else if(theToken == "csch")
    {
        mSource<<Append("MathKGI.Csch");
    }
    else if(theToken == "coth")
    {
        mSource<<Append("MathKGI.Coth");
    }
    else if(theToken == "arcsech")
    {
        mSource<<Append("MathKGI.Asech");
    }
    else if(theToken == "arccsch")
    {
        mSource<<Append("MathKGI.Acsch");
    }
    else if(theToken == "arccoth")
    {
        mSource<<Append("MathKGI.Acoth");
    }
    else if(theToken == "pi")
    {
        mSource<<Append("Math.PI");
    }
    else if(theToken == "avogadro")
    {
        mSource<<Append("6.02214179e23");
    }
    else if(theToken == "exponentiale")
    {
        mSource<<Append("Math.E");
    }
    else if(theToken == "true")
    {
        //mSource<<Append("true");
        mSource<<Append("1.0");
    }
    else if(theToken == "false")
    {
        //mSource<<Append("false");
        mSource<<Append("0.0");
    }
    else if(theToken == "NaN")
    {
        mSource<<Append("double.NaN");
    }
    else if(theToken == "INF")
    {
        mSource<<Append("double.PositiveInfinity");
    }
    else if(theToken == "geq")
    {
        mSource<<Append("supportFunctions._geq");
    }
    else if(theToken == "leq")
    {
        mSource<<Append("supportFunctions._leq");
    }
    else if(theToken == "gt")
    {
        mSource<<Append("supportFunctions._gt");
    }
    else if(theToken == "lt")
    {
        mSource<<Append("supportFunctions._lt");
    }
    else if(theToken == "eq")
    {
        mSource<<Append("supportFunctions._eq");
    }
    else if(theToken == "neq")
    {
        mSource<<Append("supportFunctions._neq");
    }
    else if(theToken == "and")
    {
        mSource<<Append("supportFunction._and");
    }
    else if(theToken == "or")
    {
        mSource<<Append("supportFunction._or");
    }
    else if(theToken == "not")
    {
        mSource<<Append("supportFunction._not");
    }
    else if(theToken == "xor")
    {
        mSource<<Append("supportFunction._xor");
    }
    else if(theToken == "root")
    {
        mSource<<Append("supportFunctions._root");
    }
    else if(theToken == "piecewise")
    {
        mSource<<Append("supportFunctions._piecewise");
    }
    else if(theToken == "delay")
    {
        mSource<<Append("supportFunctions._delay");
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
                mSource<<Append("_lp[" + ToString(index) + "][" + ToString(nParamIndex) + "]");
                bReplaced = true;
            }
        }

        if (boundarySpeciesList.find(s.tokenString, index))
        {
            mSource<<Append("_bc[" + ToString(index) + "]");
            bReplaced = true;
        }
        if (!bReplaced &&
            (mfunctionParameters.size() != 0 && !mfunctionParameters.Contains(s.tokenString)))
        {
            throw Exception("Token '" + s.tokenString + "' not recognized.");
        }
    }
}

void CGenerator::SubstituteWords(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& mSource)
{
    // Global parameters have priority
    int index;
    if (globalParameterList.find(s.tokenString, index))
    {
        mSource<<Format("_gp[{0}]", index);
    }
    else if (boundarySpeciesList.find(s.tokenString, index))
    {
        mSource<<Format("_bc[{0}]", index);

        Symbol symbol = boundarySpeciesList[index];
        if (symbol.hasOnlySubstance)
        {
            // we only store concentration for the boundary so we better
            // fix that.
            int nCompIndex = 0;
            if (compartmentList.find(symbol.compartmentName, nCompIndex))
            {
                mSource<<Format("{0}_c[{1}]", STR_FixAmountCompartments, nCompIndex);
            }
        }
    }
    else if (floatingSpeciesConcentrationList.find(s.tokenString, index))
    {
        Symbol floating1 = floatingSpeciesConcentrationList[index];
        if (floating1.hasOnlySubstance)
        {
            mSource<<Format("amounts[{0}]", index);
        }
        else
        {
            mSource<<Format("_y[{0}]", index);
        }
    }
    else if (compartmentList.find(s.tokenString, index))
    {
        mSource<<Format("_c[{0}]", index);
    }
    else if (mfunctionNames.Contains(s.tokenString))
    {
        mSource<<Format("{0} ", s.tokenString);
    }
    else if (ModifiableSpeciesReferenceList.find(s.tokenString, index))
    {
        mSource<<Format("_sr[{0}]", index);
    }
    else if (reactionList.find(s.tokenString, index))
    {
        mSource<<Format("_rates[{0}]", index);
    }
    else
    {
        SubstituteEquation(reactionName, s, mSource);
	}
}

void CGenerator::SubstituteToken(const string& reactionName, bool bFixAmounts, Scanner& s, StringBuilder& mSource)
{
	string aToken = s.tokenString;
	CodeTypes codeType = s.token();
    switch(codeType)
    {
        case CodeTypes::tWordToken:
        case CodeTypes::tExternalToken:
        case CodeTypes::tExtToken:
            SubstituteWords(reactionName, bFixAmounts, s, mSource);
            break;

        case CodeTypes::tDoubleToken:
            mSource<<Append("(double)" + WriteDouble(s.tokenDouble));
            break;
        case CodeTypes::tIntToken:
            mSource<<Append("(double)" + WriteDouble((double)s.tokenInteger));
            break;
        case CodeTypes::tPlusToken:
            mSource<<Format("+{0}\t", NL());
            break;
        case CodeTypes::tMinusToken:
            mSource<<Format("-{0}\t", NL());
            break;
        case CodeTypes::tDivToken:
            mSource<<Format("/{0}\t", NL());
            break;
        case CodeTypes::tMultToken:
            mSource<<Format("*{0}\t", NL());
            break;
        case CodeTypes::tPowerToken:
            mSource<<Format("^{0}\t", NL());
            break;
        case CodeTypes::tLParenToken:
            mSource<<Append("(");
            break;
        case CodeTypes::tRParenToken:
            mSource<<Format("){0}\t", NL());
            break;
        case CodeTypes::tCommaToken:
            mSource<<Append(",");
            break;
        case CodeTypes::tEqualsToken:
            mSource<<Format(" = {0}\t", NL());
            break;
      case CodeTypes::tTimeWord1:
            mSource<<Append("time");
            break;
        case CodeTypes::tTimeWord2:
            mSource<<Append("time");
            break;
        case CodeTypes::tTimeWord3:
            mSource<<Append("time");
            break;
        case CodeTypes::tAndToken:
            mSource<<Format("{0}supportFunctions._and", NL());
            break;
        case CodeTypes::tOrToken:
            mSource<<Format("{0}supportFunctions._or", NL());
            break;
        case CodeTypes::tNotToken:
            mSource<<Format("{0}supportFunctions._not", NL());
            break;
        case CodeTypes::tLessThanToken:
            mSource<<Format("{0}supportFunctions._lt", NL());
            break;
        case CodeTypes::tLessThanOrEqualToken:
            mSource<<Format("{0}supportFunctions._leq", NL());
            break;
        case CodeTypes::tMoreThanOrEqualToken:
            mSource<<Format("{0}supportFunctions._geq", NL());
            break;
        case CodeTypes::tMoreThanToken:
            mSource<<Format("{0}supportFunctions._gt", NL());
            break;
        case CodeTypes::tXorToken:
            mSource<<Format("{0}supportFunctions._xor", NL());
            break;
        default:
        string aToken = s.tokenToString(s.token());
        Exception ae = Exception(
                 Format("Unknown token in substituteTerms: {0}", aToken,
                 "Exception raised in Module:roadRunner, Method:substituteTerms"));
         throw ae;
    }
}

void CGenerator::WriteOutSymbolTables(StringBuilder& ignore)
{
    mSource<<Append("void loadSymbolTables()\n{" + NL());

    for (int i = 0; i < floatingSpeciesConcentrationList.size(); i++)
    {
        mSource<<Format("\tg.variableTable[{0}] = \"{1}\";{2}", i, floatingSpeciesConcentrationList[i].name, NL());
    }

    for (int i = 0; i < boundarySpeciesList.size(); i++)
    {
        mSource<<Format("\t\tg.boundaryTable[{0}] = \"{1}\";{2}", i, boundarySpeciesList[i].name, NL());
    }

	for (int i = 0; i < globalParameterList.size(); i++)
    {
		string name = globalParameterList[i].name;
       	mSource<<Format("\t\tg.globalParameterTable[{0}] = \"{1}\";{2}", i, globalParameterList[i].name, NL());
    }
    mSource<<Format("\t}{0}{0}", NL());
}

int CGenerator::ReadFloatingSpecies()
{
    // Load a reordered list into the variable list.
    StringList reOrderedList;
    if ((RoadRunner::mbComputeAndAssignConservationLaws))
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

void CGenerator::WriteInitFunction(StringBuilder& mSource, StringBuilder& source)
{
	source.Line("\n//Function to initialize the model data structure. Returns an integer indicating result");
    source.Line("int InitModel()");
    source.Line("{");

    //The following is from the constructor..
    source<<"\t"<<Append("g.numIndependentVariables = " , 	mNumIndependentSpecies , ";" , NL());
    source<<"\t"<<Append("g.numDependentVariables = " , 	mNumDependentSpecies , ";" , NL());
    source<<"\t"<<Append("g.numTotalVariables = " , 		mNumFloatingSpecies , ";" , NL());
    source<<"\t"<<Append("g.numBoundaryVariables = " , 		mNumBoundarySpecies , ";" , NL());
    source<<"\t"<<Append("g.numGlobalParameters = " , 		globalParameterList.size() , ";" , NL());
    source<<"\t"<<Append("g.numCompartments = " , 			compartmentList.size() , ";" , NL());
    source<<"\t"<<Append("g.numReactions = " , 				reactionList.size() , ";" , NL());
    source<<"\t"<<Append("g.numEvents = " , 				mNumEvents , ";" , NL());

    source<<tab<<	"g.mModelName = (char*) malloc(sizeof(char)*"<<strlen(mModelName.c_str()) + 1<<");" <<endl;
   	source<<tab<<	"strcpy(g.mModelName,\""<<mModelName<<"\");"<<endl;
	source.TLine(	"g._gp[0] = 1234;");

    source<<"\t"<<Append("InitializeDelays();" , NL());

      // Declare any eventAssignment delegates
      if (mNumEvents > 0)
      {
          source<<Append("\t\t_eventAssignments = new TEventAssignmentDelegate[numEvents];" , NL());
          source<<Append("\t\t_eventPriorities = new double[numEvents];" , NL());
          source<<Append("\t\t_computeEventAssignments= new TComputeEventAssignmentDelegate[numEvents];" , NL());
          source<<Append("\t\t_performEventAssignments= new TPerformEventAssignmentDelegate[numEvents];" , NL());

          for (int i = 0; i < mNumEvents; i++)
          {
          	string iStr = ToString(i);
              source<<Append("\t\t_eventAssignments[" + iStr + "] = new TEventAssignmentDelegate (eventAssignment_" + iStr +
                        ");" + NL());
              source<<Append("\t\t_computeEventAssignments[" + iStr +
                        "] = new TComputeEventAssignmentDelegate (computeEventAssignment_" + iStr + ");" + NL());
              source<<Append("\t\t_performEventAssignments[" + iStr +
                        "] = new TPerformEventAssignmentDelegate (performEventAssignment_" + iStr + ");" + NL());
          }

          source<<Append("\t\tresetEvents();" + NL());
          source<<Append(NL());
      }

      if (mNumModifiableSpeciesReferences > 0)
      {
          for (int i = 0; i < ModifiableSpeciesReferenceList.size(); i++)
          {
              source<<Append("\t\t_sr[" + ToString(i) + "]  = " + WriteDouble(ModifiableSpeciesReferenceList[i].value) + ";" + NL());
          }
          source<<Append(NL());
      }

      // Declare space for local parameters
      for (int i = 0; i < mNumReactions; i++)
      {
          source<<Append("\tg.localParameterDimensions[" + ToString(i) + "] = " , mLocalParameterDimensions[i] , ";" + NL());
          source<<"\tg._lp["<<i<<"] = (double*) malloc(sizeof(double)*"<<mLocalParameterDimensions[i]<<");"<<endl;
      }

    source.TLine("return 0;");
    source.Line("}");



//    mSource<<"\tchar*"<<tabs(4)	<<"mModelName;"<<NL();
//    mSource<<"\tchar**"<<tabs(4)<<"mWarnings;"<<NL();
//	mSource<<"\tdouble _gp["<<(mNumGlobalParameters + mTotalLocalParmeters)<<"];\t\t// Vector containing all the global parameters in the System  "<<NL();
//	if(mNumModifiableSpeciesReferences)
//    {
//      mSource<<"\tdouble _sr["<<mNumModifiableSpeciesReferences<<"];           // Vector containing all the modifiable species references  "<<endl;
//    }
//      mSource<<Append("\t double[][] _lp = new double[" + ToString(_NumReactions) +
//                "][];       // Vector containing all the local parameters in the System  " + NL());
//
//      mSource<<Append("\t double[] _y = new double[", floatingSpeciesConcentrationList.size(),
//                "];            // Vector containing the concentrations of all floating species ",  NL());
//
//      //mSource<<Append(String.Format("\t double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count, NL()));
//      mSource<<Format("\t double[] _init_y = new double[{0}];            // Vector containing the initial concentrations of all floating species {1}", floatingSpeciesConcentrationList.Count(), NL());
//
//      mSource<<Append("\t double[] _amounts = new double[", floatingSpeciesConcentrationList.size(),
//                "];      // Vector containing the amounts of all floating species ", NL());
//
//      mSource<<Append("\t double[] _bc = new double[", mNumBoundarySpecies,
//                "];           // Vector containing all the boundary species concentration values   " , NL());
//
//      mSource<<Append("\t double[] _c = new double[" , mNumCompartments ,
//                "];            // Vector containing all the compartment values   " + NL());
//
//      mSource<<Append("\t double[] _dydt = new double[" , floatingSpeciesConcentrationList.size() ,
//                "];         // Vector containing rates of changes of all species   " , NL());
//
//      mSource<<Append("\t double[] _rates = new double[" , mNumReactions ,
//                "];        // Vector containing the rate laws of all reactions    " , NL());
//
//      mSource<<Append("\t double[] _ct = new double[" , mNumDependentSpecies ,
//                "];           // Vector containing values of all conserved sums      " , NL());
//
//      mSource<<Append("\t double[] _eventTests = new double[" , mNumEvents ,
//                "];   // Vector containing results of any event tests        " , NL());
//
//      mSource<<Append("\t TEventDelayDelegate[] _eventDelay = new TEventDelayDelegate[" , mNumEvents ,
//                "]; // array of trigger function pointers" , NL());
//
//      mSource<<Append("\t bool[] _eventType = new bool[" , mNumEvents ,
//                "]; // array holding the status whether events are useValuesFromTriggerTime or not" , NL());
//
//      mSource<<Append("\t bool[] _eventPersistentType = new bool[" , mNumEvents ,
//                "]; // array holding the status whether events are persitstent or not" , NL());
//
//      mSource<<Append("\t double _time;" , NL());
//      mSource<<Append("\t int numIndependentVariables;" , NL());
//      mSource<<Append("\t int numDependentVariables;" , NL());
//      mSource<<Append("\t int numTotalVariables;" , NL());
//      mSource<<Append("\t int numBoundaryVariables;" , NL());
//      mSource<<Append("\t int numGlobalParameters;" , NL());
//      mSource<<Append("\t int numCompartments;" , NL());
//      mSource<<Append("\t int numReactions;" , NL());
//      mSource<<Append("\t int numRules;" , NL());
//      mSource<<Append("\t int numEvents;" , NL());
//      mSource<<Append("\tstring[] variableTable = new string[" , floatingSpeciesConcentrationList.size() , "];" , NL());
//      mSource<<Append("\tstring[] boundaryTable = new string[" , boundarySpeciesList.size() , "];" , NL());
//      mSource<<Append("\tstring[] globalParameterTable = new string[" , globalParameterList.size() , "];" , NL());
//      mSource<<Append("\tint[] localParameterDimensions = new int[" , mNumReactions , "];" , NL());
//      mSource<<Append("\t TEventAssignmentDelegate[] _eventAssignments;" , NL());
//      mSource<<Append("\t double[] _eventPriorities;" , NL());
//      mSource<<Append("\t TComputeEventAssignmentDelegate[] _computeEventAssignments;" , NL());
//      mSource<<Append("\t TPerformEventAssignmentDelegate[] _performEventAssignments;" , NL());
//      mSource<<Append("\t bool[] _eventStatusArray = new bool[" , _NumEvents , "];" , NL());
//      mSource<<Append("\t bool[] _previousEventStatusArray = new bool[" , _NumEvents , "];" , NL());
//      mSource<<Append(NL());
//      mSource<<Append("\t TModel ()  " , NL());
//      mSource<<Append("\t{" , NL());
//
//      mSource<<Append("\t\tnumIndependentVariables = " , _NumIndependentSpecies , ";" , NL());
//      mSource<<Append("\t\tnumDependentVariables = " , _NumDependentSpecies , ";" , NL());
//      mSource<<Append("\t\tnumTotalVariables = " , _NumFloatingSpecies , ";" , NL());
//      mSource<<Append("\t\tnumBoundaryVariables = " , _NumBoundarySpecies , ";" , NL());
//      mSource<<Append("\t\tnumGlobalParameters = " , globalParameterList.size() , ";" , NL());
//      mSource<<Append("\t\tnumCompartments = " , compartmentList.size() , ";" , NL());
//      mSource<<Append("\t\tnumReactions = " , reactionList.size() , ";" , NL());
//      mSource<<Append("\t\tnumEvents = " , _NumEvents , ";" , NL());
//      mSource<<Append("\t\tInitializeDelays();" , NL());
//
//      // Declare any eventAssignment delegates
//      if (_NumEvents > 0)
//      {
//          mSource<<Append("\t\t_eventAssignments = new TEventAssignmentDelegate[numEvents];" , NL());
//          mSource<<Append("\t\t_eventPriorities = new double[numEvents];" , NL());
//          mSource<<Append("\t\t_computeEventAssignments= new TComputeEventAssignmentDelegate[numEvents];" , NL());
//          mSource<<Append("\t\t_performEventAssignments= new TPerformEventAssignmentDelegate[numEvents];" , NL());
//
//          for (int i = 0; i < _NumEvents; i++)
//          {
//          	string iStr = ToString(i);
//              mSource<<Append("\t\t_eventAssignments[" + iStr + "] = new TEventAssignmentDelegate (eventAssignment_" + iStr +
//                        ");" + NL());
//              mSource<<Append("\t\t_computeEventAssignments[" + iStr +
//                        "] = new TComputeEventAssignmentDelegate (computeEventAssignment_" + iStr + ");" + NL());
//              mSource<<Append("\t\t_performEventAssignments[" + iStr +
//                        "] = new TPerformEventAssignmentDelegate (performEventAssignment_" + iStr + ");" + NL());
//          }
//
//          mSource<<Append("\t\tresetEvents();" + NL());
//          mSource<<Append(NL());
//      }
//
//      if (_NumModifiableSpeciesReferences > 0)
//      {
//          for (int i = 0; i < ModifiableSpeciesReferenceList.size(); i++)
//          {
//              mSource<<Append("\t\t_sr[" + ToString(i) + "]  = " + WriteDouble(ModifiableSpeciesReferenceList[i].value) + ";" + NL());
//          }
//          mSource<<Append(NL());
//      }
//
//      // Declare space for local parameters
//      for (int i = 0; i < _NumReactions; i++)
//      {
//          mSource<<Append("\t\tlocalParameterDimensions[" + ToString(i) + "] = " , _LocalParameterDimensions[i] , ";" + NL());
//          mSource<<Append("\t\t_lp[" + ToString(i) + "] = new double[" , _LocalParameterDimensions[i] , "];" , NL());
//      }
//
//      mSource<<Append("\t}" + NL() + NL());


	 	source.NewLine();
    	source.Line("char* GetModelName()");
    	source<<"{" 										<<endl;
    	source.TLine("return g.mModelName;");
    	source<<"}" 										<<endl;
}
}//Namespace


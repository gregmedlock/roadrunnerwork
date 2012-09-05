//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "rr_c_api.h"
#include "rr_c_api_support.h"
//---------------------------------------------------------------------------
#if defined(_MSC_VER)
	#include <direct.h>
	#define getcwd _getcwd
	#define chdir  _chrdir
	#define MAXPATH _MAX_PATH
#elif defined(__BORLANDC__)
  	#include <dir.h>
#else
#include <unistd.h>
#endif

using namespace std;
using namespace rr_c_api;

int main(int argc, char* argv[])
{
	double value;

	printf ("\n    Start of run\n");
	printf ("   ==============\n\n");

	string modelsPath(".\\..\\Models");
	if(argc > 1)
	{
		modelsPath = argv[1];
	}

	char* buffer = new char[MAXPATH];
	// Get the current working directory:
	if( (buffer = _getcwd( buffer, MAXPATH )) == NULL )
	{
		perror( "getcwd error" );
	}
	else
	{
		printf( "Current cwd = %s \nLength: %d\n", buffer, strlen(buffer) );
		delete [] buffer;
	}

	RRHandle rrHandle = NULL;
    rrHandle =  getRRInstance();

    if(!rrHandle)
    {
        cout<<"No handle...";
    }
    setTempFolder("c:\\rrTemp");
    enableLogging();
	char* text = getBuildDate();

	if(text)
	{
		cout<<"Build date: "<<text<<endl;
		freeText(text);
	}


	//string fileName = modelsPath + "\\ss_TurnOnConservationAnalysis.xml";
	//string fileName = modelsPath + "\\ss_SimpleConservedCycle.xml";
	string fileName = modelsPath + "\\ss_threeSpecies.xml";
	//string fileName = "ss_threeSpecies.xml";
	ifstream ifs(fileName.c_str());
	if(!ifs)
	{
		cerr<<"Failed opening file: "<<fileName;
		return false;
	}
	cout << "\nRunning model: " << fileName << endl;
	setComputeAndAssignConservationLaws(true);
	std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

	//cout<<sbml.c_str()<<endl;
	if(!loadSBML(sbml.c_str()))
	{
		cerr<<"Failed loading SBML from file:"<<fileName<<endl;
		cerr<<"Last error was: "<<getLastError()<<endl;
		return -1;
	}

	int r = getNumberOfReactions();
	int m = getNumberOfFloatingSpecies();
	int b = getNumberOfBoundarySpecies();
	int p = getNumberOfGlobalParameters();
	int c = getNumberOfCompartments();

	printf ("Number of reactions = %d\n", r);
	printf ("Number of floating species = %d\n", m);
	printf ("Number of boundary species = %d\n\n", b);
	printf ("Number of compartments = %d\n\n", c);

	if (m > 0) {
	   printf ("Compartment names:\n");
	   printf ("------------------\n");
	   cout<<printList(getCompartmentNames())<<endl<<endl;
	}

	if (m > 0) {
	   printf ("Floating species names:\n");
	   printf ("-----------------------\n");
	   cout<<printList(getFloatingSpeciesNames())<<endl<<endl;
	}

	if (m > 0) {
	   printf ("Initial Floating species names:\n");
	   printf ("-------------------------------\n");
	   cout<<printList(getFloatingSpeciesInitialConditionNames())<<endl;
	}

	if (b > 0) {
       printf ("\nBoundary species names:\n");
	   printf ("-----------------------\n");
	   cout<<printList(getBoundarySpeciesNames())<<endl;
	}
	printf ("\n");

	if (p > 0) {
       printf ("\nGlobal Parameter names:\n");
	   printf ("-----------------------\n");
	   cout<<printList(getGlobalParameterNames())<<endl;
	}
	printf ("\n");

	if (r > 0) {
       printf ("\nReaction names:\n");
	   printf ("---------------\n");
	   cout<<printList(getReactionNames())<<endl;
	}
	printf ("\n");

	if (m> 0) {
       printf ("\nRates of change names:\n");
	   printf ("----------------------\n");
	   cout<<printList(getRatesOfChangeNames())<<endl;
	}
	printf ("\n");


	if (r > 0) {
       printf ("\nUnscaled flux control coefficient names:\n");
	   printf ("----------------------------------------\n");
	   cout<<printStringArrayList(getUnscaledFluxControlCoefficientNames())<<endl;
	}
	printf ("\n");

	if (m > 0) {
       printf ("\nUnscaled concentration control coefficient names:\n");
	   printf ("-------------------------------------------------\n");
	   cout<<printStringArrayList(getUnscaledConcentrationControlCoefficientNames())<<endl;
	}
	printf ("\n");

	double ssVal;
    bool success = steadyState(ssVal);
    if(!success)
    {
		cerr<<"Steady State call failed. Error was: "<<getLastError()<<endl;
    }
    else
    {
	    cout<<"Compute Steady State: sums of squares: "<<ssVal<<endl;
    }

    cout<<"\nStoichiometry Matrix:"<<endl;
	printf ("---------------------\n\n");
	cout<<printMatrix(getStoichiometryMatrix());
	printf ("\n");

    cout<<"Number of independent species = "<<getNumberOfIndependentSpecies()<<endl;
    cout<<"Number of dependent Species = "<<getNumberOfDependentSpecies()<<endl<<endl;
 
	printf ("Link Matrix:\n");
	printf ("------------\n\n");
	cout<<printMatrix(getLinkMatrix()); printf ("\n\n");

	printf ("Nr Matrix:\n");
	printf ("-----------\n\n");
	cout<<printMatrix(getNrMatrix()); printf ("\n\n");

	printf ("L0 Matrix:\n");
	printf ("-----------\n\n");
	cout<<printMatrix(getL0Matrix()); printf ("\n\n");

	printf ("Full Jacobian Matrix:\n");
	printf ("---------------------\n\n");
	char* matStr = printMatrix (getFullJacobian());
	if (!matStr)
		printf ("ERROR in getFullJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Reduced Jacobian Matrix:\n");
	printf ("------------------------\n\n");
	matStr = printMatrix (getReducedJacobian());
	if (!matStr)
		printf ("ERROR in getReducedJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Reduced Jacobian Matrix:\n");
	printf ("------------------------\n\n");
	matStr = printMatrix (getReducedJacobian());
	if (!matStr)
		printf ("ERROR in getReducedJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Eigenvalue Matrix (real/imag):\n");
	printf ("----------------------------\n\n");
	matStr = printMatrix (getEigenValues());
	if (!matStr)
		printf ("ERROR in getEigenValues\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Unscaled Elasticity Matrix:\n");
	printf ("-------------------------\n\n");
	matStr = printMatrix (getUnScaledElasticityMatrix());
	if (!matStr)
		printf ("ERROR in getUnScaledElasticityMatrix\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");
	
	printf ("Scaled Elasticity Matrix:\n");
	printf ("-------------------------\n\n");
	matStr = printMatrix (getScaledElasticityMatrix());
	if (!matStr)
		printf ("ERROR in getScaledElasticityMatrix\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Unscaled Concentration Control Coefficients Matrix:\n");
	printf ("---------------------------------------------------\n\n");
	matStr = printMatrix (getUnscaledConcentrationControlCoefficientMatrix());
	if (!matStr)
    {
		printf ("ERROR in getUnscaledConcentrationControlCoefficientMatrix\n");
        cerr<<getLastError()<<endl;
    }
	else
    {
		printf ("%s", matStr);
    }
	printf ("\n\n");

	printf ("Scaled Concentration Control Coefficients Matrix:\n");
	printf ("-------------------------------------------------\n\n");
	matStr = printMatrix (getScaledConcentrationControlCoefficientMatrix());
	if (!matStr)
    {
		printf ("ERROR in getScaledConcentrationControlCoefficientMatrix\n");
        cerr<<getLastError()<<endl;
    }
	else
    {
		printf ("%s", matStr);
    }
	printf ("\n\n");

	printf ("Unscaled Flux Control Coefficients Matrix:\n");
	printf ("-----------------------------------------\n\n");
	matStr = printMatrix (getUnscaledFluxControlCoefficientMatrix());
	if (!matStr)
    {
		printf ("ERROR in getUnscaledFluxControlCoefficientMatrix\n");
        cerr<<getLastError()<<endl;
    }
	else
    {
		printf ("%s", matStr);
    }
	printf ("\n\n");


	printf ("Scaled Flux Control Coefficients Matrix:\n");
	printf ("-----------------------------------------\n\n");
	matStr = printMatrix (getScaledFluxControlCoefficientMatrix());
	if (!matStr)
    {
		printf ("ERROR in getScaledFluxControlCoefficientMatrix\n");
        cerr<<getLastError()<<endl;
    }
	else
    {
		printf ("%s", matStr);
    }
	printf ("\n\n");

	printf ("Flux Control Coefficient, CC^(_J1)_k1\n");
	getCC("_J1", "k1", value);
	printf ("Coefficient = %f\n", value);

	printf ("Flux Control Coefficient, CC^(_J1)_k2\n");
	getCC("_J1", "k2", value);
	printf ("Coefficient = %f\n", value);

	printf ("Flux Control Coefficient, CC^(_J1)_k3\n");
	getCC("_J1", "k3", value);
	printf ("Coefficient = %f\n", value);

	printf ("Flux Control Coefficient, CC^(_J1)_k4\n");
	getCC("_J1", "k4", value);
	printf ("Coefficient = %f\n", value);

	printf ("Elasticity Coefficient, EE^(_J1)_S1\n");
	getEE("_J1", "S1", value);
	printf ("Elasticity = %f\n", value);

	printf ("Elasticity Coefficient, EE^(_J2)_S1\n");
	getEE("_J2", "S1", value);
	printf ("Elasticity = %f\n", value);

	printf ("Elasticity Coefficient, EE^(_J2)_S2\n");
	getEE("_J2", "S2", value);
	printf ("Elasticity = %f\n", value);

	printf ("Elasticity Coefficient, EE^(_J3)_S2\n");
	getEE("_J3", "S2", value);
	printf ("Elasticity = %f\n", value);

	printf ("\n");
	//printf ("Flux Control Coefficient, C^(_J1)_k1\n");
	//double value;
	//getCC("_J1", "k1", value);
	//printf ("FCC = %f\n", value);

	/*getGlobalParameterByIndex (0, value);
	printf ("%f\n", value);
	getGlobalParameterByIndex (1, value);
	printf ("%f\n", value);
	getGlobalParameterByIndex (2, value);
	printf ("%f\n", value);
	getGlobalParameterByIndex (3, value);
	printf ("%f\n", value);*/

	RRVector veca;
	veca.Size = 3;
	veca.Data = new double[3];
   	veca.Data[0] = 1;
	veca.Data[1] = 2;
	veca.Data[2] = 3;

    cout<<"List of floating species: \n"<<printList(getFloatingSpeciesNames())<<endl;

	printf ("\nCall to getRatesOfChangeEx (S1=1, S2=2, S3=3):\n");
	cout<<printVector (getRatesOfChangeEx(&veca))<<endl;


//	printf ("\nCall to getReactionRatesEx (S1=1, S2=2, S3=3):\n");
//	cout<<printVector (getReactionRatesEx (&veca))<<endl;
//
//	printf ("\nCall to getRatesOfChange (with S1=1, S2=2, S3=3):\n");
//	cout<<printVector (getRatesOfChange())<<endl;

    setSelectionList("S1 S2");
//-------- The latest

    cout<<printVector(getFloatingSpeciesConcentrations());
    cout<<printVector(getGlobalParameterValues());
    cout<<"\n\n Symbols\n";
    RRArrayList2* symHandle = getAvailableSymbols();
    cout<<printArrayList(symHandle);
    freeArrayList(symHandle);
    cout<<"\n\n ================================\n";
    RRVector* test = getReactionRates();
    cout<<printVector(test);

    setFloatingSpeciesByIndex(0,2);
    setFloatingSpeciesByIndex(1,4);
    setFloatingSpeciesByIndex(2,6);

    test = getReactionRates();
    cout<<printVector(test);

    //Get value problem..



    getValue("S1", value);
    cout<<value<<endl;
    getValue("S2", value);
    cout<<value<<endl;
    getValue("S3", value);
    cout<<value<<endl;

    getRatesOfChange();

    getValue("S1", value);
    cout<<value<<endl;
    getValue("S2", value);
    cout<<value<<endl;
    getValue("S3", value);
    cout<<value<<endl;

	//cout<<getBoundarySpeciesByIndex (0)<<endl;
    //getGlobalParameterByIndex(0, value);

    //cout<<value<<endl;
    //getGlobalParameterByIndex(2, value);
    //cout<<value<<endl;

    //cout<<getParamPromotedSBML(sbml.c_str());

    //cout<<getSBML()<<endl;

    //cout<<printMatrix(getScaledElasticityMatrix());     //How to free, when doing something like this??
    //cout<<printList(getEigenValueNames());

    cout<<"\n FluxControlCoeff ------\n"<<printStringArrayList(getFluxControlCoefficientNames())<<endl;

    cout<<"\n Unscaled FluxControlCoeff ------\n"<<printStringArrayList(getUnscaledFluxControlCoefficientNames())<<endl;
    RRStringArrayList* list =getConcentrationControlCoefficientNames();
    cout<<printStringArrayList(list)<<endl;
    freeStringArrayList(list);


    //cout<<printList(getElasticityNames())<<endl;

//    setBoundarySpeciesByIndex(0,34);
    cout<<"Nr of Compartments: "<<getNumberOfCompartments()<<endl;
    setCompartmentByIndex(0,456);
    if(getCompartmentByIndex(0, value))
    {
        cout<<"Compartment Volume: "<<value<<endl;
    }
    else
    {
        cout<<getLastError()<<endl;
    }
    cout<<printList(getCompartmentNames())<<endl;

    getRateOfChange(0, value);
    cout<<"Rate of change:"<<value<<endl;

	cout<<"API Version: "<<getVersion()<<endl;

    cout<<printList(getFloatingSpeciesInitialConditionNames())<<endl;


    cout<<" ---- getElasticityCoefficientNames ---\n"<<printStringArrayList(getElasticityCoefficientNames())<<endl;
    cout<<printList(getRateOfChangeNames())<<endl;
    setCapabilities (NULL);
    cout<<getCapabilities()<<endl;

//    C_DECL_SPEC bool                    rrCallConv   getScaledFloatingSpeciesElasticity(const char* reactionName, const char* speciesName, double& value);
    if(getScaledFloatingSpeciesElasticity("_J1", "S1", value))
    {
        cout<<"ScaledFloatingSpeciesElasticity "<<value<<endl;
    }
    else
    {
        cout<<getLastError()<<endl;
    }

    cout<<"getFloatingSpeciesInitialConditionNames: "<<printList(getFloatingSpeciesInitialConditionNames())<<endl;


    cout<<writeSBML();

	///////////////////
    text = getCopyright();
    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
    }

    cout<<text<<endl;
    freeText(text);
    freeRRInstance(rrHandle);
	Pause();
    return 0;
}


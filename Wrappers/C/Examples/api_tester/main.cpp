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

	// Test list type
	printf ("Tesing list type\n");

	cRRListHandle myList = createRRList();
	
	// First construct [5, 3.1415]
	cRRListItemHandle myItem = createIntegerItem (5);
	addItem (myList, &myItem);
	myItem = createDoubleItem (3.1415);
	addItem (myList, &myItem);

	// Next construct [5, 3.1415, [2.7182, "Hello"]]
	myItem = createListItem (createRRList());
    addItem (myList, &myItem);
	cRRListItemHandle newItem = createDoubleItem (2.7182);
	addItem (getList (myItem), &newItem);

	newItem = createStringItem ("Hello");
	addItem (getList (myItem), &newItem);

	if (isListItemInteger (myList->myItems[0]))
		printf ("Yes\n");

	int length = getListLength (myList);
	myItem = getListItem (myList, 0);
	myItem = getListItem (myList, 1);
	myItem = getListItem (myList, 2);
	myItem = getListItem (myList, 3);
	if (myItem == NULL)
		printf ("Index overflow in getListItem\n");

	if (isListItem (getListItem (myList, 1), litInteger))
		printf ("Yes it is an integer\n");

	printf ("\nList:\n");
	printf (listToString (myList));
	printf ("\n\n");
		
	freeRRList (myList);

	printf ("Hit any key to continue\n");
	getchar ();

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

    char* cFileName = getCSourceFileName();

    if(cFileName)
    {
    	cout<<"\n C Source File Name: "<<cFileName;
    }

    freeText(cFileName);

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
	   cout<<stringArrayToString(getCompartmentIds())<<endl<<endl;
	}

	if (m > 0) {
	   printf ("Floating species names:\n");
	   printf ("-----------------------\n");
	   cout<<stringArrayToString(getFloatingSpeciesIds())<<endl<<endl;
	}

	if (m > 0) {
	   printf ("Initial Floating species names:\n");
	   printf ("-------------------------------\n");
	   cout<<stringArrayToString(getFloatingSpeciesInitialConditionIds())<<endl;
	}

	if (b > 0) {
       printf ("\nBoundary species names:\n");
	   printf ("-----------------------\n");
	   cout<<stringArrayToString(getBoundarySpeciesIds())<<endl;
	}
	printf ("\n");

	if (p > 0) {
       printf ("\nGlobal Parameter names:\n");
	   printf ("-----------------------\n");
	   cout<<stringArrayToString(getGlobalParameterIds())<<endl;
	}
	printf ("\n");

	if (r > 0) {
       printf ("\nReaction names:\n");
	   printf ("---------------\n");
	   cout<<stringArrayToString(getReactionIds())<<endl;
	}
	printf ("\n");

	if (m> 0) {
       printf ("\nRates of change names:\n");
	   printf ("----------------------\n");
	   cout<<stringArrayToString(getRatesOfChangeIds())<<endl;
	}
	printf ("\n");


	if (r > 0) {
       printf ("\nUnscaled flux control coefficient names:\n");
	   printf ("----------------------------------------\n");
	   cout<<stringArrayToString(getUnscaledFluxControlCoefficientIds())<<endl;
	}
	printf ("\n");

	if (m > 0) {
       printf ("\nUnscaled concentration control coefficient names:\n");
	   printf ("-------------------------------------------------\n");
	   cout<<stringArrayToString(getUnscaledConcentrationControlCoefficientIds())<<endl;
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
	cout<<matrixToString(getStoichiometryMatrix());
	printf ("\n");

    cout<<"Number of independent species = "<<getNumberOfIndependentSpecies()<<endl;
    cout<<"Number of dependent Species = "<<getNumberOfDependentSpecies()<<endl<<endl;
 
	printf ("Link Matrix:\n");
	printf ("------------\n\n");
	cout<<matrixToString(getLinkMatrix()); printf ("\n\n");

	printf ("Nr Matrix:\n");
	printf ("-----------\n\n");
	cout<<matrixToString(getNrMatrix()); printf ("\n\n");

	printf ("L0 Matrix:\n");
	printf ("-----------\n\n");
	cout<<matrixToString(getL0Matrix()); printf ("\n\n");

	printf ("Full Jacobian Matrix:\n");
	printf ("---------------------\n\n");
	char* matStr = matrixToString (getFullJacobian());
	if (!matStr)
		printf ("ERROR in getFullJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Reduced Jacobian Matrix:\n");
	printf ("------------------------\n\n");
	matStr = matrixToString (getReducedJacobian());
	if (!matStr)
		printf ("ERROR in getReducedJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Reduced Jacobian Matrix:\n");
	printf ("------------------------\n\n");
	matStr = matrixToString (getReducedJacobian());
	if (!matStr)
		printf ("ERROR in getReducedJacobian\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Eigenvalue Matrix (real/imag):\n");
	printf ("----------------------------\n\n");
	matStr = matrixToString (getEigenValues());
	if (!matStr)
		printf ("ERROR in getEigenValues\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Unscaled Elasticity Matrix:\n");
	printf ("-------------------------\n\n");
	matStr = matrixToString (getUnScaledElasticityMatrix());
	if (!matStr)
		printf ("ERROR in getUnScaledElasticityMatrix\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");
	
	printf ("Scaled Elasticity Matrix:\n");
	printf ("-------------------------\n\n");
	matStr = matrixToString (getScaledElasticityMatrix());
	if (!matStr)
		printf ("ERROR in getScaledElasticityMatrix\n");
	else
		printf ("%s", matStr);
	printf ("\n\n");

	printf ("Unscaled Concentration Control Coefficients Matrix:\n");
	printf ("---------------------------------------------------\n\n");
	matStr = matrixToString (getUnscaledConcentrationControlCoefficientMatrix());
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
	matStr = matrixToString (getScaledConcentrationControlCoefficientMatrix());
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
	matStr = matrixToString (getUnscaledFluxControlCoefficientMatrix());
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
	matStr = matrixToString (getScaledFluxControlCoefficientMatrix());
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
	veca.Count = 3;
	veca.Data = new double[3];
   	veca.Data[0] = 1;
	veca.Data[1] = 2;
	veca.Data[2] = 3;

    cout<<"List of floating species: \n"<<stringArrayToString(getFloatingSpeciesIds())<<endl;

	printf ("\nCall to getRatesOfChangeEx (S1=1, S2=2, S3=3):\n");
	cout<<vectorToString (getRatesOfChangeEx(&veca))<<endl;


//	printf ("\nCall to getReactionRatesEx (S1=1, S2=2, S3=3):\n");
//	cout<<printVector (getReactionRatesEx (&veca))<<endl;
//
//	printf ("\nCall to getRatesOfChange (with S1=1, S2=2, S3=3):\n");
//	cout<<printVector (getRatesOfChange())<<endl;

    setSelectionList("S1 S2");
//-------- The latest

    cout<<vectorToString(getFloatingSpeciesConcentrations());
    cout<<vectorToString(getGlobalParameterValues());
    cout<<"\n\n Symbols\n";
    cRRList* symHandle = getAvailableSymbols();
    cout<<listToString(symHandle);
    freeRRList(symHandle);
    cout<<"\n\n ================================\n";
    RRVector* test = getReactionRates();
    cout<<vectorToString(test);

    setFloatingSpeciesByIndex(0,2);
    setFloatingSpeciesByIndex(1,4);
    setFloatingSpeciesByIndex(2,6);

    test = getReactionRates();
    cout<<vectorToString(test);

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
    //cout<<printStringList(getEigenValueNames());

    cout<<"\n FluxControlCoeff ------\n"<<stringArrayToString(getFluxControlCoefficientIds())<<endl;

    cout<<"\n Unscaled FluxControlCoeff ------\n"<<stringArrayToString(getUnscaledFluxControlCoefficientIds())<<endl;
    RRStringArray* list =getConcentrationControlCoefficientIds();
    cout<<stringArrayToString(list)<<endl;
    freeStringArray(list);


    //cout<<printStringList(getElasticityNames())<<endl;

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
    cout<<stringArrayToString(getCompartmentIds())<<endl;

    getRateOfChange(0, value);
    cout<<"Rate of change:"<<value<<endl;

	cout<<"API Version: "<<getVersion()<<endl;

    cout<<stringArrayToString(getFloatingSpeciesInitialConditionIds())<<endl;


    cout<<" ---- getElasticityCoefficientNames ---\n"<<stringArrayToString(getElasticityCoefficientIds())<<endl;
    cout<<stringArrayToString(getRateOfChangeIds())<<endl;
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

    cout<<"getFloatingSpeciesInitialConditionNames: "<<stringArrayToString(getFloatingSpeciesInitialConditionIds())<<endl;


    cout<<getCurrentSBML();

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
	//Pause();
    return 0;
}


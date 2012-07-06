//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "rr_c_api.h"
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

	char* text = getBuildDate();

	if(text)
	{
		cout<<"Build date: "<<text<<endl;
		freeText(text);
	}

    setTempFolder("c:\\rrTemp");
	//string fileName = modelsPath + "\\ss_TurnOnConservationAnalysis.xml";
	//string fileName = modelsPath + "\\ss_SimpleConservedCycle.xml";
	string fileName = modelsPath + "\\ss_threeSpecies.xml";
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
		cerr<<"Last error was"<<getLastError()<<endl;
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
	   cout<<printList(getUnscaledFluxControlCoefficientNames())<<endl;
	}
	printf ("\n");

	if (m > 0) {
       printf ("\nUnscaled concentration control coefficient names:\n");
	   printf ("-------------------------------------------------\n");
	   cout<<printList(getUnscaledConcentrationControlCoefficientNames())<<endl;
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
	matStr = printMatrix (getEigenvalues());
	if (!matStr)
		printf ("ERROR in getEigenvalues\n");
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

	struct RRVector veca;
	veca.Size = 3;
	veca.Data = (double *) malloc (sizeof (double)*3);
	veca.Data[0] = 1;
	veca.Data[1] = 2;
	veca.Data[2] = 3;

	printf ("\nCall to getReactionRatesEx (S1=1, S2=2, S3=3):\n");
	cout<<printVector (getReactionRatesEx (&veca))<<endl;

	printf ("\nCall to getRatesOfChange (with S1=1, S2=2, S3=3):\n");
	cout<<printVector (getRatesOfChange())<<endl;

	printf ("\nCall to getRatesOfChangeEx (S1=1, S2=2, S3=3):\n");
	cout<<printVector (getRatesOfChangeEx(&veca))<<endl;


	//cout<<getBoundarySpeciesByIndex (0)<<endl;
    //getFloatingSpeciesByIndex(0, value);
    //cout<<value<<endl;
    //getGlobalParameterByIndex(0, value);

    //cout<<value<<endl;
    //getGlobalParameterByIndex(2, value);
    //cout<<value<<endl;


    RRVector* vec = getRatesOfChange();
    //cout<<getParamPromotedSBML(sbml.c_str());
	evalModel();

    //cout<<getSBML()<<endl;

    //cout<<printMatrix(getScaledElasticityMatrix());     //How to free, when doing something like this??
    //cout<<printList(getEigenValueNames());

    cout<<printList(getFluxControlCoefficientNames())<<endl;
    //cout<<printList(getConcentrationControlCoefficientNames())<<endl;
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
    getRatesOfChangeEx (NULL);
    getReactionRatesEx (NULL);
    //cout<<printList(getElasticityCoefficientNames())<<endl;
    cout<<printList(getRateOfChangeNames())<<endl;
    setCapabilities (NULL);
    //cout<<getCapabilities()<<endl;

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

    RRVector* test = getReactionRates();
    cout<<printVector(test);

    for(int i = 0; i < test->Size; i++)
    {
        test->Data[i] = i;
    }

    test =  getReactionRatesEx(test);

    cout<<printVector(test);

    test = getRatesOfChangeEx(test);
    cout<<printVector(test)<<endl;
    freeVector(test);


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
	getchar();
    return 0;
}


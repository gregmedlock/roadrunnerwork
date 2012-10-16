#include <string>
#include "UnitTest++.h"
#include "rr_c_api.h"
using namespace std;
using namespace UnitTest;


RRHandle gRR;	//Global roadrunner C handle

SUITE(Base)
{
    TEST(AllocateRR)
    {
        if(!gRR)
        {
            gRR = getRRInstance();

        }
        CHECK(gRR!=NULL);
    }

    TEST(LOAD_SBML)
    {
        CHECK(gRR!=NULL);
        string fName =  "..\\Models\\ss_threeSpedcies.xml";
        CHECK(loadSBMLFromFile(fName.c_str()));
        CHECK(gRR==NULL);
    }


    TEST(FULL_JACOBIAN)
    {
    //	CHECK(gRR!=NULL);
    //
    //    string fName =  "..\\Models\\ss_threeSpecies.xml";
    //	CHECK(gRR->loadSBMLFromFile(fName));
    //
    //	DoubleMatrix jaco = gRR->getFullJacobian();
    //	//Expected result
    //    Log(lInfo)<<jaco;
    ////          S1       S2       S3
    ////S1{{   -0.15        0        0}
    ////S2 {    0.15     -0.4        0}
    ////S3 {       0      0.4    -0.55}}

    }
}


#include "UnitTest++.h"
#include "rrLogger.h"
#include "rrRoadRunner.h"
using namespace UnitTest;

RoadRunner* gRR = NULL;
TEST(AllocateRR)
{
	if(!gRR)
    {
		gRR = new RoadRunner;

    }
	CHECK(gRR!=NULL);
}

TEST(LOAD_SBML)
{
	CHECK(gRR!=NULL);
    string fName =  "..\\Models\\ss_threeSpecies.xml";
	CHECK(gRR->loadSBMLFromFile(fName));
}


TEST(FULL_JACOBIAN)
{
	CHECK(gRR!=NULL);

    string fName =  "..\\Models\\ss_threeSpecies.xml";
	CHECK(gRR->loadSBMLFromFile(fName));

	DoubleMatrix jaco = gRR->getFullJacobian();
	//Expected result
    Log(lInfo)<<jaco;
//          S1       S2       S3
//S1{{   -0.15        0        0}
//S2 {    0.15     -0.4        0}
//S3 {       0      0.4    -0.55}}

}



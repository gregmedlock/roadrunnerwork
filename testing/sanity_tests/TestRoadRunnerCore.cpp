#include "UnitTest++.h"
#include "rrLogger.h"
#include "rrRoadRunner.h"
using namespace UnitTest;

TEST(AllocateRR)
{
	RoadRunner *rr = new RoadRunner;
	CHECK(rr!=NULL);
    delete rr;
}


TEST(LOAD_SBML)
{
	RoadRunner *rr = new RoadRunner;
	CHECK(rr!=NULL);

    string fileName =  "..\\..\\..\\ss_threeSpecies.xml";

	ifstream ifs(fileName.c_str());
	CHECK(!ifs);

	rr->ComputeAndAssignConservationLaws(false);

	std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
	CHECK(rr->loadSBML(sbml.c_str()));
}



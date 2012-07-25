#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrRoadRunner.h"
#include "rrCapsSupport.h"
#include "rrCVodeInterface.h"
//---------------------------------------------------------------------------

namespace rr
{

CapsSupport::CapsSupport(RoadRunner* rr)
:
mRoadRunner(rr),
mName("RoadRunner"),
mDescription("Settings For RoadRunner")
{

    if(mRoadRunner && mRoadRunner->GetCVodeInterface())
    {
        CvodeInterface*  cvode = mRoadRunner->GetCVodeInterface();
        CapabilitiesSection integration("integration", "CVODE", "CVODE Integrator");

        integration.Add(Capability<int>("BDFOrder", cvode->MaxBDFOrder, "Maximum order for BDF Method"));
//            new Capability("AdamsOrder", CvodeInterface.MaxAdamsOrder, "Maximum order for Adams Method"),
//            new Capability("rtol", CvodeInterface.relTol, "Relative Tolerance"),
//            new Capability("atol", CvodeInterface.absTol, "Absolute Tolerance"),
//            new Capability("maxsteps", CvodeInterface.MaxNumSteps, "Maximum number of internal stepsc"),
//            new Capability("initstep", CvodeInterface.InitStep, "the initial step size"),
//            new Capability("minstep", CvodeInterface.MinStep, "specifies a lower bound on the magnitude of the step size."),
//            new Capability("maxstep", CvodeInterface.MaxStep, "specifies an upper bound on the magnitude of the step size."),
//            new Capability("conservation", RoadRunner._bComputeAndAssignConservationLaws, "enables (=1) or disables (=0) the conservation analysis of models for timecourse simulations.")
//         })
//    };

        //Add section to Capablities
        Add(integration);
    }

    if(mRoadRunner && mRoadRunner->GetNLEQInterface())
    {

        NLEQInterface* solver = mRoadRunner->GetNLEQInterface();
        CapabilitiesSection steady("SteadyState", "NLEQ2", "NLEQ2 Steady State Solver");
        steady.Add(Capability<int>("MaxIterations", solver->maxIterations, "Maximum number of newton iterations"));
        steady.Add(Capability<double>("relativeTolerance", solver->relativeTolerance, "Relative precision of solution components"));
        Add(steady);
    }
}

void CapsSupport::Add(const CapabilitiesSection& section)
{

}

//Capabilities sectin
CapabilitiesSection::CapabilitiesSection(const string& name, const string& method, const string& descr)
{
}

void CapabilitiesSection::Add(const AbstractCapability& me)
{

}

}

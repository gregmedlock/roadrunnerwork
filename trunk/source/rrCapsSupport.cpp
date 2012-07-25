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
mName("RoadRunner"),
mDescription("Settings For RoadRunner"),
mRoadRunner(rr)
{

    if(mRoadRunner && mRoadRunner->GetCVodeInterface())
    {
        CvodeInterface*  cvode = mRoadRunner->GetCVodeInterface();
        CapabilitiesSection integration("integration", "CVODE", "CVODE Integrator");

        integration.Add(new CapabilityType<int>(    "BDFOrder",     cvode->MaxBDFOrder,     "Maximum order for BDF Method"));
        integration.Add(new CapabilityType<int>(    "AdamsOrder",   cvode->MaxAdamsOrder,   "Maximum order for Adams Method"));
        integration.Add(new CapabilityType<double>( "rtol",         cvode->relTol,          "Relative Tolerance"));
        integration.Add(new CapabilityType<double>( "atol",         cvode->absTol,          "Absolute Tolerance"));
        integration.Add(new CapabilityType<int>(    "maxsteps",     cvode->MaxNumSteps,     "Maximum number of internal stepsc"));
        integration.Add(new CapabilityType<double>( "initstep",     cvode->InitStep,        "the initial step size"));
        integration.Add(new CapabilityType<double>( "minstep",      cvode->MinStep,         "specifies a lower bound on the magnitude of the step size."));
        integration.Add(new CapabilityType<double>( "maxstep",      cvode->MaxStep,         "specifies an upper bound on the magnitude of the step size."));
        integration.Add(new CapabilityType<bool>(   "conservation", mRoadRunner->mComputeAndAssignConservationLaws,
                                                                                "enables (=1) or disables (=0) the conservation analysis of models for timecourse simulations."));

        //Add section to Capablities
        Add(integration);
    }

    if(mRoadRunner && mRoadRunner->GetNLEQInterface())
    {
        NLEQInterface* solver = mRoadRunner->GetNLEQInterface();
        CapabilitiesSection steady("SteadyState", "NLEQ2", "NLEQ2 Steady State Solver");
        steady.Add(new CapabilityType<int>("MaxIterations", solver->maxIterations, "Maximum number of newton iterations"));
        steady.Add(new CapabilityType<double>("relativeTolerance", solver->relativeTolerance, "Relative precision of solution components"));
        Add(steady);
    }
}

u_int CapsSupport::SectionCount()
{
    return mCapabilitiesSections.size();
}

void CapsSupport::Add(const CapabilitiesSection& section)
{
    mCapabilitiesSections.push_back(section);
}

string CapsSupport::AsString()
{
    string caps;
    for(int i = 0; i < SectionCount(); i++)
    {
        CapabilitiesSection& aSec = mCapabilitiesSections[i];
        caps += aSec.AsString();
    }
    return caps;
}

//Capabilities section
CapabilitiesSection::CapabilitiesSection(const string& name, const string& method, const string& description)
:
mName(name),
mMethod(method),
mDescription(description)
{
}

CapabilitiesSection::CapabilitiesSection(const CapabilitiesSection& from)
:
mName(from.mName),
mMethod(from.mMethod),
mDescription(from.mDescription),
mCapabilities(from.mCapabilities)
{


}

u_int CapabilitiesSection::Count()
{
    return mCapabilities.size();
}

void CapabilitiesSection::Add(const Capability* me)
{
    mCapabilities.push_back(me);
}

string CapabilitiesSection::AsString()
{
    stringstream caps;
    caps<<"Section: " << mName <<endl;
    caps<<"Method: " << mMethod<<endl;
    caps<<"Description: " << mDescription<<endl;

    for(int i = 0; i < Count(); i++)
    {
        caps <<*(mCapabilities[i])<<endl;
    }
    return caps.str();
}
}

#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrPendingAssignment.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{
PendingAssignment::PendingAssignment(
					double time,
                    TComputeEventAssignmentDelegate computeAssignment,
                    TPerformEventAssignmentDelegate performAssignment,
                    bool useValuesFromTriggerTime,
                    int index)
{
    Time = time;
    ComputeAssignment = computeAssignment;
    PerformAssignment = performAssignment;
    Index = index;
    UseValuesFromTriggerTime = useValuesFromTriggerTime;
    if (useValuesFromTriggerTime)
    {
        ComputedValues = computeAssignment();
    }
}



void PendingAssignment::AssignToModel()
{
    if (!UseValuesFromTriggerTime)
        ComputedValues = ComputeAssignment();
    PerformAssignment(ComputedValues);
}

}

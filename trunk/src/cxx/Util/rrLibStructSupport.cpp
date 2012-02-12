#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrLibStructSupport.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{
StructAnalysis::StructAnalysis()
:
mInstance(LibStructural::getInstance())
{

}

StructAnalysis::~StructAnalysis()
{
}


//vector<string> StructAnalysis::GetReorderedSpeciesIds()
//{
////	return (mInstance) ? mInstance->getReorderedSpecies() : vector<string>(0);
//
//}

//bool StructAnalysis::LoadSBML(const string& sbml)
//{
//    string msg = mInstance->LibStructural::loadSBML(sbml);
////    cout << msg;
//    return true;
//}
//
//int	StructAnalysis::GetNumberOfIndependentSpecies()
//{
//	return LibStructural_getNumIndSpecies();
//}
//
//vector<string> StructAnalysis::GetIndependentSpecies()
//{
//	return LibStructural::getInstance()->getIndependentSpecies();	//Static metod of class LibStructural
//}
//
//vector<string> StructAnalysis::GetDependentSpecies()
//{
//	return LibStructural::getInstance()->getDependentSpecies();
//}
//
//int StructAnalysis::GetNumSpecies()
//{
//	return LibStructural::getInstance()->getNumSpecies();
//}
//
//vector<string> StructAnalysis::GetSpecies()
//{
//	return LibStructural::getInstance()->getSpecies();
//}
}//namespace rr

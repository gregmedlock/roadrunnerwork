#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrLibStructSupport.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{
LibStructWrapper::LibStructWrapper()
:
mInstance(LibStructural::getInstance())
{

}

LibStructWrapper::~LibStructWrapper()
{
}


vector<string> LibStructWrapper::GetReorderedSpeciesIds()
{
	return (mInstance) ? mInstance->getReorderedSpecies() : vector<string>(0);

}

//bool LibStructWrapper::LoadSBML(const string& sbml)
//{
//    string msg = mInstance->LibStructural::loadSBML(sbml);
////    cout << msg;
//    return true;
//}
//
//int	LibStructWrapper::GetNumberOfIndependentSpecies()
//{
//	return LibStructural_getNumIndSpecies();
//}
//
//vector<string> LibStructWrapper::GetIndependentSpecies()
//{
//	return LibStructural::getInstance()->getIndependentSpecies();	//Static metod of class LibStructural
//}
//
//vector<string> LibStructWrapper::GetDependentSpecies()
//{
//	return LibStructural::getInstance()->getDependentSpecies();
//}
//
//int LibStructWrapper::GetNumSpecies()
//{
//	return LibStructural::getInstance()->getNumSpecies();
//}
//
//vector<string> LibStructWrapper::GetSpecies()
//{
//	return LibStructural::getInstance()->getSpecies();
//}
}//namespace rr

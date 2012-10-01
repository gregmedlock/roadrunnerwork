//---------------------------------------------------------------------------
#ifndef rrConstantsH
#define rrConstantsH
//---------------------------------------------------------------------------
#include <limits>
#include <string>
#include "rrExporter.h"
using namespace std;
namespace rr
{

RR_DECLSPEC extern char* 	WinPathSeparator;
RR_DECLSPEC extern char* 	LinuxPathSeparator;
RR_DECLSPEC extern char* 	EmptyString;
RR_DECLSPEC extern char* 	Comma;
RR_DECLSPEC extern string 	DefaultCompiler;

// Typedefs
typedef unsigned int 	u_int;
typedef long*   		IntPtr;

// Constants
RR_DECLSPEC extern char 	 tab;
RR_DECLSPEC extern double    DoubleNaN;
RR_DECLSPEC extern float     FloatNaN;
const int MAX_MODULE = 512;

// Enums...
enum SBMLType {stCompartment = 0, stSpecies, stParameter};    //Species clashes with class Species, prefix enums with st, for SbmlType

}
#endif

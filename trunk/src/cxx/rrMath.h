#ifndef rrMathH
#define rrMathH
#include "rrExporter.h"

//C++ unit(rrMath.h/rrMat.cpp) translated from C# file:
//http://roadrunner.svn.sourceforge.net/svnroot/roadrunner/trunk/LibRoadRunner/MathKGI.cs rev 75


//---------------------------------------------------------------------------
namespace rr //This namespace corresponds to C#'s namespace LibRoadRunner
{
// See: http://en.wikipedia.org/wiki/Mathematical_constant
const double EULER_CONSTANT_GAMMA 	= 0.57721566490153286060651209008;
const double GOLDEN_RATIO 			= 1.618033988749895;
const double LOG2E 					= 1.44269504088896340735992468100; /* log_2 (e) */
const double LOG10E 				= 0.43429448190325182765112891892; /* log_10 (e) */
const double SQRT2 					= 1.41421356237309504880168872421; /* sqrt(2) */
const double SQRT1_2 				= 0.70710678118654752440084436210; /* sqrt(1/2) */
const double SQRT3 					= 1.73205080756887729352744634151; /* sqrt(3) */
const double PI		 				= 3.14159265358979323846264338327; /* pi */
const double PI_BY_2 				= 1.57079632679489661923132169164; /* pi/2 */
const double PI_BY_4 				= 0.78539816339744830966156608458; /* pi/4 */
const double SQRTPI 				= 1.77245385090551602729816748334; /* sqrt(pi) */
const double TWO_BY_SQRTPI 			= 1.12837916709551257389615890312; /* 2/sqrt(pi) */
const double ONE_BU_PI 				= 0.31830988618379067153776752675; /* 1/pi */
const double TWO_BY_PI 				= 0.63661977236758134307553505349; /* 2/pi */
const double LN10 					= 2.30258509299404568401799145468; /* ln(10) */
const double LN2 					= 0.69314718055994530941723212146; /* ln(2) */
const double LNPI 					= 1.14472988584940017414342735135; /* ln(pi) */

// Square
RR_DECLSPEC double sqr(double a);
RR_DECLSPEC double Logbase(double value, double baseValue);

// -----------------------------------------------------------------------
// Start of trig functions
// -----------------------------------------------------------------------

// Convert degrees to Radians
RR_DECLSPEC double degToRad(double degrees);

// Convert radians to degrees
RR_DECLSPEC double radToDeg(double radians);

// Cotangent
RR_DECLSPEC double Cot(double a);

// Inverse cotangent
RR_DECLSPEC double Acot(double a);

// Inverse cotangent - ratio numerator and denominator provided
RR_DECLSPEC double Acot2(double a, double b);

// Secant
RR_DECLSPEC double Sec(double a);

// Inverse secant
RR_DECLSPEC double Asec(double a);

// Cosecant
RR_DECLSPEC double Csc(double a);

// Inverse cosecant
RR_DECLSPEC double Acsc(double a);

// Hyperbolic secant of a double number
RR_DECLSPEC double Sech(double a);

// Inverse hyperbolic secant of a double number
RR_DECLSPEC double Asech(double a);

// Hyperbolic cosecant of a double number
RR_DECLSPEC double Csch(double a);

// Inverse hyperbolic cosecant of a double number
RR_DECLSPEC double Acsch(double a);


// Hyperbolic cotangent of a double number
RR_DECLSPEC double Coth(double a);

// Inverse hyperbolic cotangent of a double number
RR_DECLSPEC double Acoth(double a);

// Inverse hyperbolic functions
// --------------------------------------------------------------
// Inverse hyperbolic sine of a double number
RR_DECLSPEC double Asinh(double a);

// Inverse hyperbolic cosine of a double number
RR_DECLSPEC double Acosh(double a);

// Inverse hyperbolic tangent of a double number
RR_DECLSPEC double Atanh(double a);

// Boolean functions for event handling" + NL());
RR_DECLSPEC double Gt(double a, double b);
RR_DECLSPEC double Lt(double a, double b);
RR_DECLSPEC double Geq(double a, double b);
RR_DECLSPEC double Leq(double a, double b);
RR_DECLSPEC double Eq(double a, double b);
RR_DECLSPEC double Neq(double a, double b);
RR_DECLSPEC double And(double first, ...);	//double args...
//RR_DECLSPEC bool And(params bool[] a);
//RR_DECLSPEC double Or(params double[] a);
//RR_DECLSPEC bool Or(params bool[] a);
RR_DECLSPEC double Not(double a);
RR_DECLSPEC bool Not(bool a);
//RR_DECLSPEC double Xor(params double[] a);
//RR_DECLSPEC bool Xor(params bool[] a);

//No references to 'double' Factorial
RR_DECLSPEC double Factorial(double a);
RR_DECLSPEC double Log(double a);
RR_DECLSPEC double Log(double a, double b);
RR_DECLSPEC double Delay(double a, double b);
RR_DECLSPEC double Root(double a, double b);
//RR_DECLSPEC double Piecewise(params object[] args);
}


//C# code
//using System;
//
//namespace LibRoadRunner
//{
//    public class MathKGI
//    {
//
//    }
//}

#endif

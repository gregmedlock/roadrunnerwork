#ifndef rrSupportFunctionsH
#define rrSupportFunctionsH
//#include "rrCExporter.h"

// See: http://en.wikipedia.org/wiki/Mathematical_constant
//const double EULER_CONSTANT_GAMMA 	= 0.57721566490153286060651209008;
//const double GOLDEN_RATIO 			= 1.618033988749895;
//const double LOG2E 					= 1.44269504088896340735992468100; /* log_2 (e) */
//const double LOG10E 				= 0.43429448190325182765112891892; /* log_10 (e) */
//const double SQRT2 					= 1.41421356237309504880168872421; /* sqrt(2) */
//const double SQRT1_2 				= 0.70710678118654752440084436210; /* sqrt(1/2) */
//const double SQRT3 					= 1.73205080756887729352744634151; /* sqrt(3) */
//const double PI		 				= 3.14159265358979323846264338327; /* pi */
//const double PI_BY_2 				= 1.57079632679489661923132169164; /* pi/2 */
//const double PI_BY_4 				= 0.78539816339744830966156608458; /* pi/4 */
//const double SQRTPI 				= 1.77245385090551602729816748334; /* sqrt(pi) */
//const double TWO_BY_SQRTPI 			= 1.12837916709551257389615890312; /* 2/sqrt(pi) */
//const double ONE_BY_PI 				= 0.31830988618379067153776752675; /* 1/pi */
//const double TWO_BY_PI 				= 0.63661977236758134307553505349; /* 2/pi */
//const double LN10 					= 2.30258509299404568401799145468; /* ln(10) */
//const double LN2 					= 0.69314718055994530941723212146; /* ln(2) */
//const double LNPI 					= 1.14472988584940017414342735135; /* ln(pi) */

// Boolean functions for event handling" + NL());
double 	spf_gt(double a, double b);
double 	spf_lt(double a, double b);
double 	spf_geq(double a, double b);
double 	spf_leq(double a, double b);
double 	spf_eq(double a, double b);
double 	spf_neq(double a, double b);
double 	spf_and(double val1, double val2);
//bool 	spf_and(params bool[] a);
//double 	spf_or(params double[] a);
//bool 	spf_or(params bool[] a);
//D_S double 	spf_not(double a);
////D_S bool 	spf_not(bool a);
////D_S double 	spf_xor(params double[] a);
////D_S bool 	spf_xor(params bool[] a);
int 	spf_factorial(int a);
//D_S double 	spf_log(double a);
double 	spf_log(double a, double b);
double 	spf_delay(double a, double b);
double 	spf_root(double a, double b);
//double 	spf_piecewise(params object[] args);
double 	spf_piecewise(double val1, double val2, double val3);


// Square
double spf_sqr(double a);
double Logbase(double value, double baseValue);

//// -----------------------------------------------------------------------
//// Start of trig functions
//// -----------------------------------------------------------------------
//
//// Convert degrees to Radians
double degToRad(double degrees);
//

//// Convert radians to degrees
double radToDeg(double radians);

//// Cotangent
//D_S double Cot(double a);
//
//// Inverse cotangent
//D_S double Acot(double a);
//
//// Inverse cotangent - ratio numerator and denominator provided
//D_S double Acot2(double a, double b);
//
//// Secant
//D_S double Sec(double a);
//
//// Inverse secant
//D_S double Asec(double a);
//
//// Cosecant
//D_S double Csc(double a);
//
//// Inverse cosecant
//D_S double Acsc(double a);
//
//// Hyperbolic secant of a double number
//D_S double Sech(double a);
//
//// Inverse hyperbolic secant of a double number
//D_S double Asech(double a);
//
//// Hyperbolic cosecant of a double number
//D_S double Csch(double a);
//
//// Inverse hyperbolic cosecant of a double number
//D_S double Acsch(double a);
//
//
//// Hyperbolic cotangent of a double number
//D_S double Coth(double a);
//
//// Inverse hyperbolic cotangent of a double number
//D_S double Acoth(double a);
//
//// Inverse hyperbolic functions
//// --------------------------------------------------------------
//// Inverse hyperbolic sine of a double number
//D_S double Asinh(double a);
//
//// Inverse hyperbolic cosine of a double number
//D_S double Acosh(double a);
//
//// Inverse hyperbolic tangent of a double number
//D_S double Atanh(double a);
//
//// Boolean functions for event handling" + NL());
//D_S double Gt(double a, double b);
//D_S double Lt(double a, double b);
//D_S double Geq(double a, double b);
//D_S double Leq(double a, double b);
//D_S double Eq(double a, double b);
//D_S double Neq(double a, double b);
//D_S double And(double first, ...);	//double args...
////D_S bool And(params bool[] a);
////D_S double Or(params double[] a);
////D_S bool Or(params bool[] a);
//D_S double Not(double a);
////D_S bool Not(bool a);
////D_S double Xor(params double[] a);
////D_S bool Xor(params bool[]  a);
//
////No references to 'double' Factorial
//D_S double Factorial(double a);
////D_S double log(double a);
////D_S double log(double a, double b);
//D_S double Delay(double a, double b);
//D_S double Root(double a, double b);
////D_S double Piecewise(params object[] args);
//

#endif


//using System;
//
//namespace LibRoadRunner
//{
//    public class supportFunctions
//    {
//        // Boolean functions for event handling" + NL());
//        public static double _gt(double a, double b)
//        {
//            return (a > b ? 1.0 : 0.0);
//        }
//
//        public static double _lt(double a, double b)
//        {
//            return (a < b ? 1.0 : 0.0);
//        }
//
//        public static double _geq(double a, double b)
//        {
//            return (a >= b ? 1.0 : 0.0);
//        }
//
//        public static double _leq(double a, double b)
//        {
//            return (a <= b ? 1.0 : 0.0);
//        }
//
//        public static double _eq(double a, double b)
//        {
//            return (a == b ? 1.0 : 0.0);
//        }
//
//        public static double _neq(double a, double b)
//        {
//            return (a != b ? 1.0 : 0.0);
//        }
//
//        public static double _and(params double[] a)
//        {
//            foreach (double b in a)
//                if (b != 1.0) return 0.0;
//            return 1.0;
//        }
//
//        public static bool _and(params bool[] a)
//        {
//            bool result = true;
//            foreach (bool b in a)
//                result &= b;
//            return result;
//        }
//
//        public static double _or(params double[] a)
//        {
//            foreach (double b in a)
//                if (b == 1.0) return 1.0;
//            return 0.0;
//        }
//
//        public static bool _or(params bool[] a)
//        {
//            bool result = false;
//            foreach (bool b in a)
//                result |= b;
//            return result;
//        }
//
//        public static double _not(double a)
//        {
//            return (a == 1.0 ? 0.0 : 1.0);
//        }
//
//        public static bool _not(bool a)
//        {
//            return ! a;
//        }
//
//        public static double _xor(params double[] a)
//        {
//            bool result = false;
//            for (int i = 0; i < a.Length; i++)
//            {
//                if (i == 0)
//                {
//                    result = (a[i] == 1.0);
//                }
//                else
//                {
//                    result ^= (a[i] == 1.0);
//                }
//            }
//            return (result ? 1.0 : 0.0);
//        }
//
//        public static bool _xor(params bool[] a)
//        {
//            bool result = false;
//            for (int i = 0; i < a.Length; i++)
//            {
//                if (i == 0)
//                {
//                    result = a[i];
//                }
//                else
//                {
//                    result ^= a[i];
//                }
//            }
//            return result;
//        }
//
//        public static double _factorial(double a)
//        {
//            var b = (int) a;
//            if (b < 2)
//                return 1.0;
//            return b*_factorial(b - 1);
//        }
//
//        public static double _log(double a)
//        {
//            return Math.Log(a);
//        }
//
//        public static double _log(double a, double b)
//        {
//            return Math.Log(b, a);
//        }
//
//        public static double _delay(double a, double b)
//        {
//            return a;
//        }
//
//        public static double _root(double a, double b)
//        {
//            try
//            {
//                return Math.Pow(b, 1.0/a);
//            }
//            catch
//            {
//                return 1.0;
//            }
//        }
//
//        public static double _piecewise(params object[] args)
//        {
//            try
//            {
//                for (int i = 0; i < args.Length - 1; i = i + 2)
//                {
//                    //bool bTest = (bool) args[i+1];
//                    try
//                    {
//                        var bTest = (double) args[i + 1];
//                        if (bTest == 1.0)
//                            return (double) args[i];
//                    }
//                    catch
//                    {
//                        var bTest = (bool) args[i + 1];
//                        if (bTest) return (double) args[i];
//                    }
//                }
//                return (double) args[args.Length - 1];
//            }
//            catch (Exception)
//            {
//                throw new ArgumentException("Invalid arguments for piecewise.");
//            }
//        }
//    }
//}
//


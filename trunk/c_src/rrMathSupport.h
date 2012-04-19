#ifndef rrMathSupportH
#define rrMathSupportH
#include "rrExporter.h"

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
const double ONE_BY_PI 				= 0.31830988618379067153776752675; /* 1/pi */
const double TWO_BY_PI 				= 0.63661977236758134307553505349; /* 2/pi */
const double LN10 					= 2.30258509299404568401799145468; /* ln(10) */
const double LN2 					= 0.69314718055994530941723212146; /* ln(2) */
const double LNPI 					= 1.14472988584940017414342735135; /* ln(pi) */

// Square
D_S double sqr(double a);
D_S double Logbase(double value, double baseValue);

// -----------------------------------------------------------------------
// Start of trig functions
// -----------------------------------------------------------------------

// Convert degrees to Radians
D_S double degToRad(double degrees);

// Convert radians to degrees
D_S double radToDeg(double radians);

// Cotangent
D_S double Cot(double a);

// Inverse cotangent
D_S double Acot(double a);

// Inverse cotangent - ratio numerator and denominator provided
D_S double Acot2(double a, double b);

// Secant
D_S double Sec(double a);

// Inverse secant
D_S double Asec(double a);

// Cosecant
D_S double Csc(double a);

// Inverse cosecant
D_S double Acsc(double a);

// Hyperbolic secant of a double number
D_S double Sech(double a);

// Inverse hyperbolic secant of a double number
D_S double Asech(double a);

// Hyperbolic cosecant of a double number
D_S double Csch(double a);

// Inverse hyperbolic cosecant of a double number
D_S double Acsch(double a);


// Hyperbolic cotangent of a double number
D_S double Coth(double a);

// Inverse hyperbolic cotangent of a double number
D_S double Acoth(double a);

// Inverse hyperbolic functions
// --------------------------------------------------------------
// Inverse hyperbolic sine of a double number
D_S double Asinh(double a);

// Inverse hyperbolic cosine of a double number
D_S double Acosh(double a);

// Inverse hyperbolic tangent of a double number
D_S double Atanh(double a);

// Boolean functions for event handling" + NL());
D_S double Gt(double a, double b);
D_S double Lt(double a, double b);
D_S double Geq(double a, double b);
D_S double Leq(double a, double b);
D_S double Eq(double a, double b);
D_S double Neq(double a, double b);
D_S double And(double first, ...);	//double args...
//D_S bool And(params bool[] a);
//D_S double Or(params double[] a);
//D_S bool Or(params bool[] a);
D_S double Not(double a);
//D_S bool Not(bool a);
//D_S double Xor(params double[] a);
//D_S bool Xor(params bool[]  a);

//No references to 'double' Factorial
D_S double Factorial(double a);
//D_S double log(double a);
D_S double log(double a, double b);
D_S double Delay(double a, double b);
D_S double Root(double a, double b);
//D_S double Piecewise(params object[] args);

#endif


//c#
//using System;
//
//namespace LibRoadRunner
//{
//    public class MathKGI
//    {
//        // See: http://en.wikipedia.org/wiki/Mathematical_constant
//        public static double EULER_CONSTANT_GAMMA = 0.57721566490153286060651209008;
//        public static double GOLDEN_RATIO = 1.618033988749895;
//        public static double LOG2E = 1.44269504088896340735992468100; /* log_2 (e) */
//        public static double LOG10E = 0.43429448190325182765112891892; /* log_10 (e) */
//        public static double SQRT2 = 1.41421356237309504880168872421; /* sqrt(2) */
//        public static double SQRT1_2 = 0.70710678118654752440084436210; /* sqrt(1/2) */
//        public static double SQRT3 = 1.73205080756887729352744634151; /* sqrt(3) */
//        public static double PI_BY_2 = 1.57079632679489661923132169164; /* pi/2 */
//        public static double PI_BY_4 = 0.78539816339744830966156608458; /* pi/4 */
//        public static double SQRTPI = 1.77245385090551602729816748334; /* sqrt(pi) */
//        public static double TWO_BY_SQRTPI = 1.12837916709551257389615890312; /* 2/sqrt(pi) */
//        public static double ONE_BU_PI = 0.31830988618379067153776752675; /* 1/pi */
//        public static double TWO_BY_PI = 0.63661977236758134307553505349; /* 2/pi */
//        public static double LN10 = 2.30258509299404568401799145468; /* ln(10) */
//        public static double LN2 = 0.69314718055994530941723212146; /* ln(2) */
//        public static double LNPI = 1.14472988584940017414342735135; /* ln(pi) */
//
//        // Square
//        public static double sqr(double a)
//        {
//            return a*a;
//        }
//
//        public static double Logbase(double value, double baseValue)
//        {
//            return Math.Log(value, baseValue);
//        }
//
//        // -----------------------------------------------------------------------
//        // Start of trig functions
//        // -----------------------------------------------------------------------
//
//        // Convert degrees to Radians
//        public static double degToRad(double degrees)
//        {
//            return degrees*(Math.PI/180);
//        }
//
//        // Convert radians to degrees
//        public static double radToDeg(double radians)
//        {
//            return radians*(180/Math.PI);
//        }
//
//
//        // Cotangent
//        public static double Cot(double a)
//        {
//            return 1.0D/Math.Tan(a);
//        }
//
//        // Inverse cotangent
//        public static double Acot(double a)
//        {
//            return Math.Atan(1.0/a);
//        }
//
//        // Inverse cotangent - ratio numerator and denominator provided
//        public static double Acot2(double a, double b)
//        {
//            return Math.Atan2(b, a);
//        }
//
//        // Secant
//        public static double Sec(double a)
//        {
//            return 1.0/Math.Cos(a);
//        }
//
//        // Inverse secant
//        public static double Asec(double a)
//        {
//            if (a < 1.0D && a > -1.0D)
//                //throw new ArgumentException("asec argument (" + a + ") must be >= 1 or <= -1");
//                return double.NaN;
//            return Math.Acos(1.0D/a);
//        }
//
//        // Cosecant
//        public static double Csc(double a)
//        {
//            return 1.0D/Math.Sin(a);
//        }
//
//        // Inverse cosecant
//        public static double Acsc(double a)
//        {
//            if (a < 1.0D && a > - 1.0D)
//                //throw new ArgumentException("acsc argument (" + a + ") must be >= 1 or <= -1");
//                return double.NaN;
//            return Math.Asin(1.0D/a);
//        }
//
//        // Hyperbolic secant of a double number
//        public static double Sech(double a)
//        {
//            return 1.0D/Math.Cosh(a);
//        }
//
//        // Inverse hyperbolic secant of a double number
//        public static double Asech(double a)
//        {
//            if (a > 1.0D || a < 0.0D)
//                //throw new ArgumentException("asech real number argument (" + a + ") must be >= 0 and <= 1");
//                return double.NaN;
//            return (Math.Log(1.0D/a + Math.Sqrt(1.0D/(a*a) - 1.0D)));
//        }
//
//        // Hyperbolic cosecant of a double number
//        public static double Csch(double a)
//        {
//            return 1.0D/Math.Sinh(a);
//        }
//
//        // Inverse hyperbolic cosecant of a double number
//        public static double Acsch(double a)
//        {
//            double sgn = 1.0D;
//            if (a < 0.0D)
//            {
//                sgn = -1.0D;
//                a = -a;
//            }
//            return sgn*(Math.Log(1.0/a + Math.Sqrt(1.0D/(a*a) + 1.0D)));
//        }
//
//
//        // Hyperbolic cotangent of a double number
//        public static double Coth(double a)
//        {
//            return 1.0D/Math.Tanh(a);
//        }
//
//        // Inverse hyperbolic cotangent of a double number
//        public static double Acoth(double a)
//        {
//            double sgn = 1.0D;
//            if (a < 0.0D)
//            {
//                sgn = -1.0D;
//                a = -a;
//            }
//            if (a < 1.0D)
//                //throw new ArgumentException("acoth real number argument (" + sgn*a + ") must be <= -1 or >= 1");
//                return double.NaN;
//            return 0.5D*sgn*(Math.Log(1.0D + a) - Math.Log(a - 1.0D));
//        }
//
//
//        // Inverse hyperbolic functions
//        // --------------------------------------------------------------
//        // Inverse hyperbolic sine of a double number
//        public static double Asinh(double a)
//        {
//            double sgn = 1.0D;
//            if (a < 0.0D)
//            {
//                sgn = -1.0D;
//                a = -a;
//            }
//            return sgn*Math.Log(a + Math.Sqrt(a*a + 1.0D));
//        }
//
//        // Inverse hyperbolic cosine of a double number
//        public static double Acosh(double a)
//        {
//            if (a < 1.0D)
//                //throw new ArgumentException("acosh real number argument (" + a + ") must be >= 1");
//                return double.NaN;
//            return Math.Log(a + Math.Sqrt(a*a - 1.0D));
//        }
//
//        // Inverse hyperbolic tangent of a double number
//        public static double Atanh(double a)
//        {
//            double sgn = 1.0D;
//            if (a < 0.0D)
//            {
//                sgn = -1.0D;
//                a = -a;
//            }
//            if (a > 1.0D)
//                //throw new ArgumentException("atanh real number argument (" + sgn*a + ") must be >= -1 and <= 1");
//                return double.NaN;
//            return 0.5D*sgn*(Math.Log(1.0D + a) - Math.Log(1.0D - a));
//        }
//
//        // Boolean functions for event handling" + NL());
//        public static double Gt(double a, double b)
//        {
//            return (a > b ? 1.0 : 0.0);
//        }
//
//        public static double Lt(double a, double b)
//        {
//            return (a < b ? 1.0 : 0.0);
//        }
//
//        public static double Geq(double a, double b)
//        {
//            return (a >= b ? 1.0 : 0.0);
//        }
//
//        public static double Leq(double a, double b)
//        {
//            return (a <= b ? 1.0 : 0.0);
//        }
//
//        public static double Eq(double a, double b)
//        {
//            return (a == b ? 1.0 : 0.0);
//        }
//
//        public static double Neq(double a, double b)
//        {
//            return (a != b ? 1.0 : 0.0);
//        }
//
//        public static double And(params double[] a)
//        {
//            foreach (double b in a)
//                if (b != 1.0) return 0.0;
//            return 1.0;
//        }
//
//        public static bool And(params bool[] a)
//        {
//            bool result = true;
//            foreach (bool b in a)
//                result &= b;
//            return result;
//        }
//
//        public static double Or(params double[] a)
//        {
//            foreach (double b in a)
//                if (b == 1.0) return 1.0;
//            return 0.0;
//        }
//
//        public static bool Or(params bool[] a)
//        {
//            bool result = false;
//            foreach (bool b in a)
//                result |= b;
//            return result;
//        }
//
//        public static double Not(double a)
//        {
//            return (a == 1.0 ? 0.0 : 1.0);
//        }
//
//        public static bool Not(bool a)
//        {
//            return ! a;
//        }
//
//        public static double Xor(params double[] a)
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
//        public static bool Xor(params bool[] a)
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
//        public static double Factorial(double a)
//        {
//            var b = (int) a;
//            if (b < 2)
//                return 1.0;
//            return b*Factorial(b - 1);
//        }
//
//        public static double Log(double a)
//        {
//            return Math.Log(a);
//        }
//
//        public static double Log(double a, double b)
//        {
//            return Math.Log(b, a);
//        }
//
//        public static double Delay(double a, double b)
//        {
//            return a;
//        }
//
//        public static double Root(double a, double b)
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
//        public static double Piecewise(params object[] args)
//        {
//            var array = args;
//            if (args.Length == 1 && args[0].GetType() == typeof(double[]))
//            {
//                var doubleArray = (double[]) args[0];
//                array = new object[doubleArray.Length];
//                for (int i = 0; i < doubleArray.Length; i++)
//                {
//                    array[i] = doubleArray[i];
//                }
//
//            }
//            try
//            {
//                for (int i = 0; i < array.Length - 1; i = i + 2)
//                {
//                    //bool bTest = (bool) args[i+1];
//                    try
//                    {
//                        var bTest = (double) array[i + 1];
//                        if (bTest == 1.0)
//                            return (double) array[i];
//                    }
//                    catch
//                    {
//                        var bTest = (bool) array[i + 1];
//                        if (bTest) return (double) array[i];
//                    }
//                }
//                return (double) array[array.Length - 1];
//            }
//            catch (Exception)
//            {
//                throw new ArgumentException("Invalid arguments for piecewise.");
//            }
//        }
//    }
//}

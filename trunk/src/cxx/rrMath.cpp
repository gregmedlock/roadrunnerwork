//---------------------------------------------------------------------------


#pragma hdrstop
#include <math.h>
#include "rrMath.h"
//---------------------------------------------------------------------------
namespace rr
{
        // Square
        double sqr(double a)
        {
            return a*a;
        }

        double Logbase(double value, double baseValue)
        {
            return rr::Log(value, baseValue);
        }

        // -----------------------------------------------------------------------
        // Start of trig functions
        // -----------------------------------------------------------------------

        // Convert degrees to Radians
        double degToRad(double degrees)
        {
            return degrees*(rr::PI/180);
        }

        // Convert radians to degrees
        double radToDeg(double radians)
        {
            return radians*(180/rr::PI);
        }


        // Cotangent
        double Cot(double a)
        {
            return 1.0/tan(a);
        }

        // Inverse cotangent
        double Acot(double a)
        {
            return atan(1.0/a);
        }

        // Inverse cotangent - ratio numerator and denominator provided
        double Acot2(double a, double b)
        {
            return atan2(b, a);
        }

        // Secant
        double Sec(double a)
        {
            return cos(a);
        }

        // Inverse secant
        double Asec(double a)
        {
            //if (a < 1.0 && a > -1.0)
                //throw new ArgumentException("asec argument (" + a + ") must be >= 1 or <= -1");
			//    return NaN;
            return acos(1.0/a);
        }

        // Cosecant
        double Csc(double a)
        {
            return 1.0/sin(a);
        }

        // Inverse cosecant
        double Acsc(double a)
        {
            //if (a < 1.0 && a > - 1.0)
                //throw new ArgumentException("acsc argument (" + a + ") must be >= 1 or <= -1");
            //    return double.NaN;
            return asin(1.0/a);
        }

        // Hyperbolic secant of a double number
        double Sech(double a)
        {
            return 1.0/cosh(a);
        }

        // Inverse hyperbolic secant of a double number
        double Asech(double a)
        {
//            if (a > 1.0 || a < 0.0)
//                //throw new ArgumentException("asech real number argument (" + a + ") must be >= 0 and <= 1");
//                return double.NaN;
            return (Log(1.0/a + sqrt(1.0/(a*a) - 1.0)));
        }

        // Hyperbolic cosecant of a double number
        double Csch(double a)
        {
            return 1.0/sinh(a);
        }

        // Inverse hyperbolic cosecant of a double number
        double Acsch(double a)
        {
            double sgn = 1.0;
            if (a < 0.0)
            {
                sgn = -1.0;
                a = -a;
            }
            return sgn*(Log(1.0/a + sqrt(1.0/(a*a) + 1.0)));
        }


        // Hyperbolic cotangent of a double number
        double Coth(double a)
        {
            return 1.0/tanh(a);
        }

        // Inverse hyperbolic cotangent of a double number
        double Acoth(double a)
        {
            double sgn = 1.0;
            if (a < 0.0)
            {
                sgn = -1.0;
                a = -a;
            }
//            if (a < 1.0)
//                //throw new ArgumentException("acoth real number argument (" + sgn*a + ") must be <= -1 or >= 1");
//                return double.NaN;
            return 0.5*sgn*(Log(1.0 + a) - Log(a - 1.0));
        }


        // Inverse hyperbolic functions
        // --------------------------------------------------------------
        // Inverse hyperbolic sine of a double number
        double Asinh(double a)
        {
            double sgn = 1.0;
            if (a < 0.0)
            {
                sgn = -1.0;
                a = -a;
            }
            return sgn*Log(a + sqrt(a*a + 1.0));
        }

        // Inverse hyperbolic cosine of a double number
        double Acosh(double a)
        {
//            if (a < 1.0)
//                //throw new ArgumentException("acosh real number argument (" + a + ") must be >= 1");
//                return double.NaN;
            return Log(a + sqrt(a*a - 1.0));
        }

        // Inverse hyperbolic tangent of a double number
        double Atanh(double a)
        {
            double sgn = 1.0;
            if (a < 0.0)
            {
                sgn = -1.0;
                a = -a;
            }
//            if (a > 1.0)
//                //throw new ArgumentException("atanh real number argument (" + sgn*a + ") must be >= -1 and <= 1");
//                return double.NaN;
            return 0.5*sgn*(Log(1.0 + a) - Log(1.0 - a));
        }

        // Boolean functions for event handling" + NL());
        double Gt(double a, double b)
        {
            return (a > b ? 1.0 : 0.0);
        }

        double Lt(double a, double b)
        {
            return (a < b ? 1.0 : 0.0);
        }

        double Geq(double a, double b)
        {
            return (a >= b ? 1.0 : 0.0);
        }

        double Leq(double a, double b)
        {
            return (a <= b ? 1.0 : 0.0);
        }

        double Eq(double a, double b)
        {
            return (a == b ? 1.0 : 0.0);
        }

        double Neq(double a, double b)
        {
            return (a != b ? 1.0 : 0.0);
        }

//        double And(params double[] a)
//        {
//            foreach (double b in a)
//                if (b != 1.0) return 0.0;
//            return 1.0;
//        }

//        bool And(params bool[] a)
//        {
//            bool result = true;
//            foreach (bool b in a)
//                result &= b;
//            return result;
//        }

//        double Or(params double[] a)
//        {
//            foreach (double b in a)
//                if (b == 1.0) return 1.0;
//            return 0.0;
//        }

//        bool Or(params bool[] a)
//        {
//            bool result = false;
//            foreach (bool b in a)
//                result |= b;
//            return result;
//        }

        double Not(double a)
        {
            return (a == 1.0 ? 0.0 : 1.0);
        }

        bool Not(bool a)
        {
            return ! a;
        }

//        double Xor(params double[] a)
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
//        bool Xor(params bool[] a)
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

//        double Factorial(double a)
//        {
//            var b = (int) a;
//            if (b < 2)
//                return 1.0;
//            return b*Factorial(b - 1);
//        }

        double Log(double a)
        {
            return log(a);
        }

        double Log(double a, double b)
        {
//            return Math.Log(b, a);
			return log(a) / log(b);
        }

        double Delay(double a, double b)
        {
            return a;
        }

        double Root(double a, double b)
        {
            try
            {
                return pow(b, 1.0/a);
            }
            catch(...)
            {
                return 1.0;
            };
        }

//        double Piecewise(params object[] args)
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



}
#pragma package(smart_init)

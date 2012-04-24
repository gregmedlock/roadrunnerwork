#include <stdarg.h>				//va_list etc..
#include <stdbool.h>
#include "rrSupportFunctions.h"

//---------------------------------------------------------------------------
// Boolean functions for event handling" + NL());
double spf_gt(double a, double b)
{
    return (a > b ? 1.0 : 0.0);
}

double spf_lt(double a, double b)
{
    return (a < b ? 1.0 : 0.0);
}

double spf_geq(double a, double b)
{
    return (a >= b ? 1.0 : 0.0);
}

double spf_leq(double a, double b)
{
    return (a <= b ? 1.0 : 0.0);
}

double spf_eq(double a, double b)
{
    return (a == b ? 1.0 : 0.0);
}

double spf_neq(double a, double b)
{
    return (a != b ? 1.0 : 0.0);
}

double spf_and(int nrOfArguments, ...)
{
	va_list listPointer;

    // Currently, listPointer is UNINITIALIZED, however,
    // make listPointer point to the first argument in the list
    va_start(listPointer, nrOfArguments);

	double result = 1.0;

    int i;	//This is C!
    for(i = 0; i < nrOfArguments; i++)
    {
        // Get an argument.  Must know
        // the type of the arg to retrieve
        // it from the va_list.
      double arg = va_arg( listPointer, double);

		printf( "    The %dth arg is %f\n", i, arg );
      	if(arg != 1.0)
      	{
      		result = 0.0;
          	break;
      	}
	}
	va_end( listPointer );
    return result;
}

double spf_or(int nrOfArguments, ...)
{
	va_list listPointer;
    va_start(listPointer, nrOfArguments);

	double result = 0.0;

    int i;	//This is C..
    for(i = 0; i < nrOfArguments; i++)
    {
        // Get an argument.  Must know
        // the type of the arg to retrieve
        // it from the va_list.
      double arg = va_arg( listPointer, double);

		printf( "    The %dth arg is %f\n", i, arg );
      	if(arg == 1.0)
      	{
      		result = 1.0;
          	break;
      	}
	}
	va_end( listPointer );
    return result;
}

//        bool _xor(params bool[] a)
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

double spf_xor(int nrOfArguments, ...)
{
	va_list listPointer;
    va_start(listPointer, nrOfArguments);

	_Bool result = false;

    int i;	//This is C..
    for(i = 0; i < nrOfArguments; i++)
    {
        // Get an argument.  Must know
        // the type of the arg to retrieve
        // it from the va_list.
    	double arg = va_arg( listPointer, double);
		printf( "    The %dth arg is %f\n", i, arg );

      	if(i == 0)
      	{
      		result = (arg == 1.0);
        }
        else
        {
        	result ^= (arg == 1.0);
        }
	}
	va_end( listPointer );
    return (result ? 1.0 : 0.0);
}

//double spf_and(double val1, double val2)
//{
////    foreach (double b in a)
////        if (b != 1.0) return 0.0;
////    return 1.0;
//}

//        bool _and(params bool[] a)
//        {
//            bool result = true;
//            foreach (bool b in a)
//                result &= b;
//            return result;
//        }

//        double _or(params double[] a)
//        {
//            foreach (double b in a)
//                if (b == 1.0) return 1.0;
//            return 0.0;
//        }
//
//        bool _or(params bool[] a)
//        {
//            bool result = false;
//            foreach (bool b in a)
//                result |= b;
//            return result;
//        }

double spf_not(double a)
{
    return (a == 1.0 ? 0.0 : 1.0);
}

//bool _not(bool a)
//{
//    return ! a;
//}
//        double _xor(params double[] a)
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


int spf_factorial(int a)
{
    static int b;
    b = a;
    if (b < 2)
	{
        return 1;
	}
    return b * spf_factorial(b - 1);
}

double spf_log(double a)
{
    return log(a);
}

double spf_log10(double a)
{
    return log10(a);
}

//double spf_log(double a, double b)
//{
//    return log(b, a);
//}

double spf_delay(double a, double b)
{
    return a;
}

double spf_root(double a, double b)
{
    if(a != 0)
    {
        return pow(b, 1.0/a);
    }
    else
    {
        return 1.0;
    }
}

double spf_piecewise(int nrOfArgs, ...)
{
	return -1;	//Todo: implement this one
}
//        double _piecewise(params object[] args)
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

//
double sec(double a)
{
	return sec(a);
}

//// Cotangent
double cot(double a)
{return -1;}
//
//// Inverse cotangent
double arccot(double a)
{return -1;}
//
//// Inverse cotangent - ratio numerator and denominator provided
double arccot2(double a, double b)
{return -1;}
//
//
//// Inverse secant
double asec(double a)
{return -1;}
//
//// Cosecant
double csc(double a)
{return -1;}
//
//// Inverse cosecant
double acsc(double a)
{return -1;}
//
//// Hyperbolic secant of a double number
double sech(double a)
{return -1;}
//
//// Inverse hyperbolic secant of a double number
double asech(double a)
{return -1;}

double arcsech(double a)
{return -1;}
//
//// Hyperbolic cosecant of a double number
double csch(double a)
{return -1;}
//

//// Inverse hyperbolic cosecant of a double number
double arccsc(double a)
{
	return -1;
}

//// Inverse hyperbolic cosecant of a double number
double arccsch(double a)
{return -1;}
//
//
//// Hyperbolic cotangent of a double number
double coth(double a)
{return -1;}
//
//// Inverse hyperbolic cotangent of a double number
double arccoth(double a)
{return -1;}
//
//// Inverse hyperbolic functions
//// --------------------------------------------------------------
//// Inverse hyperbolic sine of a double number
double arcsinh(double a)
{return -1;}
//
//// Inverse hyperbolic cosine of a double number
double arccosh(double a)
{return -1;}
//
//// Inverse hyperbolic tangent of a double number
double arctanh(double a)
{return -1;}


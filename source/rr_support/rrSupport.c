#include <stdio.h>                //va_list etc..
#include <stdarg.h>                //va_list etc..
#include <stdbool.h>
#include <math.h>
#include "rrSupport.h"

#define IS_ODD(n)  ((n) & 1)
double myFastSin ( double angle );

double trunc(double d)
{
    return (d > 0) ? floor(d) : ceil(d) ;
}

double IntPower(double x, int y)
{
//The following code goes into an endless loop when y is negative
//    double value = 1.0;
//    int count = 0;
//    do
//    {
//        if(IS_ODD(exponent))
//        {
//            value *= base;
//        }
//        exponent >>= 1;
//        base *= base;
//        ++count;
//    }
//    while (exponent);
//    return (value);

    double  temp;
    if( y == 0)
    {
       return 1;
    }

    temp = IntPower(x, y/2);
    if (y%2 == 0)
    {
        return temp*temp;
    }
    else
    {
        if(y > 0)
            return x*temp*temp;
        else
            return (temp*temp)/x;
    }
}

double MyPower(double base, double exponent)
{
    double result;
    int sign = 1;
    double x = 0;
    if (base < 0)
    {
        sign = -1;
        base = fabs(base);
    }

    if (exponent == 0.0)
    {
         return (1.0*sign);              // n**0 = 1
    }

    if ((base == 0.0) && (exponent > 0.0))
    {
         return (0.0);                   // 0**n = 0, n > 0
    }

    x = exponent - trunc (exponent);

    if ((x == 0.0) && (fabs(exponent) <= 2147483647))
    {
        result = (sign*IntPower(base, trunc (exponent)));
    }
    else
    {
        result = (sign*exp(exponent * log(base)));
    }
    return result;
}

double spf_pow(double base, double exp)
{
	if(base == 0)
    {
		return 0;
    }
	else
    {
        return MyPower(base, exp);
    }
}

//---------------------------------------------------------------------------
// Boolean functions for event handling" + NL());
double spf_gt(double a, double b)
{
    return (a > b ? 1.0 : 0.0);
}

double spf_lt(double a, double b)
{
//    printf("in %f less than %f", a, b);
    return (a <= b ? 1.0 : 0.0);
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
    double result = 1.0;
    int i;
    va_list listPointer;


    // make listPointer point to the first argument in the list
    va_start(listPointer, nrOfArguments);

    for(i = 0; i < nrOfArguments; i++)
    {
        // Get an argument.  Must know the type of the arg to retrieve
        // it from the va_list.
        double arg = va_arg(listPointer, double);

        printf( "The %dth arg is %f\n", i, arg );
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
    double result = 0.0;
    int i;
    va_list listPointer;
    va_start(listPointer, nrOfArguments);

    for(i = 0; i < nrOfArguments; i++)
    {
        // Get an argument.  Must know
        // the type of the arg to retrieve
        // it from the va_list.
        double arg = va_arg( listPointer, double);

        printf("The %dth arg is %f\n", i, arg);
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
    int i;
    bool result = false;
    va_list listPointer;
    va_start(listPointer, nrOfArguments);

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


int    spf_ceil(double a)
{
    return (int) ceil(a);
}

int spf_factorial(int a)
{

    if (a <= 0 )
    {
        return 1;
    }
    return a * spf_factorial(a - 1);
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
        return spf_pow(b, 1.0/a);
    }
    else
    {
        return 1.0;
    }
}

double spf_piecewise(int nrOfArgs, ...)
{
    int i;
    double result = 0.0;
    double nextArg;
    va_list listPointer;

    // make listPointer point to the first argument in the list
    va_start(listPointer, nrOfArgs);
    for(i = 0; i < nrOfArgs - 1; i= i + 2)
    {
        // Get an argument.  Must know the type of the arg to retrieve
        // it from the va_list.
        result  = (double) va_arg(listPointer, double);
/*        printf( "The %d:th arg is %f\n", i, result );*/
        nextArg = (double) va_arg(listPointer, double);
/*        printf( "The %d:th arg is %f\n", i + 1, nextArg );*/
        if(nextArg == 1.0)
        {
            va_end( listPointer );            
/*            printf( "Returning result %f\n",  result );*/
            return result;
        }
    }
   
    //Get last argument (this works only if nrOfArgs are odd..)
    //Todo: rewrite this

    result = (double) va_arg(listPointer, double);
    va_end( listPointer );       
/*    printf( "Returning result outside loop%f\n",  result );*/

    return result;
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

double spf_sin(double a)
{
    return sin(a);
}

double sec(double a)
{
    return sec(a);
}

//// Cotangent
double cot(double a)
{
    return -1;
}
//
//// Inverse cotangent
double arccot(double a)
{
    return -1;
}

//// Inverse cotangent - ratio numerator and denominator provided
double arccot2(double a, double b)
{
    return -1;
}

//// Inverse secant
double asec(double a)
{
    return -1;
}

//// Cosecant
double csc(double a)
{
    return -1;
}

//// Inverse cosecant
double acsc(double a)
{
    return -1;
}

//// Hyperbolic secant of a double number
double sech(double a)
{
    return -1;
}

//// Inverse hyperbolic secant of a double number
double asech(double a)
{
    return -1;
}

double arcsech(double a)
{
    return -1;
}

//// Hyperbolic cosecant of a double number
double csch(double a)
{
    return -1;
}

//// Inverse hyperbolic cosecant of a double number
double arccsc(double a)
{
    return -1;
}

//// Inverse hyperbolic cosecant of a double number
double arccsch(double a)
{
    return -1;
}

//// Hyperbolic cotangent of a double number
double coth(double a)
{
    return -1;
}

//// Inverse hyperbolic cotangent of a double number
double arccoth(double a)
{
    return -1;
}

//// Inverse hyperbolic functions
//// --------------------------------------------------------------
//// Inverse hyperbolic sine of a double number
double arcsinh(double a)
{
    return -1;
}

//// Inverse hyperbolic cosine of a double number
double arccosh(double a)
{
    return -1;
}

//// Inverse hyperbolic tangent of a double number
double arctanh(double a)
{
    return -1;
}



//First of all sine and cosine tables

double sinTable[] = {
0.0,                                    //sin(0)
0.17364817766693034885171662676931 ,    //sin(10)
0.34202014332566873304409961468226 ,    //sin(20)
0.5 ,                                    //sin(30)
0.64278760968653932632264340990726 ,    //sin(40)
0.76604444311897803520239265055542 ,    //sin(50)
0.86602540378443864676372317075294 ,    //sin(60)
0.93969262078590838405410927732473 ,    //sin(70)
0.98480775301220805936674302458952 ,    //sin(80)
1.0                                     //sin(90)
};

double cosTable[] = {
1.0 ,                                    //cos(0)
0.99984769515639123915701155881391 ,    //cos(1)
0.99939082701909573000624344004393 ,    //cos(2)
0.99862953475457387378449205843944 ,    //cos(3)
0.99756405025982424761316268064426 ,    //cos(4)
0.99619469809174553229501040247389 ,    //cos(5)
0.99452189536827333692269194498057 ,    //cos(6)
0.99254615164132203498006158933058 ,    //cos(7)
0.99026806874157031508377486734485 ,    //cos(8)
0.98768834059513772619004024769344         //cos(9)
};
// sin (a+b) = sin(a)*cos(b) + sin(b)*cos(a)
// a = 10*m where m is a natural number and 0<= m <= 90
// i.e. lets a+b = 18.22
// then a = 10, b = 8.22


double myFastSin(double angle)
{
    int aVal;
    double bVal;

    aVal = angle * 0.1;
    bVal = angle - (10.0 * aVal);

//    return sinTable[aVal] * cosTable[int(bVal)] + bVal * holyConstant * sinTable[9-aVal];
    return -1;//sin(angle);//sinTable[aVal]*cosTable[(int)bVal] + bVal*holyConstant*sinTable[9-aVal];
//    return 0;
}


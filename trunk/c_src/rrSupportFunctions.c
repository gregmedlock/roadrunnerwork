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

double _geq(double a, double b)
{
    return (a >= b ? 1.0 : 0.0);
}

double _leq(double a, double b)
{
    return (a <= b ? 1.0 : 0.0);
}

double _eq(double a, double b)
{
    return (a == b ? 1.0 : 0.0);
}

double _neq(double a, double b)
{
    return (a != b ? 1.0 : 0.0);
}

//        double _and(params double[] a)
//        {
//            foreach (double b in a)
//                if (b != 1.0) return 0.0;
//            return 1.0;
//        }

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

double _not(double a)
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
//
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
//
int spf_factorial(int a)
{
//    static int b = a;
//    if (b < 2)
//	{
//        return 1;
//	}
//    return b * spf_factorial(b - 1);
}

//double _log(double a)
//{
//    return log(a);
//}

double spf_log(double a, double b)
{
    return log(b, a);
}

double _delay(double a, double b)
{
    return a;
}

double _root(double a, double b)
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


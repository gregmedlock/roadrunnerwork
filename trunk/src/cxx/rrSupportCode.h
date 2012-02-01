#ifndef rrSupportCodeH
#define rrSupportCodeH
#include <math.h>
#include "rrMath.h"
#include "rrExporter.h"

namespace rr
{
// Boolean functions for event handling" + NL());
RR_DECLSPEC double 	_gt(double a, double b);
RR_DECLSPEC double 	_lt(double a, double b);
RR_DECLSPEC double 	_geq(double a, double b);
RR_DECLSPEC double 	_leq(double a, double b);
RR_DECLSPEC double 	_eq(double a, double b);
RR_DECLSPEC double 	_neq(double a, double b);
//RR_DECLSPEC double 	_and(params double[] a);
//RR_DECLSPEC bool 	_and(params bool[] a);
//RR_DECLSPEC double 	_or(params double[] a);
//RR_DECLSPEC bool 	_or(params bool[] a);
RR_DECLSPEC double 	_not(double a);
RR_DECLSPEC bool 	_not(bool a);
//RR_DECLSPEC double 	_xor(params double[] a);
//RR_DECLSPEC bool 	_xor(params bool[] a);
RR_DECLSPEC int 	_factorial(const int& a);
RR_DECLSPEC double 	_log(double a);
RR_DECLSPEC double 	_log(double a, double b);
RR_DECLSPEC double 	_delay(double a, double b);
RR_DECLSPEC double 	_root(double a, double b);
//RR_DECLSPEC double 	_piecewise(params object[] args);
}	//namespace rr

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


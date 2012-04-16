#ifndef rrCompilerH
#define rrCompilerH
#include <windows.h>
#include <vector>
#include <string>

#include "rrObject.h"
#include "rrStringList.h"

using std::vector;
using std::string;

namespace rr
{
    /// <summary>
    /// the Compile class was written out of the idea to generate wrapper
    /// classes in memory at runtime and then compile them ...
    /// </summary>
HINSTANCE LoadDLL(const string& dll);

class RR_DECLSPEC Compiler : public rrObject
{
	private:
//        static vector<string> 	m_oAssemblies;
//        static vector<string> 	m_sCompileErrors;

//        vector<string> 			m_oProxies;
//     	void 					Compile(const string& source);
//     	rrObject* 				Compile2(const string& source, const string& sClassName);
        HINSTANCE 				mDLLHandle;
        string					mDLLFileName;	//With path

    public:
        /// <summary>
        /// the execute method takes a stringcollection of wrapper classes,
        /// compiles them and executes methods on the classes
        /// </summary>
        /// <param name="source"></param>
//		void 					Execute(const string& source);
        						Compiler();
        					   ~Compiler();
		bool					CreateDLL(const string& cmdLine);
		string					GetDLLName(){return mDLLFileName;}

        /// <summary>
        /// the execute method takes a stringcollection of wrapper classes,
        /// compiles them and executes methods on the classes
        /// </summary>
        /// <param name="oProxyCode"></param>
        void 					Execute(StringList& oProxyCode);
        static rrObject 		getInstance(const string& source, const string& sClassName);
        static rrObject* 		getInstance(const string& source, const string& sClassName, const string& sLocation);
        bool					CompileC_DLL(const string& cSource);

        /// <summary>
        /// adds an assembly to the assembly list ... this list will be needed
        /// to add references to that assemblies for the newly compiled class
        /// </summary>
        /// <param name="sAssembly"></param>
//        static void 			addAssembly(const string& sAssembly);

        /// <summary>
        /// sets the list of proxies. This is a list of strings representing Namespace
        /// and classname of the newly generated classes ... this will be used to create
        /// instances later on
        /// </summary>
        /// <param name="oProxies"></param>
  //      void addProxy(StringList oProxies);

    	static string getLastErrors();

}; //class compiler

} //namespace rr


#endif

////#region Using directives
////
////using System;
////using System.CodeDom.Compiler;
////using System.Collections.Specialized;
////using System.Diagnostics;
////using System.Reflection;
////using System.Text;
////using Microsoft.CSharp;
////
////#endregion
////
////namespace LibRoadRunner.Util
////{
////    /// <summary>
////    /// the Compile class was written out of the idea to generate wrapper
////    /// classes in memory at runtime and then compile them ...
////    /// </summary>
////    public class Compiler
////    {
////        private static readonly StringCollection m_oAssemblies = new StringCollection();
////        private static readonly StringCollection m_sCompileErrors = new StringCollection();
////        private StringCollection m_oProxies = new StringCollection();
////
////        /// <summary>
////        /// the execute method takes a stringcollection of wrapper classes,
////        /// compiles them and executes methods on the classes
////        /// </summary>
////        /// <param name="source"></param>
////        public void Execute(string source)
////        {
////            var cscp = new CSharpCodeProvider();
////            Compile(cscp, source);
////        }
////
////        /// <summary>
////        /// the execute method takes a stringcollection of wrapper classes,
////        /// compiles them and executes methods on the classes
////        /// </summary>
////        /// <param name="oProxyCode"></param>
////        public void Execute(StringCollection oProxyCode)
////        {
////            var cscp = new CSharpCodeProvider();
////            foreach (string source in oProxyCode)
////                Compile(cscp, source);
////        }
////
////
////        /// <summary>
////        /// the execute method takes a stringcollection of wrapper classes,
////        /// compiles them and executes methods on the classes
////        /// </summary>
////        /// <param name="oProxyCode"></param>
////        public static object getInstance(string source, string sClassName)
////        {
////            var oCompler = new Compiler();
////            var cscp = new CSharpCodeProvider();
////            return oCompler.Compile2(cscp, source, sClassName);
////        }
////
////        /// <summary>
////        /// the execute method takes a stringcollection of wrapper classes,
////        /// compiles them and executes methods on the classes
////        /// </summary>
////        /// <param name="oProxyCode"></param>
////        public static object getInstance(string source, string sClassName, string sLocation)
////        {
////            var oCompler = new Compiler();
////            addAssembly(sLocation);
////            var cscp = new CSharpCodeProvider();
////            return oCompler.Compile2(cscp, source, sClassName);
////        }
////
////
////        /// <summary>
////        /// adds an assembly to the assembly list ... this list will be needed
////        /// to add references to that assemblies for the newly compiled class
////        /// </summary>
////        /// <param name="sAssembly"></param>
////        public static void addAssembly(string sAssembly)
////        {
////            m_oAssemblies.Add(sAssembly);
////        }
////
////        /// <summary>
////        /// sets the list of proxies. This is a list of strings representing Namespace
////        /// and classname of the newly generated classes ... this will be used to create
////        /// instances later on
////        /// </summary>
////        /// <param name="oProxies"></param>
////        public void addProxy(StringCollection oProxies)
////        {
////            m_oProxies = oProxies;
////        }
////
////        private void Compile(CodeDomProvider provider, string source)
////        {
////            var param = new CompilerParameters();
////            param.GenerateExecutable = false;
////            param.IncludeDebugInformation = false;
////            param.GenerateInMemory = true;
////            param.ReferencedAssemblies.Add("SBW.dll");
////            foreach (string s in m_oAssemblies)
////                param.ReferencedAssemblies.Add(s);
////
////            //ICodeCompiler cc = provider.CreateCompiler();
////            CompilerResults cr = provider.CompileAssemblyFromSource(param, source);
////            StringCollection output = cr.Output;
////            if (cr.Errors.Count != 0)
////            {
////                Debug.WriteLine("Error invoking registerMethods.");
////                CompilerErrorCollection es = cr.Errors;
////                foreach (CompilerError s in es)
////                    Debug.WriteLine(s.ErrorText);
////            }
////            else
////            {
////                foreach (string sProxy in m_oProxies)
////                {
////                    object o = cr.CompiledAssembly.CreateInstance(sProxy);
////                    if (o != null)
////                    {
////                        Type type = o.GetType();
////                        type.InvokeMember("registerMethods",
////                                          BindingFlags.InvokeMethod |
////                                          BindingFlags.Default, null, o, null);
////                    }
////                    else
////                    {
////                        Debug.WriteLine("couldn't register services of proxy '" + sProxy + "'");
////                    }
////                }
////            }
////        }
////
////        public static string getLastErrors()
////        {
////            var oBuilder = new StringBuilder();
////            foreach (string s in m_sCompileErrors)
////                oBuilder.Append(s + Environment.NewLine);
////            return oBuilder.ToString();
////        }
////
////        private object Compile2(CodeDomProvider provider, string source, string sClassName)
////        {
////            m_sCompileErrors.Clear();
////            var param = new CompilerParameters();
////            param.GenerateExecutable = false;
////            param.IncludeDebugInformation = false;
////            param.GenerateInMemory = true;
////            foreach (string s in m_oAssemblies)
////                param.ReferencedAssemblies.Add(s);
////
////            CompilerResults cr = provider.CompileAssemblyFromSource(param, source);
////            StringCollection output = cr.Output;
////
////            try
////            {
////                object o = cr.CompiledAssembly.CreateInstance(sClassName);
////                if (o != null)
////                {
////                    return o;
////                }
////                else
////                {
////                    Debug.WriteLine("Couldn't create instance: '" + sClassName + "'");
////                    Debug.WriteLine("Error Compiling the model.");
////                    m_sCompileErrors.Add("Error Compiling the model:");
////                    CompilerErrorCollection es = cr.Errors;
////                    foreach (CompilerError s in es)
////                    {
////                        m_sCompileErrors.Add("    Error at Line,Col: " + s.Line + "," + s.Column + " error number: " +
////                                             s.ErrorNumber + " " + s.ErrorText);
////                        Debug.WriteLine(s.ErrorText);
////                    }
////                }
////            }
////            catch (Exception)
////            {
////                Debug.WriteLine("Couldn't create instance: '" + sClassName + "'");
////                Debug.WriteLine("Error Compiling the model.");
////                m_sCompileErrors.Add("Error Compiling the model:");
////                CompilerErrorCollection es = cr.Errors;
////                foreach (CompilerError s in es)
////                {
////                    m_sCompileErrors.Add("    Error at Line,Col: " + s.Line + "," + s.Column + " error number: " +
////                                         s.ErrorNumber + " " + s.ErrorText);
////                    Debug.WriteLine(s.ErrorText);
////                }
////            }
////            return null;
////        }
////    }
////}


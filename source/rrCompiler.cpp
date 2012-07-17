#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include <sstream>
#if defined(__CODEGEARC__)
#include <dir.h>
#else
#include <direct.h>
#endif
#include "rrLogger.h"
#include "rrCompiler.h"
#include "rrException.h"
#include "rrStringUtils.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------

using namespace std;
namespace rr
{

Compiler::Compiler()
:
mDLLHandle(NULL),
mCompiler("tcc"),
mSupportCodeFolder("../rr_support")
{
    Log(lDebug5)<<"In Compiler CTOR";
    mIncludePaths.push_back(".");
    mLibraryPaths.push_back(".");
    mIncludePaths.push_back("../rr_support");
    mIncludePaths.push_back("./include");
    mLibraryPaths.push_back(".");
    mLibraryPaths.push_back("./lib");
}

Compiler::~Compiler(){}

string Compiler::GetDLLName(){return mDLLFileName;}
bool Compiler::CompileC_DLL(const string& sourceFileName)
{
    //Now compile the code and load the resulting dll, and call an exported function in it...
    string dllFName(ChangeFileExtensionTo(ExtractFileName(sourceFileName), "dll"));

    mDLLFileName = JoinPath(ExtractFilePath(sourceFileName), dllFName);

    string exeCmd = CreateCompilerCommand(mCompiler, sourceFileName);

    Log(lDebug2)<<"Compiling model..";
    Log(lInfo)<<"\nExecuting: "<<exeCmd;

    if(!Compile(exeCmd))
    {
        Log(lError)<<"Creating DLL failed..";
        throw Exception("Creating DLL failed..");
    }

    //Check if the DLL exists...
    return FileExists(mDLLFileName);
}

string Compiler::CreateCompilerCommand(const string& compiler, const string& sourceFileName)
{
    stringstream exeCmd;
    if(compiler == "tcc")
    {
        //-g adds runtime debug information
        //-v is for verbose
        //-rdynamic : Export global symbols to the dynamic linker
        //-b : Generate additional support code to check memory allocations and array/pointer bounds. `-g' is implied.

        exeCmd<<"tcc -g -shared -rdynamic " \
        <<sourceFileName<<" " \
        <<JoinPath(mSupportCodeFolder, "rrSupport.c");

        if(gLog.GetLogLevel() == lDebug1)
        {
            exeCmd<<" -v";
        }

        if(gLog.GetLogLevel() == lDebug2)
        {
            exeCmd<<" -vv";
        }

        if(gLog.GetLogLevel() == lDebug3)
        {
            exeCmd<<" -vvv";
        }

        exeCmd<<" -o"<<mDLLFileName<<" -DBUILD_MODEL_DLL ";//
        //   <<" -DDEBUG_SPF "

        //Add include paths
        for(int i = 0; i < mIncludePaths.size(); i++)
        {
            exeCmd<<"-I"<<mIncludePaths[i]<<" " ;
        }

        //Add library paths
        for(int i = 0; i < mLibraryPaths.size(); i++)
        {
            exeCmd<<"-L"<<mLibraryPaths[i]<<" " ;
        }
    }
    else if(compiler == "bcc")
    {
/*    exeCmd<<"bcc32 -WD ";
//    exeCmd<<" -e"<<mDLLFileName<<" -vu +c:\\rrw\\src\\c_src\\bcc.cfg " \
//    <<sourceFileName \
//    <<" c:\\rrw\\src\\c_src\\rrSupport.c";
*/
    }
    return exeCmd.str();
}

bool Compiler::Compile(const string& cmdLine)
{
    STARTUPINFO         si;
    PROCESS_INFORMATION pi;

    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );

    if( !cmdLine.size() )
    {
        return false;
    }

    // Start the child process.
    if( !CreateProcessA(
        NULL,           // No module name (use command line)
        (char*) cmdLine.c_str(),        // Command line
        NULL,                           // Process handle not inheritable
        NULL,                           // Thread handle not inheritable
        FALSE,                          // Set handle inheritance to FALSE
        CREATE_NO_WINDOW,               // Creation flags
        NULL,                           // Use parent's environment block
        NULL,                           // Use parent's starting directory
        &si,                            // Pointer to STARTUPINFO structure
        &pi )                           // Pointer to PROCESS_INFORMATION structure
    )
    {
        Log(lError)<<"CreateProcess failed: "<<GetLastError();
        return false;
    }

    // Wait until child process exits.
    WaitForSingleObject( pi.hProcess, INFINITE );

    // Close process and thread handles.
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );
    return true;
}

//    private:
//        static readonly StringCollection m_oAssemblies = new StringCollection();
//        static readonly StringCollection m_sCompileErrors = new StringCollection();
//        StringCollection m_oProxies = new StringCollection();
//
//
//    public:
//        /// <summary>
//        /// the execute method takes a stringcollection of wrapper classes,
//        /// compiles them and executes methods on the classes
//        /// </summary>
//        /// <param name="source"></param>
//        public void Execute(string source)
//        {
//            var cscp = new CSharpCodeProvider();
//            Compile(cscp, source);
//        }
//
//        /// <summary>
//        /// the execute method takes a stringcollection of wrapper classes,
//        /// compiles them and executes methods on the classes
//        /// </summary>
//        /// <param name="oProxyCode"></param>
//        public void Execute(StringCollection oProxyCode)
//        {
//            var cscp = new CSharpCodeProvider();
//            foreach (string source in oProxyCode)
//                Compile(cscp, source);
//        }
//
//
//        /// <summary>
//        /// the execute method takes a stringcollection of wrapper classes,
//        /// compiles them and executes methods on the classes
//        /// </summary>
//        /// <param name="oProxyCode"></param>
//        public static object getInstance(string source, string sClassName)
//        {
//            var oCompler = new Compiler();
//            var cscp = new CSharpCodeProvider();
//            return oCompler.Compile2(cscp, source, sClassName);
//        }
//
/// <summary>
/// the execute method takes a stringcollection of wrapper classes,
/// compiles them and executes methods on the classes
/// </summary>
/// <param name="oProxyCode"></param>
rrObject* Compiler::getInstance(const string& source, const string& sClassName, const string& sLocation)
{
    //var oCompler = new Compiler();
    //addAssembly(sLocation);
//    CSharpCodeProvider *cscp = new CSharpCodeProvider();
//    return Compile2(cscp, source, sClassName);
    return NULL;
}
//
//
//        /// <summary>
//        /// adds an assembly to the assembly list ... this list will be needed
//        /// to add references to that assemblies for the newly compiled class
//        /// </summary>
//        /// <param name="sAssembly"></param>
//        public static void addAssembly(string sAssembly)
//        {
//            m_oAssemblies.Add(sAssembly);
//        }
//
//        /// <summary>
//        /// sets the list of proxies. This is a list of strings representing Namespace
//        /// and classname of the newly generated classes ... this will be used to create
//        /// instances later on
//        /// </summary>
//        /// <param name="oProxies"></param>
//        public void addProxy(StringCollection oProxies)
//        {
//            m_oProxies = oProxies;
//        }
//
//    private void Compile(CodeDomProvider provider, string source)
//    {
//        var param = new CompilerParameters();
//        param.GenerateExecutable = false;
//        param.IncludeDebugInformation = false;
//        param.GenerateInMemory = true;
//        param.ReferencedAssemblies.Add("SBW.dll");
//        foreach (string s in m_oAssemblies)
//            param.ReferencedAssemblies.Add(s);
//
//        //ICodeCompiler cc = provider.CreateCompiler();
//        CompilerResults cr = provider.CompileAssemblyFromSource(param, source);
//        StringCollection output = cr.Output;
//        if (cr.Errors.Count != 0)
//        {
//            Debug.WriteLine("Error invoking registerMethods.");
//            CompilerErrorCollection es = cr.Errors;
//            foreach (CompilerError s in es)
//                Debug.WriteLine(s.ErrorText);
//        }
//        else
//        {
//            foreach (string sProxy in m_oProxies)
//            {
//                object o = cr.CompiledAssembly.CreateInstance(sProxy);
//                if (o != null)
//                {
//                    Type type = o.GetType();
//                    type.InvokeMember("registerMethods",
//                                      BindingFlags.InvokeMethod |
//                                      BindingFlags.Default, null, o, null);
//                }
//                else
//                {
//                    Debug.WriteLine("couldn't register services of proxy '" + sProxy + "'");
//                }
//            }
//        }
//    }
//
//    public static string getLastErrors()
//    {
//        var oBuilder = new StringBuilder();
//        foreach (string s in m_sCompileErrors)
//            oBuilder.Append(s + Environment.NewLine);
//        return oBuilder.ToString();
//    }
//
//    private object Compile2(CodeDomProvider provider, string source, string sClassName)
//    {
//        m_sCompileErrors.Clear();
//        var param = new CompilerParameters();
//        param.GenerateExecutable = false;
//        param.IncludeDebugInformation = false;
//        param.GenerateInMemory = true;
//        foreach (string s in m_oAssemblies)
//            param.ReferencedAssemblies.Add(s);
//
//        CompilerResults cr = provider.CompileAssemblyFromSource(param, source);
//        StringCollection output = cr.Output;
//
//        try
//        {
//            object o = cr.CompiledAssembly.CreateInstance(sClassName);
//            if (o != null)
//            {
//                return o;
//            }
//            else
//            {
//                Debug.WriteLine("Couldn't create instance: '" + sClassName + "'");
//                Debug.WriteLine("Error Compiling the model.");
//                m_sCompileErrors.Add("Error Compiling the model:");
//                CompilerErrorCollection es = cr.Errors;
//                foreach (CompilerError s in es)
//                {
//                    m_sCompileErrors.Add("    Error at Line,Col: " + s.Line + "," + s.Column + " error number: " +
//                                         s.ErrorNumber + " " + s.ErrorText);
//                    Debug.WriteLine(s.ErrorText);
//                }
//            }
//        }
//        catch (Exception)
//        {
//            Debug.WriteLine("Couldn't create instance: '" + sClassName + "'");
//            Debug.WriteLine("Error Compiling the model.");
//            m_sCompileErrors.Add("Error Compiling the model:");
//            CompilerErrorCollection es = cr.Errors;
//            foreach (CompilerError s in es)
//            {
//                m_sCompileErrors.Add("    Error at Line,Col: " + s.Line + "," + s.Column + " error number: " +
//                                     s.ErrorNumber + " " + s.ErrorText);
//                Debug.WriteLine(s.ErrorText);
//            }
//        }
//        return null;
//    }

} //namespace rr


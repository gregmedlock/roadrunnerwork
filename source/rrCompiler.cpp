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

Compiler::Compiler(const string& compiler)
:
mDLLHandle(NULL),
mCompiler(compiler),
mSupportCodeFolder("../rr_support")
{
    Log(lDebug5)<<"In Compiler CTOR";
}

Compiler::~Compiler(){}

bool Compiler::SetCompiler(const string& compiler)
{
    mCompiler = compiler;
	return true;
}

string Compiler::GetDLLName()
{
    return mDLLFileName;
}

bool Compiler::CompileC_DLL(const string& sourceFileName)
{
    //Compile the code and load the resulting dll, and call an exported function in it...
    string dllFName(ChangeFileExtensionTo(ExtractFileName(sourceFileName), "dll"));

    mDLLFileName = JoinPath(ExtractFilePath(sourceFileName), dllFName);

    //Setup compiler environment
    SetupCompilerEnvironment();

    string exeCmd = CreateCompilerCommand(sourceFileName);

    //exeCmd += " > compileLog.log";
    Log(lDebug2)<<"Compiling model..";
    Log(lInfo)<<"\nExecuting: "<<exeCmd;

    if(!Compile(exeCmd))
    {
        Log(lError)<<"Creating DLL failed..";
        throw Exception("Creating Model DLL failed..");
    }

    //Check if the DLL exists...
    return FileExists(mDLLFileName);
}

bool Compiler::SetupCompilerEnvironment()
{
    mIncludePaths.clear();
    mLibraryPaths.clear();
    if(mCompiler == "tcc")
    {
        mIncludePaths.push_back(".");
        mIncludePaths.push_back("./include");
        mLibraryPaths.push_back(".");
        mLibraryPaths.push_back("./lib");

    }
    else if(mCompiler == "bcc")
    {

    }

    mIncludePaths.push_back(mSupportCodeFolder);

    return true;
}

string Compiler::CreateCompilerCommand(const string& sourceFileName)
{
    stringstream exeCmd;
    if(mCompiler == "tcc")
    {
        //-g adds runtime debug information
        //-v is for verbose
        //-rdynamic : Export global symbols to the dynamic linker
        //-b : Generate additional support code to check memory allocations and array/pointer bounds. `-g' is implied.

        exeCmd<<"tcc -g -shared -rdynamic " \
        <<sourceFileName<<" " \
        <<JoinPath(mSupportCodeFolder, "rrSupport.c");

//        if(gLog.GetLogLevel() == lDebug1)
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
    else if(mCompiler == "bcc")
    {
        exeCmd<<"bcc32 -WD ";
        exeCmd<<" -e"<<mDLLFileName<<" -vu +r:\\rrInstalls\\xe\\rr_support\\bcc.cfg " \
        <<sourceFileName \
        <<" r:\\rrInstalls\\xe\\rr_support\\rrSupport.c";

    }
    return exeCmd.str();
}

bool Compiler::Compile(const string& cmdLine)
{
    STARTUPINFO         si;
    PROCESS_INFORMATION pi;
    SECURITY_ATTRIBUTES sap,sat,sao;
    //sec attributes for the output file
    sao.nLength=sizeof(SECURITY_ATTRIBUTES);
    sao.lpSecurityDescriptor=NULL;
    sao.bInheritHandle=1;

    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );

    if( !cmdLine.size() )
    {
        return false;
    }

    //open the output file on the server's tmp folder (for that test will be on the C:/ root)
    string compilerTempFile(gLog.GetLogFileName());

    HANDLE out;
    if((out=CreateFile(     compilerTempFile.c_str(),
                            GENERIC_WRITE|GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE,
                            &sao,
                            OPEN_EXISTING,//CREATE_ALWAYS,
                            FILE_ATTRIBUTE_NORMAL,
                            NULL))==INVALID_HANDLE_VALUE)
    {
        Log(lError)<<"Failed creating logFiel for compiler output";
        return false;
    }

    SetFilePointer( out, 0, NULL, FILE_END); //set pointer position to end file

    //init the STARTUPINFO struct
    si.dwFlags=STARTF_USESTDHANDLES;
    si.hStdOutput = out;
    si.hStdError = out;

    //proc sec attributes
    sap.nLength=sizeof(SECURITY_ATTRIBUTES);
    sap.lpSecurityDescriptor=NULL;
    sap.bInheritHandle=1;

    //thread sec attributes
    sat.nLength=sizeof(SECURITY_ATTRIBUTES);
    sat.lpSecurityDescriptor=NULL;
    sat.bInheritHandle=1;

    // Start the child process.
    if( !CreateProcessA(
        NULL,                           // No module name (use command line)
        (char*) cmdLine.c_str(),        // Command line
        &sap,                           // Process handle not inheritable
        &sat,                           // Thread handle not inheritable
        TRUE,                          // Set handle inheritance
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
    WaitForSingleObject(pi.hProcess, INFINITE);

    // Close process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    CloseHandle(out);

//    //Read the log file and log it
//    vector<string> fContent = SplitString(GetFileContent(compilerTempFile.c_str()),"\n");
//    Log(lInfo)<<"Compiler output";
//    Log(lInfo)<<fContent;
    return true;
}

string getCompilerMessages()
{
    return "No messages yet";
}

} //namespace rr


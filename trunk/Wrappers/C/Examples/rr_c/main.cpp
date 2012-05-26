#pragma hdrstop
#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <tchar.h>
#include <iomanip>
#include "rrException.h"
#include "rrUtils.h"
#include "rrStringUtils.h"
#include "rrGetOptions.h"
#include "rrArgs.h"
#include "rr_c_api.h"

//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

void ProcessCommandLineArguments(int argc, char* argv[], Args& args);

int main(int argc, char * argv[])
{
    LogOutput::mLogToConsole = true;
    Args args;
    ProcessCommandLineArguments(argc, argv, args);
    try
    {

        Log(lInfo)<<"C API Build date: "<<getBuildDate();
        string settingsFile;
        vector<string> lines;
        string sbml;
        RRResultHandle result;
        stringstream ss;

        Log(lInfo)<<"======== Testing RoadRunner C API (from C++) ==================\n";
        RRHandle aHandle;
        aHandle  = getRRInstance();
        int index = 0;
        if(args.UseOSTempFolder)
        {
            args.TempDataFolder = "C:\\temp";
        }

        if(args.ModelFileName.size())
        {
            string logName = ExtractFileName(args.ModelFileName);
            logName = ChangeFileExtensionTo(logName, ".log");
            gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(JoinPath(args.TempDataFolder, logName) )));
        }
        else
        {
            gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(JoinPath(args.TempDataFolder, "RoadRunner.log") )));
        }

        Log(lShowAlways)<<"Logs are going to "<<gLog.GetLogFileName();
        Log(lShowAlways)<<"Log level is:" <<LogLevelToString(gLog.GetLogLevel());

        setTempFolder(args.TempDataFolder.c_str());

        //The following will load and compile and simulate the sbml model in the file
        if(!FileExists(args.ModelFileName))
        {
            Log(lInfo)<<"Please supply a sbml model file name, using option -m<modelfilename>";
            throw("Exit...");
        }

        lines = GetLinesInFile(args.ModelFileName);
        for(int i = 0; i < lines.size(); i++)
        {
            sbml += lines[i];
        }

        if(!loadSBML(sbml.c_str()))
        {
            char* error = getLastError();

            throw(RRException (string(error)));
        }

        //Then read settings file if it exists..
        setTimeStart(args.StartTime);
        setTimeEnd(args.EndTime);
        setNumPoints(args.Steps);
        setSelectionList(args.SelectionList.c_str());

        RRStringListHandle sl = getReactionNames();
        if(sl)
        {
            for(int i = 0; i < sl->Count; i++)
            {
                Log(lInfo)<<"Reaction "<<i<<": "<<sl->String[i];
            }
        }

        freeStringList(sl);

        RRDataMatrixHandle mat = getStoichiometryMatrix();
        if(mat)
        {
            printMatrix(mat);
        }
        result = simulate();

        if(!result)
        {
            Log(lError)<<"Failed running simulation";
            throw("Exit...");
        }

        double test = getValue("S1");

        Log(lInfo)<<"Get value gave: "<<test;

        if(!setValue("S1", 2.5))
        {
            Log(lError)<<"Failed setting value";
        }

        test = getValue("S1");
        Log(lInfo)<<"Get value (2) gave: "<<test;

        //Write to std out
        for(int i = 0; i < result->CSize; i++)
        {
            ss<<result->ColumnHeaders[i];
            if(i < result->CSize + 1)
            {
                ss<<"\t";
            }
        }
        ss<<endl;
        index = 0;

        //The data layout is simple row after row, in one single long row...
        for(int row = 0; row < result->RSize; row++)
        {
            for(int col = 0; col < result->CSize; col++)
            {
                ss<<(result->Data[index++]);
                if(col < result->CSize + 1)
                {
                    ss<<"\t";
                }
            }
            ss<<"\n";
        }
        Log(lInfo)<<ss.str();
        freeRRResult();
        deleteRRInstance(aHandle);
    }
    catch(const RRException& e)
    {
        Log(lError)<<"App did throw an exception.."<<e.what();
    }

    Log(lInfo)<<"RoadRunner is exiting...";

    if(args.Pause)
    {
        Pause();
    }
    return 0;
}

void ProcessCommandLineArguments(int argc, char* argv[], Args& args)
{
    char c;
    while ((c = GetOptions(argc, argv, ("cpuv:n:d:t:l:m:s:e:z:"))) != -1)
    {
        switch (c)
        {
            case ('v'): args.LogLevel                       = StringToLogLevel(optarg);     break;
            case ('c'): args.OnlyCompile                    = true;                         break;
            case ('p'): args.Pause                          = true;                         break;
            case ('t'): args.TempDataFolder                 = optarg;                       break;
            case ('d'): args.DataOutputFolder               = optarg;                       break;
            case ('m'): args.ModelFileName                  = optarg;                       break;
            case ('u'): args.UseOSTempFolder                = true;                         break;
            case ('l'): args.SelectionList                  = optarg;                       break;
            case ('s'): args.StartTime                      = ToDouble(optarg);             break;
            case ('e'): args.EndTime                        = ToDouble(optarg);             break;
            case ('z'): args.Steps                          = ToInt(optarg);                break;
            case ('?'):
            {
                    cout<<Usage(argv[0])<<endl;
            }
            default:
            {
                string str = argv[optind-1];
                if(str != "-?")
                {
                    cout<<"*** Illegal option:\t"<<argv[optind-1]<<" ***\n"<<endl;
                }
                exit(-1);
            }
        }
    }

    //Check arguments, and choose to bail here if something is not right...
    if(argc < 2)
    {
        cout<<Usage(argv[0])<<endl;
        exit(0);
    }

    gLog.SetCutOffLogLevel(args.LogLevel);

}




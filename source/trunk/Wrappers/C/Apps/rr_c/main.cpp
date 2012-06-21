#pragma hdrstop
#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <tchar.h>

#if defined(__CODEGEARC__)
#include <dir.h>
#else
#include <direct.h>
#endif

#include <iomanip>

#include "rrUtils.h"
#include "rrStringUtils.h"
#include "GetOptions.h"
#include "Args.h"
#include "rr_c_api.h"

//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

void ProcessCommandLineArguments(int argc, char* argv[], Args& args);

int main(int argc, char* argv[])
{
    Args args;
    ProcessCommandLineArguments(argc, argv, args);

    string settingsFile;
    stringstream ss;
    char* error;

	bool doMore = true;	//set to false to move to end

    cout<<"======== RoadRunner C API Client==================\n";
    RRHandle aHandle  = getRRInstance();

	if(!aHandle)
    {
    	doMore = false;
    }

	char* text = getBuildDate();
	if(text)
	{
		cout<<"Build date: "<<text<<endl;
		freeText(text);
	}

    if(args.UseOSTempFolder)
    {
        args.TempDataFolder = "C:\\temp";
    }

    setTempFolder(args.TempDataFolder.c_str());

    if(!FileExists(args.ModelFileName))
    {
        cerr<<"The file:"<<args.ModelFileName<<" don't exist. Please supply a sbml model file name, using option -m<modelfilename>";
        doMore = false;
    }

    //RoadRunner Flags and options
    if(doMore)
    {
	    setComputeAndAssignConservationLaws(args.ComputeAndAssignConservationLaws);
    }

    if(doMore)
    {
    	if(!loadSBML(GetFileContent(args.ModelFileName).c_str()))
	    {
    	    char* error = getLastError();
        	cerr<<error<<endl;
	        doMore = false;;
    	}
    }

    if(doMore && args.CalculateSteadyState)
    {
       	cout<<"Calculating steady state: "<<endl;
     	double ss = steadyState();
        if(ss == -1)
        {
            cerr<<"steadyState API function failed\n";
            cerr<<"API error was: "<<getLastError()<<endl;
            doMore = false;
        }
        else
        {

            //Get value for each specie?
            RRStringList* list = getSelectionList();
            for(int i = 1; i < list->Count; i++)   	//at index 0 is 'time'
            {

                double value = getValue(list->String[i]);
                if(value == -1)
                {
                    cerr<<"getValue API function failed\n";
                    cerr<<"API error was: "<<getLastError()<<endl;
                    return -1;
                }
                cout<<list->String[i]<<" steady state at "<<value<<endl;
            }
        }
    }

	if(doMore)
    {
	    setTimeStart(args.StartTime);
    	setTimeEnd(args.EndTime);
	    setNumPoints(args.Steps);
    	setSelectionList(args.SelectionList.c_str());
    }

    text = getCopyright();
    if(hasError())
    {
        char* error = getLastError();
        cout<<error<<endl;
    }

    cout<<text<<endl;

    freeRRInstance(aHandle);
    cout<<"RoadRunner is exiting...\n";

    if(args.Pause)
    {
        Pause();
    }
    return 0;
}

void ProcessCommandLineArguments(int argc, char* argv[], Args& args)
{
    char c;
    while ((c = GetOptions(argc, argv, ("cpuxyv:n:d:t:l:m:s:e:z:"))) != -1)
    {
        switch (c)
        {
            case ('v'): args.CurrentLogLevel                        = StringToLogLevel(optarg);     break;
            case ('c'): args.OnlyCompile                            = true;                         break;
            case ('p'): args.Pause                                  = true;                         break;
            case ('t'): args.TempDataFolder                         = optarg;                       break;
            case ('d'): args.DataOutputFolder                       = optarg;                       break;
            case ('m'): args.ModelFileName                          = optarg;                       break;
            case ('u'): args.UseOSTempFolder                        = true;                         break;
            case ('l'): args.SelectionList                          = optarg;                       break;
            case ('s'): args.StartTime                              = ToDouble(optarg);             break;
            case ('e'): args.EndTime                                = ToDouble(optarg);             break;
            case ('z'): args.Steps                                  = ToInt(optarg);                break;
            case ('x'): args.CalculateSteadyState                   = true;                			break;
            case ('y'): args.ComputeAndAssignConservationLaws  		= false;                			break;
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
}

#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "CommandLineParameters.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

Paras::Paras()
:
OnlyCompile(false),
Pause(false),
LogLevel(lInfo),
ModelFileName("")
{
    //Default for data output is in the same dir as where the executable is
    DataOutputFolder = ".";
    //CSIDL_MYDOCUMENTS
//    TCHAR myDocs[MAX_PATH];
//    HRESULT result = SHGetFolderPath(NULL, CSIDL_PERSONAL, NULL, SHGFP_TYPE_CURRENT, myDocs);
//
//    if(result == S_OK)
//    {
//        //Make Sure MyDocuments/RoadRunner exists..
//        TCHAR rrDocs[MAX_PATH];
//        strcpy(rrDocs, strcat(myDocs,"\\RoadRunner"));
//        if(FolderExists(rrDocs))
//        {
//            DataOutputFolder = rrDocs;
//        }
//        else
//        {
//            if(!CreateFolder(rrDocs))
//            {
//                Log(lError)<<"No data output folder..";
//                DataOutputFolder = "";
//            }
//            else
//            {
//                DataOutputFolder = rrDocs;
//            }
//        }
//
//    }

    //Default for temporary data output is the users AppData/Local/Temp Folder
    //  Gets the temp path env string (no guarantee it's a valid path).
    TCHAR lpTempPathBuffer[MAX_PATH];
    DWORD dwRetVal = GetTempPath(   MAX_PATH,
                                    lpTempPathBuffer); // buffer for path
    if (dwRetVal > MAX_PATH || (dwRetVal == 0))
    {
        Log(lError)<<"GetTempPath failed";
    }
    else
    {
        Log(lInfo)<<"Writing temporary files to: "<<string(lpTempPathBuffer);
    }
    TempDataFolder = string(lpTempPathBuffer);
}


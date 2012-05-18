#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrArgs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

Args::Args()
:
UseOSTempFolder(false),
OnlyCompile(false),
Pause(false),
LogLevel(lInfo),
ModelFileName(""),
DataOutputFolder(""),
TempDataFolder(".")
{
    //Default for data output is in the same dir as where the executable is

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


}


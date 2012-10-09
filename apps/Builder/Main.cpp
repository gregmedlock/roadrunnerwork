//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "mtkLogger.h"
#include "Main.h"
#include "mtkStringUtils.h"
#include "mtkFileUtils.h"
#include "mtkVCLUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "mtkSTDStringEdit"
#pragma link "mtkIniFileC"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
:
TRegistryForm(Owner, "RRBuilder"),
mIniParas(mIniFile->GetIniFile()),
mLogData(NULL),
mLogFileName("RRBuilder.log"),
mLogFileReader(mLogFileName, UpdateLog, &mLogData),
mSVNUpdateProcess("svn.exe"),
mSVNUpdateThread(&mSVNUpdateData, UpdateFromSVNUpdate, &mSVNUpdateProcess)
{
	mIniFile->Init();
    mIniParas.SetIniSection("GENERAL");
    mIniParas.Add( &SandBoxFolderE->SetupIni(			"SANDBOX_FOLDER"		 	, 	"/cygdrive/r/roadrunnerwork") 	);
    mIniParas.Add( &VSBuildRootFolderE->SetupIni(		"VS_BUILD"		 			, 	"R:\\builds\\vs\\release") 	);
    mIniParas.Add( &svnExecutableE->SetupIni(			"SVN_EXECUTABLE"		 	, 	"C:\\cygwin\\bin\\svn.exe") 	);

	mIniFile->Load();
	mIniParas.Read();

    //UPdate UI
	SandBoxFolderE->Update();
	VSBuildRootFolderE->Update();
	svnExecutableE->Update();


    //Setup Logger
	gLogger.Init("", lDebug1, unique_ptr<LogFile>(new LogFile(mLogFileName)));
    Log(lInfo)<<"Logger was initialized";

    //Start thread to read log file
    if(!mLogFileReader.IsAlive())
    {
    	mLogFileReader.ReStart();
	}
}
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{}

void __fastcall	TMainForm::UpdateLog()
{
	if(mLogFileReader.HasData())
    {
    	string& aStr = *(mLogFileReader.GetData());
    	mLogMemo->Lines->Add(aStr.c_str());
        mLogFileReader.Purge();
    }
}

void __fastcall	TMainForm::UpdateFromSVNUpdate()
{
//	if(mLogFileReader.HasData())
//    {
//    	string& aStr = *(mLogFileReader.GetData());
//    	mLogMemo->Lines->Add(aStr.c_str());
//        mLogFileReader.Purge();
//    }
	Log(lInfo)<<"Svn update is updating";
}

void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
	mIniParas.Write();
    mIniFile->Save();
}

void __fastcall TMainForm::BrowseForFolderAExecute(TObject *Sender)
{
	TButton* aBtn = static_cast<TButton*>(static_cast<TAction*>(Sender)->ActionComponent);

    if(aBtn == SandBoxBtn || aBtn == VSBuildBtn)
    {

        String folder = BrowseForDir(NULL);
        string fldr = GetFilePath(stdstr(folder));

        if(!FolderExists(fldr))
        {
            return;
        }

        if(aBtn == SandBoxBtn)
        {
            SandBoxFolderE->SetString(fldr);
        }

        if(aBtn == VSBuildBtn)
        {
            VSBuildRootFolderE->SetString(fldr);
        }
    	return;
    }

    if(aBtn == svnExecutableBtn)
    {
    	BrowseForFileDlg->Execute();
    	if(BrowseForFileDlg->FileName.Length() > 0 )
    	{
            string aFile = stdstr(BrowseForFileDlg->FileName);
            svnExecutableE->SetString(aFile);
    	}
    }
}


void __fastcall TMainForm::Button5Click(TObject *Sender)
{
	BuildCheckAExecute(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::BuildCheckAExecute(TObject *Sender)
{
	//Check that folders exists
    if(FolderExists(SandBoxFolderE->GetString()))
    {
    	Log(lInfo)<<"The folder "<<SandBoxFolderE->GetString()<<" does exists";
    }

    if(FolderExists(VSBuildRootFolderE->GetString()))
    {
    	Log(lInfo)<<"The folder "<<VSBuildRootFolderE->GetString()<<" does exists";
    }

    if(FileExists(svnExecutableE->GetString()))
    {
    	Log(lInfo)<<"The svn executable: "<<svnExecutableE->GetString()<<" does exists";
    }

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
    ShutDownTimer->Enabled = false;
	if(mLogFileReader.IsAlive())
    {
    	CanClose = false;
	    ShutDownTimer->Enabled = true;
	    mLogFileReader.ShutDown();
        return;
    }
    else
    {
    	CanClose = true;
    }
}


void __fastcall TMainForm::ShutDownTimerTimer(TObject *Sender)
{
	Close();
}


void __fastcall TMainForm::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)
{
	if(Key == VK_ESCAPE)
    {
        Close();
    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SVNUpdateExecute(TObject *Sender)
{
	//Run the svn update in a thread, using mtkProcess
	mSVNUpdateProcess.SetExeFile(svnExecutableE->GetString());
    mSVNUpdateProcess.SetMessageHandling(CATCH_MESSAGE);
    mSVNUpdateProcess.Create("update " + SandBoxFolderE->GetString());
    mSVNUpdateThread.Run();
}






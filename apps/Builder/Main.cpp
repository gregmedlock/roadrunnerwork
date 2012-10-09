//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
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
mIniParas(mIniFile->GetIniFile())
{
	mIniFile->Init();
    mIniParas.SetIniSection("GENERAL");
    mIniParas.Add( &SandBoxFolderE->SetupIni(				"SANDBOX_FOLDER"		 	, 	"R:\\") 	);
	mIniFile->Load();
	mIniParas.Read();


    //UPdate UI
	SandBoxFolderE->Update();

}
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()

{
}

void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
	mIniParas.Write();
    mIniFile->Save();
}

void __fastcall TMainForm::BrowseForFolderAExecute(TObject *Sender)
{
	String folder = BrowseForDir(NULL);
	string fldr = GetFilePath(stdstr(folder));

    if(FolderExists(fldr))
    {
		SandBoxFolderE->SetString(fldr);
    }
}



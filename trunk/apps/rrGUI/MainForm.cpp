#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
#include "MainForm.h"
#include "mtkFileUtils.h"
#include "mtkStringUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Chart"
#pragma link "mtkFloatLabeledEdit"
#pragma link "mtkIniFileC"
#pragma link "mtkIntLabeledEdit"
#pragma link "TeEngine"
#pragma link "TeeProcs"
#pragma link "TFileSelectionFrame"
#pragma resource "*.dfm"
TMForm *MForm;
//---------------------------------------------------------------------------
using namespace mtk;

__fastcall TMForm::TMForm(TComponent* Owner)
    : TForm(Owner)
{
    LogOutput::mLogToConsole = (false);
    LogOutput::SetLogMemo(mLogMemo);
    LogOutput::mLogToMemo = true;
    mIniFileC->Init();
    mModelFolders.SetIniFile(mIniFileC->GetFile());
    mModelFolders.SetIniSection("MODEL_FOLDERS");

    mIniFileC->Load();
    mModelFolders.Read();
    Log(lInfo)<<"Reading settings..";
    mtkIniSection* folders = mIniFileC->GetSection("MODEL_FOLDERS");
    if(folders)
    {
        //Fill out combo box
        for(int i = 0; i < folders->KeyCount(); i++)
        {
            mtkIniKey* aKey = folders->mKeys[i];
            Log(lInfo)<<*aKey;
            modelFoldersCB->Items->Add(aKey->mValue.c_str());
        }
    }

	TFileSelectionFrame1->TreeView1->OnDblClick =  LoadFromTreeViewAExecute;
//	TFileSelectionFrame1->TreeView1->PopupMenu  =  PopupMenu2;

    startupTimer->Enabled = true;
}

__fastcall TMForm::~TMForm()
{
    mModelFolders.Write();
    mIniFileC->Save();
}

//---------------------------------------------------------------------------
void __fastcall TMForm::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)
{
    if(Key == VK_ESCAPE)
    {
        Close();
    }
}

void __fastcall TMForm::Button2Click(TObject *Sender)
{
    Log(lInfo)<<"Hello";
}

void __fastcall TMForm::startupTimerTimer(TObject *Sender)
{
    startupTimer->Enabled = false;
    //Select  models folder
    modelFoldersCB->ItemIndex = 0;

    if( modelFoldersCB->ItemIndex > -1 && modelFoldersCB->ItemIndex <= modelFoldersCB->Items->Count)
    {
        mCurrentModelsFolder = ToSTDString(modelFoldersCB->Items[modelFoldersCB->ItemIndex].Text);
        modelFoldersCBSelect(NULL);
    }
}
//---------------------------------------------------------------------------

void __fastcall TMForm::modelFoldersCBChange(TObject *Sender)
{
    Log(lInfo)<<"Model folder is changing..";
}
//---------------------------------------------------------------------------

void __fastcall TMForm::modelFoldersCBSelect(TObject *Sender)
{
    if( modelFoldersCB->ItemIndex > -1 && modelFoldersCB->ItemIndex <= modelFoldersCB->Items->Count)
    {
        mCurrentModelsFolder = ToSTDString(modelFoldersCB->Text);
        TFileSelectionFrame1->RemoveMonitoredFolders();

        Log(lInfo)<<"Model folder: "<<mCurrentModelsFolder<<" is selected..";
        TFileSelectionFrame1->MonitorFolder(mCurrentModelsFolder, "*.xml");
    	TFileSelectionFrame1->ReScanDataFolderAExecute(NULL);
    }
}

void __fastcall TMForm::selectModelsFolderExecute(TObject *Sender)
{
    //Browse for folder
    String folder = BrowseForDir(NULL);

    if(folder.Length())
    {
        Log(lInfo)<<"Selected folder "<<ToSTDString(folder.c_str());
        string fldr = ToSTDString(folder);
        fldr = RemoveTrailingSeparator(fldr, "\\");
        fldr = RemoveTrailingSeparator(fldr, "\\");
        if(FolderExists(fldr))
        {
            mCurrentModelsFolder = ToSTDString(fldr.c_str());

            //Check if present in CBox
            int indx = modelFoldersCB->Items->IndexOf(folder) ;
            if(indx == -1)
            {
                modelFoldersCB->Items->Add(mCurrentModelsFolder.c_str());
                modelFoldersCB->ItemIndex = modelFoldersCB->Items->IndexOf(folder);
                mtkIniSection* folders = mIniFileC->GetSection("MODEL_FOLDERS");
                if(folders)
                {
                    string  keyName = "Item" + mtk::ToString(folders->KeyCount() + 1);
                    folders->CreateKey(keyName, mCurrentModelsFolder);
                }
            }
        }
    }
}

void __fastcall TMForm::LoadFromTreeViewAExecute(TObject *Sender)
{
    string fName = TFileSelectionFrame1->GetSelectedFileInTree();
    if(fName.size())
    {
        Log(lInfo)<<"Loading model: "<<  fName;
    }
}



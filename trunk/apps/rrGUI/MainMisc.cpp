#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "rrLogger.h"
#include "rrUtils.h"
#include "MainForm.h"
#include "rrStringUtils.h"
#pragma package(smart_init)

using namespace rr;
void __fastcall TMForm::SetupINIParameters()
{
    mIniFileC->Init();

    //General Parameters
    mGeneralParas.SetIniSection("GENERAL");
    mGeneralParas.SetIniFile(mIniFileC->GetFile());
    mGeneralParas.Insert( &mStartTimeE->SetupIni("START_TIME", 0));
    mGeneralParas.Insert( &mEndTimeE->SetupIni("END_TIME", 40));
    mGeneralParas.Insert( &mNrOfSimulationPointsE->SetupIni("NR_OF_SIMULATION_POINTS", 100));
    mGeneralParas.Insert( &mSelectionListHeight.Setup("SEL_LB_HEIGHT", 30));
    mGeneralParas.Insert( &mCompiler.Setup("MODEL_COMPILER", "tcc"));
    mGeneralParas.Insert( &mConservationAnalysis.Setup("CONSERVATION_ANALYSIS", "false"));

    mModelFolders.SetIniSection("MODEL_FOLDERS");
    mModelFolders.SetIniFile(mIniFileC->GetFile());

    mIniFileC->Load();
    mGeneralParas.Read();
    mModelFolders.Read();

    Log(rr::lInfo)<<"Reading settings..";
    mtkIniSection* folders = mIniFileC->GetSection("MODEL_FOLDERS");
    if(folders)
    {
        //Fill out combo box
        for(int i = 0; i < folders->KeyCount(); i++)
        {
            mtkIniKey* aKey = folders->mKeys[i];
            Log(rr::lInfo)<<*aKey;
            modelFoldersCB->Items->Add(aKey->mValue.c_str());
        }
    }

    //Update UI
    mStartTimeE->Update();
    mEndTimeE->Update();
    mNrOfSimulationPointsE->Update();
    SelList->Height = mSelectionListHeight;

    if(mCompiler == "tcc")
    {
        CompilerRG->ItemIndex = 0;
    }
    else if(mCompiler == "bcc")
    {
        CompilerRG->ItemIndex = 1;
    }

    ConservationAnalysisCB->Checked = mConservationAnalysis == "true" ? true : false;
}

void __fastcall TMForm::LogMessage()
{
    if(mLogString)
    {
        mLogMemo->Lines->Add(mLogString->c_str());
        delete mLogString;
        // Signal to the data sink thread that we can now accept another message...
        mLogString = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TMForm::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)
{
    if(Key == VK_ESCAPE)
    {
        Close();
    }
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

void __fastcall TMForm::logModelFileAExecute(TObject *Sender)
{
    string fName = TFileSelectionFrame1->GetSelectedFileInTree();
    if(fName.size())
    {
        Log(rr::lInfo)<<"Model File: "<<fName;
        if(!rr::FileExists(fName))
        {
            return;
        }

        ifstream aFile(fName.c_str());
        string  str((std::istreambuf_iterator<char>(aFile)), std::istreambuf_iterator<char>());

        vector<string> strings = rr::SplitString(str,"\n");
        for(int i = 0; i < strings.size(); i++)
        {
            Log(rr::lInfo)<<strings[i];
        }
    }
}

TColor TMForm::GetColor(int i)
{
    switch(i)
    {
        case 0: return clRed;
        case 1: return clBlue;
        case 2: return clGreen;
        case 3: return clPurple;
        case 4: return clOlive;
        case 5: return clCream;
        case 6: return clBlack;
        default: return clBlue;
    }
}

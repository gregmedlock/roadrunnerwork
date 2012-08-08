#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "MainForm.h"
//#include "mtkFileUtils.h"
//#include "mtkStringUtils.h"
#include "rrRoadRunner.h"
#include "rrLogger.h"
#include "rrException.h"
#include "rrStringUtils.h"
#include "rrUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Chart"
#pragma link "mtkFloatLabeledEdit"
#pragma link "mtkIniFileC"
#pragma link "mtkIntLabeledEdit"
#pragma link "TeEngine"
#pragma link "TeeProcs"
#pragma link "TFileSelectionFrame"
#pragma link "Series"
#pragma link "TeeComma"
#pragma link "TeeEdit"
#pragma resource "*.dfm"
TMForm *MForm;
//---------------------------------------------------------------------------
using namespace rr;

__fastcall TMForm::TMForm(TComponent* Owner)
    : TForm(Owner),
    mLogFileSniffer("", this),
    mLogString(NULL)
{
    LogOutput::mLogToConsole = (false);
    gLog.SetCutOffLogLevel(rr::lDebug1);
    mTempDataFolder = "R:\\rrTemp";

    //This is roadrunners logger
    mRRLogFileName = rr::JoinPath(mTempDataFolder, "RoadRunnerUI.log");
    gLog.Init("", gLog.GetLogLevel(), unique_ptr<LogFile>(new LogFile(mRRLogFileName )));

    //Setup a logfile sniffer and propagate logs to memo...
    mLogFileSniffer.SetFileName(mRRLogFileName);
    mLogFileSniffer.Start();
    SetupINIParameters();
	TFileSelectionFrame1->TreeView1->OnDblClick =  LoadFromTreeViewAExecute;
	TFileSelectionFrame1->TreeView1->PopupMenu  =  TVPopupMenu;

    TFileSelectionFrame1->FSToolBar->Visible = false;
    startupTimer->Enabled = true;
    mRR = new RoadRunner;
}

__fastcall TMForm::~TMForm()
{
    mSelectionListHeight = SelList->Height;
    mGeneralParas.Write();
    mModelFolders.Write();
    mIniFileC->Save();
    delete mRR;
    mLogFileSniffer.ShutDown();
}

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
//---------------------------------------------------------------------------

void __fastcall TMForm::modelFoldersCBChange(TObject *Sender)
{
    Log(rr::lInfo)<<"Model folder is changing..";
}
//---------------------------------------------------------------------------

void __fastcall TMForm::modelFoldersCBSelect(TObject *Sender)
{
    if( modelFoldersCB->ItemIndex > -1 && modelFoldersCB->ItemIndex <= modelFoldersCB->Items->Count)
    {
        mCurrentModelsFolder = ToSTDString(modelFoldersCB->Text);
        TFileSelectionFrame1->RemoveMonitoredFolders();

        Log(rr::lInfo)<<"Model folder: "<<mCurrentModelsFolder<<" is selected..";
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
        Log(rr::lInfo)<<"Selected folder "<<ToSTDString(folder.c_str());
        string fldr = ToSTDString(folder);
        fldr = RemoveTrailingSeparator(fldr, "\\");
        fldr = RemoveTrailingSeparator(fldr, "\\");
        if(rr::FolderExists(fldr))
        {
            mCurrentModelsFolder = ToSTDString(fldr.c_str());

            //Check if present in CBox
            int indx = modelFoldersCB->Items->IndexOf(folder) ;
            if(indx == -1)
            {
                modelFoldersCB->Items->Add(mCurrentModelsFolder.c_str());
                modelFoldersCB->ItemIndex = modelFoldersCB->Items->IndexOf(folder);
                mtkIniSection* folders = mIniFileC->GetSection("MODEL_FOLDERS");
                if(!folders)
                {
                    if(mIniFileC->CreateSection("MODEL_FOLDERS"))
                    {
                        folders = mIniFileC->GetSection("MODEL_FOLDERS");
                    }
                }
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
    ClearMemoA->Execute();
    string fName = TFileSelectionFrame1->GetSelectedFileInTree();
    if(fName.size())
    {
        Log(rr::lInfo)<<"Loading model: "<<  fName;

        try
        {
            if(!mRR)
            {
                //delete mRR;
                mRR = new RoadRunner;
            }

            if(mRR->loadSBMLFromFile(fName))
            {
                Log(rr::lInfo)<<"Loaded model with no exception";

                loadAvailableSymbolsA->Execute();
                //Enable simulate action
                SimulateA->Enabled = true;
            }
            else
            {
                Log(rr::lInfo)<<"There was problems loading model from file: "<<fName;
                SimulateA->Enabled = false;
            }
        }
        catch(const rr::Exception& ex)
        {
            Log(rr::lInfo)<<"RoadRunner Exception :"<<ex.what();
            SimulateA->Enabled = false;
        }
    }
}

void __fastcall TMForm::logModelFileAExecute(TObject *Sender)
{
    string fName = TFileSelectionFrame1->GetSelectedFileInTree();
    if(fName.size())
    {
        Log(rr::lInfo)<<"Model File: "<<  fName;
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

//---------------------------------------------------------------------------
void __fastcall TMForm::LoadModelAExecute(TObject *Sender)
{
    LoadFromTreeViewAExecute(Sender);
}

void __fastcall TMForm::ClearMemoAExecute(TObject *Sender)
{
    mLogMemo->Clear();
}

//---------------------------------------------------------------------------
void __fastcall TMForm::SimulateAExecute(TObject *Sender)
{
    if(mRR)
    {
        //Setup selection list
        mRR->simulateEx(0,5, 100);
        SimulationData data = mRR->GetSimulationResult();

        string resultFileName( rr::JoinPath(mRR->GetTempFileFolder(), mRR->GetModelName()));
        resultFileName = ChangeFileExtensionTo(resultFileName, ".csv");

        Log(rr::lInfo)<<"Saving result to file: "<<resultFileName;

        ofstream fs(resultFileName.c_str());
        fs << data;
        fs.close();

        Plot(data);
    }
}

//---------------------------------------------------------------------------
void __fastcall TMForm::loadAvailableSymbolsAExecute(TObject *Sender)
{
    if(mRR)
    {
        SelList->Clear();
        ArrayList2 symbols = mRR->getAvailableSymbols();
        StringList fs = symbols.GetSubList("Floating Species");
//        StringList fs = symbols.GetSubList("Floating Species");
        Log(rr::lInfo)<<fs;
        //Add floating species to list box
        for(int i = 0; i < fs.Count(); i++)
        {
            SelList->Items->Add(fs[i].c_str());
            SelList->Checked[i] = true;
        }
    }
}

void TMForm::Plot(const rr::SimulationData& result)
{
    Chart1->RemoveAllSeries();

    //Fill out data for all series
    Log(rr::lInfo)<<"Simulation Result"<<result;
    int nrOfSeries = result.GetNrOfCols() -1; //First one is time
    StringList colNames = result.GetColumnNames();
    vector<TLineSeries*> series;
    for(int i = 0; i < nrOfSeries; i++)
    {
        TLineSeries* aSeries = new TLineSeries(Chart1);
        aSeries->Title = colNames[i+1].c_str();
        series.push_back(aSeries);
        Chart1->AddSeries(aSeries);
    }

    for(int j = 0; j < result.GetNrOfRows(); j++)
    {
        double xVal = result(j,0);
        for(int i = 0; i < nrOfSeries; i++)
        {
            series[i]->AddXY(xVal, result(j, i+1), "");
        }
    }


}

void __fastcall TMForm::ChartEditor2Click(TObject *Sender)
{
    ChartEditor1->Execute();
}
//---------------------------------------------------------------------------


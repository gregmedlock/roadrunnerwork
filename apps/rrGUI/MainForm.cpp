#ifdef USE_PCH
#include "rr_pch.h"
#endif
#pragma hdrstop
#include "MainForm.h"
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
#pragma link "TeeTools"
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

    //Setup road runner
    mRR = new RoadRunner;
    mRR->SetTempFileFolder(mTempDataFolder);
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

//---------------------------------------------------------------------------
void __fastcall TMForm::modelFoldersCBChange(TObject *Sender)
{
    Log(rr::lInfo)<<"Model folder is changing..";
}

//---------------------------------------------------------------------------
void __fastcall TMForm::modelFoldersCBSelect(TObject *Sender)
{
    if(modelFoldersCB->ItemIndex > -1 && modelFoldersCB->ItemIndex <= modelFoldersCB->Items->Count)
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

    if(!folder.Length())
    {
        Log(rr::lInfo)<<"Bad folder...";
        return;
    }

    Log(rr::lInfo)<<"Selected folder "<<ToSTDString(folder.c_str());
    string fldr = ToSTDString(folder);
    fldr = RemoveTrailingSeparator(fldr, "\\");
    fldr = RemoveTrailingSeparator(fldr, "\\");
    if(!rr::FolderExists(fldr))
    {
        return;
    }

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

//---------------------------------------------------------------------------
void __fastcall TMForm::LoadModelAExecute(TObject *Sender)
{
    LoadFromTreeViewAExecute(Sender);
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
        StringList list = GetCheckedSpecies();
        string selected = list.AsString();
        mRR->setSelectionList(selected);

        Log(rr::lInfo)<<"Currently selected species: "<<mRR->getSelectionList().AsString();
        mRR->simulateEx(mStartTimeE->GetValue(), *mEndTimeE, mNrOfSimulationPointsE->GetValue());
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

StringList TMForm::GetCheckedSpecies()
{
    //Go trough the listbox and return checked items

    StringList checked;
    for(int i = 0; i < SelList->Count; i++)
    {
        if(SelList->Checked[i])
        {
            String anItem = SelList->Items->Strings[i];
            checked.Add(ToSTDString(anItem));
        }
    }
    return checked;
}

//---------------------------------------------------------------------------
void __fastcall TMForm::loadAvailableSymbolsAExecute(TObject *Sender)
{
    if(mRR)
    {
        SelList->Clear();
        ArrayList2 symbols = mRR->getAvailableSymbols();
        StringList fs = symbols.GetSubList("Floating Species");
        Log(rr::lInfo)<<fs;

        //Add floating species to list box
        for(int i = 0; i < fs.Count(); i++)
        {
            SelList->Items->Add(fs[i].c_str());
            SelList->Checked[i] = true;
        }
        CheckUI();
    }
}

void TMForm::Plot(const rr::SimulationData& result)
{
    Chart1->RemoveAllSeries();

    //Fill out data for all series
    Log(rr::lDebug4)<<"Simulation Result"<<result;
    int nrOfSeries = result.GetNrOfCols() -1; //First one is time
    StringList colNames = result.GetColumnNames();
    vector<TLineSeries*> series;
    for(int i = 0; i < nrOfSeries; i++)
    {
        TLineSeries* aSeries = new TLineSeries(Chart1);
        aSeries->Title = colNames[i+1].c_str();

        aSeries->Color = GetColor(i);
        aSeries->LinePen->Width = 3;
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

void __fastcall TMForm::CheckUI()
{
    //Check if there is at least one checked species in the list box
    bool hasOneSelected = false;

    for(int i = 0; i < SelList->Count; i++)
    {
        if(SelList->Checked[i])
        {
            hasOneSelected = true;
            break;
        }
    }

    EnableDisableSimulation(hasOneSelected);
}

//---------------------------------------------------------------------------
void TMForm::EnableDisableSimulation(bool enableDisable)
{
    if(enableDisable)
    {
        Log(rr::lInfo)<<"Enabling simulation..";
    }
    else
    {
        Log(rr::lInfo)<<"Disabling simulation..";
    }
    mStartTimeE->Enabled            = enableDisable;
    mEndTimeE->Enabled              = enableDisable;
    mNrOfSimulationPointsE->Enabled = enableDisable;
    SimulateA->Enabled              = enableDisable;
}

void __fastcall TMForm::SelListClick(TObject *Sender)
{
    CheckUI();
}



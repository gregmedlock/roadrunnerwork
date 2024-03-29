#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "Chart.hpp"
#include "mtkFloatLabeledEdit.h"
#include "mtkIniFileC.h"
#include "mtkIntLabeledEdit.h"
#include "TeEngine.hpp"
#include "TeeProcs.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "TFileSelectionFrame.h"
#include "mtkIniParameters.h"
#include <Menus.hpp>
#include "rrLogFileReader.h"
#include <ToolWin.hpp>
#include <CheckLst.hpp>
#include "Series.hpp"
#include "TeeComma.hpp"
#include "TeeEdit.hpp"
#include "rrStringList.h"
#include "TeeTools.hpp"
#include "mtkSTDStringEdit.h"
#include "rrSimulationSettings.h"
#include "rrLogLevel.h"
#include <jpeg.hpp>
#include <OleCtrls.hpp>
#include <SHDocVw.hpp>
#include "rrSimulateThread.h"
namespace rr
{
class RoadRunner;
class SimulationData;

}

namespace LIB_LA
{
template <class T>
class Matrix;
}

using namespace rr;
//---------------------------------------------------------------------------
class TMForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TPanel *Panel2;
    TStatusBar *StatusBar1;
    TRadioGroup *CompilerRG;
    TGroupBox *GroupBox1;
    TButton *Button1;
    TMemo *mLogMemo;
    TChart *Chart1;
    TActionList *RRActions;
    TAction *CompileA;
    TGroupBox *GroupBox3;
    mtkFloatLabeledEdit *mStartTimeE;
    mtkFloatLabeledEdit *mEndTimeE;
    mtkIntLabeledEdit *mNrOfSimulationPointsE;
    mtkIniFileC *mIniFileC;
    TFileSelectionFrame *FSF;
    TComboBox *modelFoldersCB;
    TTimer *startupTimer;
    TAction *selectModelsFolder;
    TAction *LoadFromTreeViewA;
    TSplitter *Splitter2;
    TPopupMenu *TVPopupMenu;
    TAction *logModelFileA;
    TMenuItem *LogModelFile1;
    TAction *LoadModelA;
    TMenuItem *Load1;
    TToolBar *ToolBar1;
    TPanel *Panel3;
    TToolButton *ToolButton1;
    TActionList *MiscActions;
    TAction *ClearMemoA;
    TPopupMenu *MemoPopup;
    TMenuItem *Clear1;
    TAction *SimulateA;
    TSplitter *Splitter3;
    TAction *loadAvailableSymbolsA;
    TCheckListBox *SelList;
    TLineSeries *Series1;
    TChartEditor *ChartEditor1;
    TPopupMenu *ChartPopup;
    TMenuItem *ChartEditor2;
    TGroupBox *GroupBox2;
    TPanel *Panel4;
    TPanel *Panel5;
    TAction *UnLoadModelA;
    TButton *Button3;
    TLabel *mModelNameLbl;
    TPageControl *PageControl1;
    TTabSheet *TabSheet1;
    TTabSheet *TabSheet2;
    mtkSTDStringEdit *filterEdit;
    TButton *Button4;
    TButton *loadUnloadBtn;
    TToolButton *ToolButton2;
    TAction *LogCurrentDataA;
    TCheckBox *ConservationAnalysisCB;
    TComboBox *LogLevelCB;
    TLabel *Label1;
    TTabSheet *TabSheet3;
    TImage *testSuitePic;
    TGroupBox *GroupBox4;
    TWebBrowser *WebBrowser1;
    TButton *Button2;
    TPageControl *PageControl2;
    TTabSheet *TabSheet4;
    TTabSheet *TabSheet5;
    TSplitter *Splitter1;
    TToolButton *ToolButton3;
    TAction *LogCCodeA;
    TPopupMenu *DropBoxPopup;
    TAction *RemoveCurrentModelFolderItemA;
    TMenuItem *Remove1;
    TToolBar *ToolBar2;
    TToolButton *ToolButton4;
    TActionList *TestSuiteActions;
    TAction *PlotTestTestSuiteData;
	TTimer *ShutDownTimer;
	TTabSheet *TabSheet6;
	mtkIntLabeledEdit *runCount;
	TButton *Button5;
	TMemo *runCountMemo;
	TGroupBox *GroupBox5;
	TGroupBox *GroupBox6;
	TButton *RunThreadBtn;
	TTimer *CheckThreadTimer;
    void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
    void __fastcall startupTimerTimer(TObject *Sender);
    void __fastcall modelFoldersCBChange(TObject *Sender);
    void __fastcall modelFoldersCBSelect(TObject *Sender);
    void __fastcall selectModelsFolderExecute(TObject *Sender);
    void __fastcall LoadFromTreeViewAExecute(TObject *Sender);
    void __fastcall logModelFileAExecute(TObject *Sender);
    void __fastcall LoadModelAExecute(TObject *Sender);
    void __fastcall ClearMemoAExecute(TObject *Sender);
    void __fastcall SimulateAExecute(TObject *Sender);
    void __fastcall loadAvailableSymbolsAExecute(TObject *Sender);
    void __fastcall ChartEditor2Click(TObject *Sender);
    void __fastcall SelListClick(TObject *Sender);
    void __fastcall UnLoadModelAExecute(TObject *Sender);
    void __fastcall filterEditKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
    void __fastcall Button4Click(TObject *Sender);
    void __fastcall LogCurrentDataAExecute(TObject *Sender);
    void __fastcall LoadModelAUpdate(TObject *Sender);
    void __fastcall FSFTreeView1Click(TObject *Sender);
    void __fastcall LogLevelCBChange(TObject *Sender);
    void __fastcall LogCCodeAExecute(TObject *Sender);
    void __fastcall RemoveCurrentModelFolderItemAExecute(TObject *Sender);
    void __fastcall modelFoldersCBContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
    void __fastcall PlotTestTestSuiteDataExecute(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall ShutDownTimerTimer(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall CheckThreadTimerTimer(TObject *Sender);
	void __fastcall RunThreadBtnClick(TObject *Sender);

private:	// User declarations
    mtkIniParameters            	mGeneralParas;

    mtkIniParameter<int>        	mSelectionListHeight;
    mtkIniParameter<int>        	mPageControlHeight;

    mtkIniParameter<mtkLogLevel>   	mLogLevel;
    mtkIniParameter<string>     	mCompiler;
    mtkIniParameter<string>     	mCurrentModelsFolder;
    mtkIniParameter<string>     	mCurrentModelFileName;
    mtkIniParameter<string>     	mTempDataFolder;
    mtkIniParameter<string>     	mRRLogFileName;
    mtkIniParameter<bool>       	mConservationAnalysis;

    mtkIniParameters            	mModelFolders;
    rr::RoadRunner             	   *mRR;                //RoadRunner instance
    rr::LogFileReader           	mLogFileSniffer;

    void            	__fastcall  SetupINIParameters();
    void                        	Plot(const rr::SimulationData& result);
    void                        	EnableDisableSimulation(bool enable);
    void            	__fastcall  CheckUI();
    StringList                      GetCheckedSpecies();
    TColor                          GetColor(int i);
    void                            AddItemsToListBox(const StringList& items);
    SimulationSettings              mSettings;
    string                          GetCompiler();//What is set in the RadioGroup
    void            	__fastcall  UpdateTestSuiteInfo();
    string                          GetCurrentModelPath();
    string                          GetSettingsFile();
    SimulateThread			        mSimulateThread;
	SimulationData 				   *mData;				//The data is created by the thread and consumed by the UI
    friend SimulateThread;
    void            __fastcall    	PlotFromThread();

public:		// User declarations
                    __fastcall      TMForm(TComponent* Owner);
                    __fastcall 	   ~TMForm();
    void            __fastcall      LogMessage();
    string                         *mLogString;

};
//---------------------------------------------------------------------------
extern PACKAGE TMForm *MForm;

#endif

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
namespace rr
{
class RoadRunner;
}
//---------------------------------------------------------------------------
class TMForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TPanel *Panel2;
    TStatusBar *StatusBar1;
    TRadioGroup *RadioGroup1;
    TGroupBox *GroupBox1;
    TButton *Button1;
    TMemo *mLogMemo;
    TChart *Chart1;
    TActionList *RRActions;
    TButton *Button2;
    TAction *CompileA;
    TGroupBox *GroupBox3;
    mtkFloatLabeledEdit *mStartTimeE;
    mtkFloatLabeledEdit *mEndTimeE;
    mtkIntLabeledEdit *mNrOfSimulationPointsE;
    mtkIniFileC *mIniFileC;
    TFileSelectionFrame *TFileSelectionFrame1;
    TComboBox *modelFoldersCB;
    TTimer *startupTimer;
    TAction *selectModelsFolder;
    TAction *LoadFromTreeViewA;
    TSplitter *Splitter1;
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

private:	// User declarations
    mtkIniParameters            mGeneralParas;

    mtkIniParameter<int>        mSelectionListHeight;
    mtkIniParameters            mModelFolders;
    mtkIniParameter<string>     mCurrentModelsFolder;
    mtkIniParameter<string>     mTempDataFolder;
    mtkIniParameter<string>     mRRLogFileName;

    void            __fastcall  SetupINIParameters();
    rr::RoadRunner             *mRR;                //RoadRunner instance
    rr::LogFileReader           mLogFileSniffer;

public:		// User declarations
                    __fastcall  TMForm(TComponent* Owner);
                    __fastcall ~TMForm();
        void        __fastcall  LogMessage();
        string                 *mLogString;
};
//---------------------------------------------------------------------------
extern PACKAGE TMForm *MForm;

#endif

//---------------------------------------------------------------------------

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
#include "mtkLogger.h"
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
    TActionList *ActionList1;
    TGroupBox *GroupBox2;
    TButton *Button2;
    TButton *Button3;
    TAction *CompileA;
    TGroupBox *GroupBox3;
    mtkFloatLabeledEdit *mtkFloatLabeledEdit1;
    mtkFloatLabeledEdit *mtkFloatLabeledEdit2;
    mtkIntLabeledEdit *mtkIntLabeledEdit1;
    mtkIniFileC *mIniFileC;
    TFileSelectionFrame *TFileSelectionFrame1;
    TComboBox *modelFoldersCB;
    TTimer *startupTimer;
    TAction *selectModelsFolder;
    TAction *LoadFromTreeViewA;
    void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall startupTimerTimer(TObject *Sender);
    void __fastcall modelFoldersCBChange(TObject *Sender);
    void __fastcall modelFoldersCBSelect(TObject *Sender);
    void __fastcall selectModelsFolderExecute(TObject *Sender);
    void __fastcall LoadFromTreeViewAExecute(TObject *Sender);

private:	// User declarations
    mtkIniParameters            mGeneralParas;
    mtkIniParameters            mModelFolders;
    mtkIniParameter<string>     mCurrentModelsFolder;


public:		// User declarations
        __fastcall              TMForm(TComponent* Owner);
        __fastcall             ~TMForm();
};
//---------------------------------------------------------------------------
extern PACKAGE TMForm *MForm;

#endif

//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TFileSelectionFrame.h"
#include <ExtCtrls.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <string>
#include "mtkExeFile.h"
#include "mtkProcess.h"
using namespace std;
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGroupBox *GroupBox1;
	TMemo *Memo1;
	TFileSelectionFrame *fsf;
	TPanel *Panel1;
	TPanel *Panel2;
	TSplitter *Splitter1;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TTabSheet *TabSheet2;
	TMemo *Memo2;
	TButton *Button1;
	TActionList *ActionList1;
	TAction *LoadModelA;
	TAction *TestModelA;
	TMemo *Log1;
	TPanel *Panel3;
	TEdit *mModelFileName;
	TAction *updateFileName;
	TRadioGroup *RGLogLevel;
	TTabSheet *TabSheet3;
	TMemo *Memo3;
	TMemo *Memo4;
	TSplitter *Splitter2;
	TGroupBox *GroupBox2;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	void __fastcall LoadModelAExecute(TObject *Sender);
	void __fastcall fsfTreeView1DblClick(TObject *Sender);
	void __fastcall TestModelAExecute(TObject *Sender);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall fsfTreeView1Click(TObject *Sender);

private:	// User declarations
	string 			mrrModelsRoot;
	string 			mOutputRoot;
    mtkExeFile		mRR;
	string 			GetSelectedFileName();

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

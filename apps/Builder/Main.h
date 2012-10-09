#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include "mtkSTDStringEdit.h"
#include <ComCtrls.hpp>
#include "TRegistryForm.h"
#include "mtkIniParameters.h"
#include "mtkIniFileC.h"
struct AppParas
{
		mtkIniParameter<string>	mSandBoxFolder;
};

//---------------------------------------------------------------------------
class TMainForm : public TRegistryForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TPanel *Panel2;
	TPanel *Panel3;
	TMemo *Memo1;
	TActionList *BuildActions;
	TAction *CreateDownloadPageWiki;
	TButton *Button1;
	TAction *SVNUpdate;
	TButton *Button2;
	TButton *Button3;
	TAction *BuildVisualStudioA;
	TAction *SVNCommitA;
	TButton *Button4;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TTabSheet *TabSheet2;
	TAction *BuildCheckA;
	TButton *Button5;
	TGroupBox *GroupBox1;
	mtkSTDStringEdit *VSBuildRootFolderE;
	mtkSTDStringEdit *SandBoxFolderE;
	TButton *Button6;
	TButton *Button7;
	TActionList *MiscActions;
	TAction *BrowseForFolderA;
	mtkIniFileC *mIniFile;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall BrowseForFolderAExecute(TObject *Sender);
private:	// User declarations

	mtkIniParameters    		mIniParas;
    AppParas 					mAppParas;


public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
	__fastcall ~TMainForm();
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif

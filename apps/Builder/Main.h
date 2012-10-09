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
#include "mtkLogFileReader.h"
#include "mtkProcess.h"
#include "mtkProcessThread.h"

#include <Dialogs.hpp>
struct AppParas
{

};

using namespace mtk;

//---------------------------------------------------------------------------
class TMainForm : public TRegistryForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TPanel *Panel2;
	TPanel *Panel3;
	TMemo *mLogMemo;
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
	TButton *SandBoxBtn;
	TButton *VSBuildBtn;
	TActionList *MiscActions;
	TAction *BrowseForFolderA;
	mtkIniFileC *mIniFile;
	mtkSTDStringEdit *svnExecutableE;
	TButton *svnExecutableBtn;
	TTimer *ShutDownTimer;
	TFileOpenDialog *BrowseForFileDlg;
	TAction *BrowseForFileA;
	TOpenDialog *BrowseForFolderDlg;
	TStatusBar *StatusBar1;
	TSplitter *Splitter1;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall BrowseForFolderAExecute(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall BuildCheckAExecute(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall ShutDownTimerTimer(TObject *Sender);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall SVNUpdateExecute(TObject *Sender);


private:	// User declarations

	mtkIniParameters    		mIniParas;
    AppParas 					mAppParas;
    string						mLogFileName;
    string*						mLogData;
    string*						mSVNUpdateData;
	LogFileReader				mLogFileReader;
    mtkProcess					mSVNUpdateProcess;
	ProcessThread 				mSVNUpdateThread;

public:		// User declarations
	void	__fastcall			UpdateLog();
	void 	__fastcall 			UpdateFromSVNUpdate();

	__fastcall TMainForm(TComponent* Owner);
	__fastcall ~TMainForm();
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif

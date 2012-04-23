//---------------------------------------------------------------------------
#include <string>
#include <vcl.h>
#pragma hdrstop
#include "Main.h"
#include "mtkVCLUtils.h"
#include "mtkFileUtils.h"
#include "mtkFileSystemTreeItems.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TFileSelectionFrame"
#pragma resource "*.dfm"
TForm1 *Form1;

using namespace mtk;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
:
TForm(Owner)
{
	mrrModelsRoot = "C:\\rrw\\Models\\sbml-test-cases-2.0.2\\cases\\semantic";
	mOutputRoot  = "C:\\rrw\\DataOutput\\XE";

	fsf->MonitorFolder(mrrModelsRoot,"*l2v4.xml");
	fsf->MonitorFolder(mOutputRoot,"*.c");
    fsf->ReScanDataFolderAExecute(NULL);

	fsf->TreeView1->OnClick    = fsfTreeView1Click;
    fsf->TreeView1->OnDblClick = fsfTreeView1DblClick;

    TMenuItem* loadXML = new TMenuItem(NULL);
    loadXML->Action = LoadModelA;
    fsf->TreeView1->PopupMenu->Items->Insert(0,loadXML);
	mRR.SetFileName("C:\\rrw\\installs\\xe\\bin\\simulate.exe");
    mRR.SetMessageHandling(CATCHMESSAGE);
}


string TForm1::GetSelectedFileName()
{
	//Load XML file into XML memo
    string itemName;
    TTreeNode* aNode = fsf->GetSelected()->GetPrev();

    if(!aNode)
    {
    	return string("");
    }

    mtkFileSystemItem *info  = (mtkFileSystemItem*)(aNode->Data);    // cast data into mtkTreeItemBase pointer
    if(!info)
        return string("");
	mtkFileItem* 	fileItem 	= dynamic_cast<mtkFileItem*>(info);
    itemName = ToSTDString(fsf->GetSelected()->Text);

	string path = JoinPath(mrrModelsRoot, ToSTDString(aNode->Text));
	string fullName = JoinPath(path, itemName);
    return  mtk::FileExists(fullName) ? fullName : string("");

}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadModelAExecute(TObject *Sender)
{
    string fName = GetSelectedFileName();

    if(fName.size())
    {
		Memo2->Lines->LoadFromFile(fName.c_str());
    }
}

void __fastcall TForm1::fsfTreeView1DblClick(TObject *Sender)
{
	LoadModelAExecute(NULL);
}


void __fastcall TForm1::TestModelAExecute(TObject *Sender)
{
	Log1->Clear();
    mtkProcess	rr(&mRR);

 	//Get test suite number from selected file
	string fName = ToSTDString(mModelFileName->Text);
    fName = ExtractFileName(fName);
    vector<string> parts = SplitString(fName,"-");

    //First part is the number
    int nTheNumber = ToInt(parts[0]);

    stringstream paras;
    paras<<"-v"<<RGLogLevel->ItemIndex<<" -n"<<nTheNumber;

    rr.Create(paras.str().c_str());
    rr.Run();
    vector<string> msg = rr.GetOutput();
    for(int i = 0; i < msg.size(); i++)
    {
    	Log1->Lines->Add(msg[i].c_str());
    }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)
{
    if(Key == VK_ESCAPE)
    {
        Close();
    }
}

void __fastcall TForm1::fsfTreeView1Click(TObject *Sender)
{
	//Fill out filename
    string name = GetSelectedFileName();
    if(name.size())
    {
    	mModelFileName->Text = name.c_str();
    }
}



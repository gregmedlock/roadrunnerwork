unit ufMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRoadRunnerAPI, ExtCtrls, IOUtils, Grids;

type
  TfrmMain = class(TForm)
    pnlBottm: TPanel;
    lblProgress: TLabel;
    btnGetCopyright: TButton;
    btnLoadSBML: TButton;
    grid: TStringGrid;
    btnGetReactionNames: TButton;
    btnGetAvailableSymbols: TButton;
    lstSummary: TListBox;
    btnSteadyState: TButton;
    edtModelName: TEdit;
    btnLoadTwoModels: TButton;
    Label1: TLabel;
    lblTempFolder: TEdit;
    chkConservationLaws: TCheckBox;
    btnSimulate: TButton;
    procedure btnGetCopyrightClick(Sender: TObject);
    procedure btnLoadSBMLClick(Sender: TObject);
    procedure btnGetReactionNamesClick(Sender: TObject);
    procedure btnGetAvailableSymbolsClick(Sender: TObject);
    procedure btnSteadyStateClick(Sender: TObject);
    procedure btnLoadTwoModelsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkConservationLawsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    rrInstance : Pointer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

Uses uMatrix, uRRList;

procedure TfrmMain.btnGetCopyrightClick(Sender: TObject);
begin
  lblProgress.caption := string (getCopyright);
end;

procedure TfrmMain.btnGetReactionNamesClick(Sender: TObject);
var i : integer; strList : TStringList;
begin
  strList := getReactionNames;
  lstSummary.Clear;
  for i := 0 to strList.Count - 1 do
      lstSummary.Items.Add (strList[i]);
  strList.Free;
end;

procedure TfrmMain.btnLoadSBMLClick(Sender: TObject);
var str : AnsiString;
    m : TMatrix;
    i, j : integer;
    list : TStringList;
begin
  rrInstance := getRRInstance;
  setComputeAndAssignConservationLaws (chkConservationLaws.checked);
  lstSummary.Clear;

  str := AnsiString (TFile.ReadAllText(edtModelName.text));
  if not loadSBML(str) then
     begin
     lblProgress.Caption := 'Failed to load SBML model';
     exit;
     end;
end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
var list: TStringList;
    i, j: integer;
    m : TMatrix;
begin
  list := TStringList.Create;
  //list.Add ('time');
  //list.Add ('Node0');
  //list.Add ('Node1');
  //setSelectionList (list);
  m := simulate();
  grid.ColCount := m.c + 1;
  grid.RowCount := m.r + 1;
  for i := 1 to m.r do
      for j := 1 to m.c do
          grid.Cells [j-1, i] := Format ('%8.5g', [m[i,j]]);
  list.free;

  lstSummary.Items.Add ('Number of Reactions: ' + inttostr (getNumberOfReactions));
  lstSummary.Items.Add ('Number of Boundary Species: ' + inttostr (getNumberOfBoundarySpecies));
  lstSummary.Items.Add ('Number of Floating Species: ' + inttostr (getNumberOfFloatingSpecies));
  lstSummary.Items.Add ('Number of Global Parameters: ' + inttostr (getNumberOfGlobalParameters));
  lstSummary.Items.Add ('');
  list := getReactionNames;
  for i := 0 to list.Count - 1 do
      lstSummary.Items.Add ('Reaction Name: ' + list[i]);
  list.Free;
  lstSummary.Items.Add ('');

  list := getBoundarySpeciesNames;
  for i := 0 to list.Count - 1 do
      lstSummary.Items.Add ('Boundary Species Name: ' + list[i]);
  list.Free;
  lstSummary.Items.Add ('');

  list := getFloatingSpeciesNames;
  for i := 0 to list.Count - 1 do
      lstSummary.Items.Add ('Floating Species Name: ' + list[i]);
  list.Free;
  lstSummary.Items.Add ('');

  list := getGlobalParameterNames;
  for i := 0 to list.Count - 1 do
      lstSummary.Items.Add ('Global Parameter Name: ' + list[i]);
  lstSummary.Items.Add ('');
  list.Free;
end;

procedure TfrmMain.btnSteadyStateClick(Sender: TObject);
var d : double;
    fn : TStringList;
    i : integer;
begin
  d := steadyState;
  showmessage (Format('%g', [d]));
  fn := getFloatingSpeciesNames;
  for i := 0 to fn.Count - 1 do
      showmessage (floattostr (getValue(fn[i])));
  fn.free;
end;

procedure TfrmMain.chkConservationLawsClick(Sender: TObject);
var b : boolean;
begin
  if chkConservationLaws.checked then
     b := true
  else
     b := false;
  setComputeAndAssignConservationLaws (b);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  freeRRInstance (rrInstance);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var errMsg : AnsiString;
begin
  if loadRoadRunner (errMsg) then
     begin
     rrInstance := getRRInstance;
     lblProgress.caption := 'RoadRunner Loaded: ' + 'Build Date: ' + getBuildDate;
     lblTempFolder.Text := getTempFolder;
     end
  else
     lblProgress.caption := 'Failed to load: ' + string (errMsg);
end;

procedure TfrmMain.btnLoadTwoModelsClick(Sender: TObject);
var  instance : Pointer;
     str1, str2 : string;
     b : boolean;
begin
  instance := getRRInstance;
  str1 := AnsiString (TFile.ReadAllText('feedback.xml'));
  b := loadSBML (str1);
  if b then
     showmessage ('loadSBML successful (true)')
  else
     showmessage ('Failed to loadSBML (false)');
  if fileExists ('equilib.xml') then
     begin
     str2 := AnsiString (TFile.ReadAllText('equilib.xml'));
     b := loadSBML (str2);
     end
  else
     showmessage ('Failed to find file');
end;


procedure TfrmMain.btnGetAvailableSymbolsClick(Sender: TObject);
var x, list : TRRList; i, j : integer;
begin
  lstSummary.Clear;
  x := getAvailableSymbols;
  for i := 0 to x.count - 1 do
      begin
      lstSummary.Items.Add (x[i].getList[0].getString);   // Label
      if x[i].getList.count > 1 then
         begin
         list := x[i].getList;
         for j := 1 to list.Count - 1 do
             lstSummary.Items.Add (list[j].getString);
         end;
      end;
end;

end.

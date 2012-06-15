unit ufMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRoadRunnerAPI, ExtCtrls, IOUtils, Grids;

type
  TForm2 = class(TForm)
    Button1: TButton;
    pnlBottm: TPanel;
    lblProgress: TLabel;
    btnGetCopyright: TButton;
    btnLoadSBML: TButton;
    grid: TStringGrid;
    btnGetReactionNames: TButton;
    btnGetAvailableSymbols: TButton;
    lstSummary: TListBox;
    btnSteadyState: TButton;
    lblBuildDate: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure btnGetCopyrightClick(Sender: TObject);
    procedure btnLoadSBMLClick(Sender: TObject);
    procedure btnGetReactionNamesClick(Sender: TObject);
    procedure btnGetAvailableSymbolsClick(Sender: TObject);
    procedure btnSteadyStateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

Uses uMatrix, uRRList;

procedure TForm2.btnGetCopyrightClick(Sender: TObject);
begin
  lblProgress.caption := string (getCopyright);
end;

procedure TForm2.btnGetReactionNamesClick(Sender: TObject);
var i : integer; strList : TStringList;
begin
  strList := getReactionNames;
  lstSummary.Clear;
  for i := 0 to strList.Count - 1 do
      lstSummary.Items.Add (strList[i]);
  strList.Free;
end;

procedure TForm2.btnLoadSBMLClick(Sender: TObject);
var str : AnsiString;
    m : TMatrix;
    i, j : integer;
    list : TStringList;
begin
  lstSummary.Clear;
  list := TStringList.Create;
  str := AnsiString (TFile.ReadAllText('feedback.xml'));
  if not loadSBML(str) then
     lblProgress.Caption := 'Failed to load SBML model';
  list.Add ('time');
  list.Add ('S1');
  list.Add ('S2');
  setSelectionList (list);
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


procedure TForm2.btnSteadyStateClick(Sender: TObject);
var d : double;
begin
  d := steadyState;
  showmessage (floattostr (d));
end;

procedure TForm2.Button1Click(Sender: TObject);
var errMsg : AnsiString;
begin
  if loadRoadRunner (errMsg) then
     begin
     lblProgress.caption := 'RoadRunner Loaded';
     lblBuildDate.Caption := 'Build Date: ' + getBuildDate;
     end
  else
     begin
     lblProgress.caption := string (errMsg);
     lblBuildDate.Caption := 'Failed to load';
     end;
end;

procedure TForm2.btnGetAvailableSymbolsClick(Sender: TObject);
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

program DelphiTestProject;

uses
  Forms,
  ufMain in 'ufMain.pas' {Form2},
  uRoadRunnerAPI in '..\..\Wrappers\Delphi\uRoadRunnerAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

program FaceDetectApp;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  unitFaces in 'unitFaces.pas' {formfaces},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(Tformfaces, formfaces);
  Application.Run;
end.

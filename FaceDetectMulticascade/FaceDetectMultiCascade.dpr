// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program FaceDetectMultiCascade;
{$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Forms,
  Windows,
  unit1 in 'unit1.pas' {Form1};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.MainForm.HandleNeeded;
  Application.Run;
end.


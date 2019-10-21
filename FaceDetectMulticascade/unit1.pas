unit unit1;

interface

uses
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.core.types_c,
  ocv.core_c,
  ocv.highgui_c,
  ocv.objdetect_c,
  ocv.utils,
  ocv.cls.contrib,
  ocv.legacy,

  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  IniFiles;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RadioButton1Click(Sender: TObject);

  private
    FrameBitmap: TBitmap;
    m_bSessionEnding   : Boolean;
    m_iCameraIndex  : Integer;
    procedure StartCapture(camnum:integer);
    procedure StopCapture;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure DetectAndDraw();
    procedure ReadConfig;
  public
    //
  protected
    //
  end;

var
Form1: TForm1;

CascadeFile: AnsiString = 'face.xml';
ClassCascade: pCvHaarClassifierCascade = nil;

MyCapture: pCvCapture;
MyInputImage: pIplImage;
MyStorage: pCvMemStorage;

implementation
{$R *.dfm}


procedure TForm1.WMQueryEndSession(var Message: TMessage);
begin
  m_bSessionEnding := True;
  Message.Result := 1;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not m_bSessionEnding then
  begin
    StopCapture;
    Halt;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReadConfig;
  StartCapture(m_iCameraIndex);
end;

procedure TForm1.StopCapture;
begin
  Application.OnIdle := nil;
  cvReleaseCapture(MyCapture);
  FrameBitmap.Free;
end;

procedure TForm1.StartCapture(camnum:integer);
begin
  FrameBitmap := TBitmap.Create;
  FrameBitmap.PixelFormat := pf24bit;

  MyCapture := cvCreateCameraCapture(camnum);

    ClassCascade := cvLoad(pCVChar(@CascadeFile[1]), nil, nil, nil);
    if not Assigned(ClassCascade) then
    begin
      ShowMessage('ERROR: Could not load cascade.');
      Halt;
    end;

    MyStorage := cvCreateMemStorage(0);
    if Assigned(MyCapture) then Application.OnIdle := OnIdle
    else
      Exit;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  MyInputImage := cvQueryFrame(MyCapture);
  if (not Assigned(MyInputImage)) then Application.OnIdle := nil
  else
  begin
    DetectAndDraw();
    IplImage2Bitmap(MyInputImage, FrameBitmap);
    Image1.Picture.Graphic := FrameBitmap;
    Done := False;
  end;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  if RadioButton1.Checked then
    CascadeFile:='face.xml';
  if RadioButton2.Checked then
    CascadeFile:='eyes.xml';
  if RadioButton3.Checked then
    CascadeFile:='mouth.xml';
  if RadioButton4.Checked then
    CascadeFile:='faceshoulders.xml';
  if RadioButton5.Checked then
    CascadeFile:='smile.xml';


  ClassCascade := cvLoad(pCVChar(@CascadeFile[1]), nil, nil, nil);
end;

procedure TForm1.DetectAndDraw();
var
  iIndex : Integer;
  inputObject: pCvSeq;
  ObjectOutimage: pCvRect;
  inputPt1, inputPt2: TCvPoint;
begin
  inputObject := cvHaarDetectObjects(MyInputImage, ClassCascade, MyStorage, 1.1, 10,
                                   CV_HAAR_SCALE_IMAGE, cvSize(90, 90), cvSize(0, 0));
  cvClearMemStorage(MyStorage);
  if inputObject.total=0 then exit;
  for iIndex := 1 to inputObject^.total do
  begin
    ObjectOutimage := pCvRect(cvGetSeqElem(inputObject, iIndex));
    inputPt1.x := ObjectOutimage^.x;
    inputPt2.x := (ObjectOutimage^.x + ObjectOutimage^.width);
    inputPt1.y := ObjectOutimage^.y;
    inputPt2.y := (ObjectOutimage^.y + ObjectOutimage^.height);
    cvRectangle(MyInputImage, inputPt1, inputPt2, CV_RGB(0, 0, 255), 2, 8, 0);
  end;
end;

procedure TForm1.ReadConfig;
var
 oAppINI   : TInifile;
begin
  oAppINI := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini')) ;
  try
     m_iCameraIndex := oAppINI.ReadInteger('Camera','Index',0) ;
  finally
     oAppINI.Free;
  end;
end;

end.

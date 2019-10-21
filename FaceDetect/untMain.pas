unit untMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  VCL.Forms,
  VCL.Dialogs,
  ExtCtrls,
  StdCtrls, Vcl.ExtDlgs,
  Jpeg, System.ImageList, Vcl.ImgList,

  // OpenCV Units
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.core.types_c,
  ocv.core_c,
  ocv.highgui_c,
  ocv.objdetect_c,
  ocv.utils,
  ocv.cls.contrib,
  ocv.legacy,

  IniFiles,
  //SQLite Units
  SqlExpr,
  Data.DbxSqlite;

type
  PRGB24 = ^TRGB24;
  TRGB24 = record B, G, R: Byte; end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGB24;
  pfaceImage = ^TfaceImage;
    TfaceImage = record
    MyCapture: pCvCapture;       // Capture handle
    MyInputImage: pIplImage;     // Input image
    MyStorage: pCvMemStorage;    // Memory storage
    TotalFaceDetect: Integer;    // Total face detect
  end;
  TfrmMain = class(TForm)
    ImageListButtons: TImageList;
    Button1: TButton;
    Button3: TButton;
    Button5: TButton;
    Button2: TButton;
    Timer1: TTimer;
    SavePictureDialog1: TSavePictureDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Edit1: TEdit;
    Button4: TButton;
    LabelTimer: TLabel;
    lbName: TLabel;
    CheckDetectEyes: TCheckBox;
    pnImagem: TPanel;
    Image1: TImage;
    pnDetect: TPanel;
    Image2: TImage;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
  private
    { Private declarations }
    m_iCameraIndex : Integer;
    FrameBitmap: TBitmap;
    SessionEnding: Boolean;
    MyfaceImage: pfaceImage;
    font: TCvFont;
    GlobalSize: TCvSize;

    LiveFacesTestHSV:pIplImage;
    inComingfaces0: pIplImage;
    LiveFacesToTest:pIplImage;
    iplcoloredfacesave:pIplImage;
    iplgrayfacesave:pIplImage;
    ipllivefaceshow: pIplImage;
    ReSizeFrame:pIplImage;
    dbInputImage:pIplImage;
    procedure ChangeImageExposure(src: TBitmap; k: Single);
    procedure StartCapture(camnum:integer);
    procedure StopCapture;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    function DetectAndDraw(const faceImage: pfaceImage) : Boolean;
    procedure JustDetect();
    procedure ClearWindow;
    procedure CreateDataFile(const sFile : String);
    procedure StreamToVariant (Stream : TMemoryStream; var v : OleVariant);
    function JPEGToVariant(aJPEG : TJPEGImage) : OleVariant;
    procedure InitVariables;
    procedure InitDB;
    procedure InitPaths;
    procedure VariantToStream (const v : olevariant; Stream : TMemoryStream);
    procedure VariantToJPEGToBMP(aValue:OleVariant; var aJPEG:TJPEGImage; var aBMP:TBitmap);
    procedure ReadConfig;
  public
    { Public declarations }
    procedure LoadDatabase;
    procedure LoadFaces(Sender: TObject);
    function DoSQLiteConnect : Boolean;
  end;

var
  frmMain: TfrmMain;
  FaceCascadeFile : AnsiString;
  FaceCascade     : pCvHaarClassifierCascade = nil;

  EyesCascadeFile : AnsiString;
  EyesCascade     : pCvHaarClassifierCascade = nil;

  StoragePath     : AnsiString;


  dbNumfaces     : Integer = 0;
  mi_countprints : Integer;


  oImagelface : TimageList;
  oNameslist  : TStringList;
  vIdlist     : array of integer;

  fimages    : TInputArrayOfIplImage;
  flabels    : TInputArrayOfInteger;

threadvar

  oBitmapDisplay      : TBitmap;
  inputPt1, inputPt2  : TCvPoint;

  sqliteConn          : TSQLConnection;
  sqliteQuery         : TSQLQuery;

  lbpface_id          : IFaceRecognizer;
  lbpgender_id        : IFaceRecognizer;
  lbpemo_id           : IFaceRecognizer;

  fxnames             : array of string;
  gfxnames            : array of string;
  efxnames            : array of string;

  MyHandle            : THandle;
  xgetfrom            : String;

const
  WORK_WIDTH  = 128;
  WORK_HEIGHT = 128;


implementation

{$R *.dfm}

uses UnitFaces;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  sNomeArquivo : String;
begin
  ClearWindow;
  StopCapture; //Ref: #13392
  mi_countprints := 0;

  //Ref: #13246
  LabelTimer.Caption := '10';
  Timer1.Enabled := True;

  SavePictureDialog1.DefaultExt := GraphicExtension(TBitmap);
  StartCapture(m_iCameraIndex);
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  ClearWindow;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  iBrightCycle : Integer;
  bFound : Boolean;
begin
  OpenPictureDialog1.InitialDir := StoragePath;
  if OpenPictureDialog1.Execute then
  begin
    ClearWindow;
    DoSQLiteconnect;
    LoadDatabase;

    for iBrightCycle := 0 to 50 do
    begin

      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      ChangeImageExposure(Image1.Picture.Bitmap, iBrightCycle * 5);
      Image1.Repaint;

      if not Assigned(FrameBitmap) then
      begin
        FrameBitmap := TBitmap.Create;
        FrameBitmap.PixelFormat := pf24bit;
      end;

      resizeframe := cvCreateImage(cvSize(1280,960), IPL_DEPTH_8U, 3); //SET XVGA INPUT TO
      MyfaceImage.MyInputImage := cvCreateImage(cvSize(640,480), IPL_DEPTH_8U, 3); //HD OUTPUT

      //Ref: #13392
      MyfaceImage.MyCapture := cvCreateCameraCapture(m_iCameraIndex);
      cvSetCaptureProperty(MyfaceImage.MyCapture, CV_CAP_PROP_FRAME_WIDTH, 640);
      cvSetCaptureProperty(MyfaceImage.MyCapture, CV_CAP_PROP_FRAME_HEIGHT, 480);

      FaceCascade := cvLoad(pCVChar(@FaceCascadeFile[1]), nil, nil, nil);
      if not Assigned(FaceCascade) then
      begin
        ShowMessage('ERROR: Unable to load cascade file:' + FaceCascadeFile);
        Halt;
      end;

      EyesCascade := cvLoad(pCVChar(@EyesCascadeFile[1]), nil, nil, nil);
      if not Assigned(EyesCascade) then
      begin
        ShowMessage('ERROR: Unable to load cascade file:' + EyesCascadeFile);
        Halt;
      end;

      MyfaceImage.MyStorage := cvCreateMemStorage(0);
      MyfaceImage.MyInputImage := BitmapToIplImage(Image1.Picture.Bitmap);

      bFound:=DetectAndDraw(MyfaceImage);
      IplImage2Bitmap(MyfaceImage.MyInputImage, FrameBitmap);
      Image1.Picture.Graphic := FrameBitmap;
      if bFound then
        Break;
    end;

  end;

end;

procedure TfrmMain.Button4Click(Sender: TObject);
var
  bSave: TBitmap;
  jDest: TJPEGImage;
begin
  //if (totfacedetected=0) then exit;
  if (Edit1.Text='') then exit;
  if (Edit1.Text='Digite um nome') then exit;
  if (Image2.Picture.Graphic = nil) then exit;

  ipllivefaceshow := CropIplImage(MyfaceImage.MyInputImage,CvRect(inputPt1.x,inputPt1.y,inputPt2.x-inputPt1.x,inputPt2.y-inputPt1.y));
  cvResize(ipllivefaceshow, iplcoloredfacesave, CV_INTER_LINEAR);
  cvCvTColor(iplcoloredfacesave, iplgrayfacesave, Cv_BGR2GRAY);

  bSave := Tbitmap.Create;
  bSave := CvImage2Bitmap(iplcoloredfacesave);
  iplImage2Bitmap(iplgrayfacesave,bSave);

  jDest := TJpegImage.Create;
  jDest.ProgressiveEncoding:=true;
  jDest.Scale := jsFullSize;
  jDest.Smoothing := True;
  jDest.Performance := jpBestQuality;
  jDest.Assign(bSave);

  sqliteQuery.SQL.Text := 'INSERT INTO tfaces(name,face) VALUES(:xname,:xface)';
  sqliteQuery.ParamByName('xname').Value := Edit1.Text;
  sqliteQuery.ParamByName('xface').AsBlob := JPEGToVariant(jDest);
  sqliteQuery.ExecSQL;

  bSave.Free;
  jDest.Free;
  Edit1.Text:='';
  ClearWindow;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  formfaces.Show();
end;

procedure TfrmMain.ChangeImageExposure(src: TBitmap; k: Single);
var
  RGB: PRGBArray;
  i, x, y, RGBOffset: Integer;
  lut: array[0..255] of integer;
begin
  for i := 0 to 255 do begin
    if k < 0 then
      lut[i]:= i - ((-Round((1 - Exp((i / -128)*(k / 128)))*256)*(i xor 255)) shr 8)
    else
      lut[i]:= i + ((Round((1 - Exp((i / -128)*(k / 128)))*256)*(i xor  255)) shr 8);
    if lut[i] < 0 then lut[i] := 0 else if lut[i] > 255 then lut[i] := 255;
  end;
  RGB := src.ScanLine[0];
  RGBOffset := Integer(src.ScanLine[1]) - Integer(RGB);
  for y := 0 to src.Height - 1 do begin
    for x := 0 to src.Width - 1 do begin
      RGB[x].R := LUT[RGB[x].R];
      RGB[x].G := LUT[RGB[x].G];
      RGB[x].B := LUT[RGB[x].B];
    end;
    RGB:= PRGBArray(Integer(RGB) + RGBOffset);
  end;
end;

procedure TfrmMain.LoadFaces(Sender: TObject);
begin
  DoSQLiteconnect;
  LoadDatabase;
end;

procedure TfrmMain.VariantToStream(const v : olevariant; Stream : TMemoryStream);
var
  p : pointer;
begin
  Stream.Position := 0;
  Stream.Size := VarArrayHighBound (v, 1) - VarArrayLowBound(v,  1) + 1;
  p := VarArrayLock (v);
  Stream.Write (p^, Stream.Size);
  VarArrayUnlock (v);
  Stream.Position := 0;
end;

procedure TfrmMain.VariantToJPEGToBMP(aValue:OleVariant; var aJPEG:TJPEGImage; var aBMP:TBitmap);
var
   Stream : TMemoryStream;
begin
  try
    Stream := TMemoryStream.Create;
    VariantToStream (aValue,Stream);
    aJPEG.LoadfromStream(Stream);
    aBMP.Assign(aJPEG);
  finally
     aValue:=0;
     Stream.free;
  end;
end;

procedure TfrmMain.LoadDatabase;
var
  dbFaces: pIplImage;
  xID:integer;
  xFace:OleVariant;
  jpgFace: TJpegImage;
  bmpFace: TBitmap;
  xname:string;
begin
  sqliteQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS                          '+
                          '       tfaces(id INTEGER PRIMARY KEY AUTOINCREMENT, '+
                          '              name VARCHAR(25),                     '+
                          '              face MEDIUMBLOB) ;                    ';
  sqliteQuery.ExecSQL;
  sqliteQuery.SQL.Text := 'PRAGMA auto_vacuum = FULL';
  sqliteQuery.ExecSQL;
  sqliteQuery.SQL.Text := 'SELECT id,name,face FROM tfaces';
  sqliteQuery.Open;

  dbNumfaces := sqliteQuery.RecordCount;

  if (dbNumfaces=0) then Exit;

  jpgFace := TJpegImage.Create;
  bmpFace := TBitmap.Create;

  SetLength(vIdlist, dbNumfaces);
  SetLength(fimages, dbNumfaces);
  SetLength(flabels, dbNumfaces);
  setlength(fxnames, dbNumfaces);

  dbFaces := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 1);
  dbNumfaces:=0;

  while not(sqliteQuery.Eof) do
  begin
    xID:=sqliteQuery.FieldByName('id').AsInteger;
    xName:=sqliteQuery.FieldByName('name').AsString;
    xFace:=sqliteQuery.FieldByName('face').AsVariant;

    VariantToJPEGToBMP(xFace, jpgFace, bmpFace);
    dbFaces := ocv.utils.BitmapToIplImage(bmpFace);

    bmpFace.Canvas.StretchDraw(Rect(0, 0, bmpFace.Width-28, bmpFace.Height-28), bmpFace);
    oImagelface.Add(bmpFace,bmpFace);
    oNameslist.Add(xName);

    fimages[dbNumfaces] := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 1);
    cvCvTColor(dbFaces, fimages[dbNumfaces], Cv_BGR2GRAY);
    flabels[dbNumfaces] := dbNumfaces;
    fxnames[dbNumfaces] := xName;
    vIdlist[dbNumfaces] := xID;

    inc(dbNumfaces);
    sqliteQuery.Next;
  end;
  FreeAndNil(jpgFace);
  FreeAndNil(bmpFace);
  cvReleaseImage(dbFaces);
  sqliteQuery.Close;

  lbpface_id := TFaceRecognizer.createLBPHFaceRecognizer(1,8,8,8,69); //select2
  lbpface_id.Train(fimages,flabels);

end;


procedure TfrmMain.WMQueryEndSession(var Message: TMessage);
begin
  SessionEnding := True;
  Message.Result := 1;
end;

//USE SQL DATABASE CONNECTION
function TfrmMain.DoSQLiteConnect : boolean;
begin
  try
    sqliteConn := TSQLConnection.Create(nil);
    sqliteConn.DriverName := 'Sqlite';
    sqliteConn.ConnectionName := 'SQLiteConnection';
    sqliteConn.VendorLib:='sqlite3.dll';
    sqliteConn.Params.Values['Database'] := ExtractFilePath(ParamStr(0)) + 'dataface.db';
    sqliteQuery := TSQLQuery.Create(nil);
    sqliteQuery.SQLConnection := sqliteConn;
    sqliteConn.LoginPrompt := false;
    sqliteConn.Connected := True;
    result :=true;
  except
    result:=false;
  end;
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
begin
  Button4.Enabled := ((Edit1.Text <> 'Digite um nome') and (Edit1.Text <> '') and (lbName.Caption = 'Nome: '));
end;

procedure TfrmMain.Edit1Enter(Sender: TObject);
begin
  if (Edit1.Text = 'Digite um nome') then
  begin
    Edit1.Font.Color := clWindowText;
    Edit1.Text := '';
  end;
  Button4.Enabled := ((Edit1.Text <> 'Digite um nome') and (Edit1.Text <> '') and (lbName.Caption = 'Nome: '));
end;

procedure TfrmMain.Edit1Exit(Sender: TObject);
begin
  if (Trim(Edit1.Text) = '') then
  begin
    Edit1.Font.Color := clMedGray;
    Edit1.Text := 'Digite um nome';
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not SessionEnding then
  begin
    StopCapture;
    Halt;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ReadConfig;
  InitVariables;
  InitDB;
  InitPaths;
  LabelTimer.Caption := '';
  Timer1.Enabled := False;
end;

procedure TfrmMain.CreateDataFile(const sFile : String);
begin
  CopyFile(PWideChar(ExtractFilePath(ParamStr(0))+'dbblank.db'),PWideChar(ExtractFilePath(ParamStr(0))+'dataface.db'), False);
end;

procedure TfrmMain.InitVariables;
begin
  GlobalSize := cvSize(WORK_WIDTH, WORK_HEIGHT);
  cvInitFont(@font, CV_FONT_HERSHEY_COMPLEX, 0.8, 0.8, 0, 1, CV_AA);

  inComingfaces0 := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 3); //3 because its original
  iplcoloredfacesave := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 3); //3 because its original
  ipllivefaceshow := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 3); //3 because its original
  LiveFacesToTest := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 1); //1 because original to GRAY(BGR[0])
  iplgrayfacesave := cvCreateImage(GlobalSize, IPL_DEPTH_8U, 1); //1 because original to GRAY(BGR[0])

  oBitmapDisplay := TBitmap.Create;
  oBitmapDisplay.SetSize(WORK_WIDTH,WORK_HEIGHT);

  oNameslist := TStringList.Create;
  oImagelface := TimageList.Create(self);
  oImagelface.SetSize(WORK_WIDTH-28,WORK_HEIGHT-28);
end;

procedure TfrmMain.InitDB;
begin
  if not DoSqliteConnect then
     CreateDataFile('dataface.db');
  LoadDatabase;
end;

procedure TfrmMain.InitPaths;
begin
  FaceCascadeFile := ExtractFilePath(ParamStr(0)) +  'cascade\face.xmL';
  EyesCascadeFile := ExtractFilePath(ParamStr(0)) +  'cascade\eyes.xml';
  //Path for the images
  StoragePath     := ExtractFilePath(ParamStr(0)) +  'images\';
  //Create path if not exists
  ForceDirectories(StoragePath);
end;


procedure TfrmMain.StopCapture;
begin
  Application.OnIdle := nil;
  cvReleaseCapture(MyfaceImage.MyCapture);
  FrameBitmap.Free;
end;


procedure TfrmMain.StartCapture(camnum:integer);
begin
  FrameBitmap := TBitmap.Create;
  FrameBitmap.PixelFormat := pf24bit;
  FrameBitmap.SetSize(640,480); //Ref: #13392
  xgetfrom := 'localcam';

  //Ref: #13392
  MyfaceImage.MyCapture := cvCreateCameraCapture(camnum);
  cvSetCaptureProperty(MyfaceImage.MyCapture, CV_CAP_PROP_FRAME_WIDTH, 640);
  cvSetCaptureProperty(MyfaceImage.MyCapture, CV_CAP_PROP_FRAME_HEIGHT, 480);

  FaceCascade := cvLoad(pCVChar(@FaceCascadeFile[1]), nil, nil, nil);
  if not Assigned(FaceCascade) then
  begin
    ShowMessage('ERROR: Unable to load cascade file:' + FaceCascadeFile);
    Halt;
  end;

  EyesCascade := cvLoad(pCVChar(@EyesCascadeFile[1]), nil, nil, nil);
  if not Assigned(EyesCascade) then
  begin
    ShowMessage('ERROR: Unable to load cascade file:' + EyesCascadeFile);
    Halt;
  end;

  //Ref: #13392
  MyfaceImage.MyStorage := cvCreateMemStorage(0);
  if Assigned(MyfaceImage.MyCapture) then
  begin
    resizeframe := cvCreateImage(cvSize(1280,960), IPL_DEPTH_8U, 3); //SET XVGA INPUT TO
    MyfaceImage.MyInputImage := cvCreateImage(cvSize(640,480), IPL_DEPTH_8U, 3); //HD OUTPUT
    Application.OnIdle := OnIdle;
  end
  else
  begin
    resizeframe := cvCreateImage(cvSize(1280,960), IPL_DEPTH_8U, 3); //SET XVGA INPUT TO
    MyfaceImage.MyInputImage := cvCreateImage(cvSize(640,480), IPL_DEPTH_8U, 3); //HD OUTPUT
    Exit;
  end;

end;

procedure TfrmMain.OnIdle(Sender: TObject; var Done: Boolean);
var
  bFound : Boolean;
  iBrightCycle : Integer;
begin
  MyfaceImage.MyInputImage := cvQueryFrame(MyfaceImage.MyCapture);
  if (not Assigned(MyfaceImage.MyInputImage)) then Application.OnIdle := nil
  else
  begin
    JustDetect();
    IplImage2Bitmap(MyfaceImage.MyInputImage, FrameBitmap);
    Image1.Picture.Graphic := FrameBitmap;
    Done := False;
    //mi_countprints := mi_countprints + 1;
    SavePictureDialog1.FileName := '';
    if mi_countprints = 10 then
    begin
      //Ref: #13246
      LabelTimer.Caption := '';
      Timer1.Enabled := False;
      SavePictureDialog1.InitialDir := StoragePath;
      SavePictureDialog1.FileName :=  StoragePath + 'capture.bmp';
      if SavePictureDialog1.Execute then
      begin
        Image1.Picture.SaveToFile(SavePictureDialog1.FileName);
        for iBrightCycle := 0 to 50 do
        begin
          if iBrightCycle>0 then
            Image1.Picture.LoadFromFile(SavePictureDialog1.FileName);
          ChangeImageExposure(Image1.Picture.Bitmap, iBrightCycle * 5);
          Image1.Repaint;
          MyfaceImage.MyStorage := cvCreateMemStorage(0);
          resizeframe := BitmapToIplImage(Image1.Picture.Bitmap);
          cvResize(resizeframe, MyfaceImage.MyInputImage,  CV_INTER_NN);
          bFound:=DetectAndDraw(MyfaceImage);
          IplImage2Bitmap(MyfaceImage.MyInputImage, FrameBitmap);
          Image1.Picture.Graphic := FrameBitmap;
        end;
      end;
      Application.OnIdle := nil;
    end;
  end;
end;


procedure TfrmMain.StreamToVariant (Stream : TMemoryStream; var v : OleVariant);
var
  p : pointer;
begin
  v := VarArrayCreate ([0, Stream.Size - 1], varByte);
  p := VarArrayLock (v);
  Stream.Position := 0;
  Stream.Read (p^, Stream.Size);
  VarArrayUnlock (v);
end;


procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  mi_countprints := mi_countprints + 1;
  LabelTimer.Caption := IntToStr(10-mi_countprints);
end;

function TfrmMain.JPEGToVariant(aJPEG : TJPEGImage) : OleVariant;
var
  Stream        : TMemoryStream;
begin
  try
    Stream := TMemoryStream.Create;
    aJPEG.SaveToStream(Stream);
    StreamToVariant(Stream, result);
  finally
    Stream.Free;
  end;
end;

procedure TfrmMain.JustDetect();
var
i: Integer;
inputFace: pCvSeq;
faceOutimage: pCvRect;
inputPt1, inputPt2: TCvPoint;
begin
  inputFace := cvHaarDetectObjects(MyfaceImage.MyInputImage, FaceCascade, MyfaceImage.MyStorage, 1.1, 10,  CV_HAAR_SCALE_IMAGE, cvSize(90, 90), cvSize(0, 0));
  cvClearMemStorage(MyfaceImage.MyStorage);
  if inputFace.total=0 then exit;
end;

function TfrmMain.DetectAndDraw(const faceImage: pfaceImage) : Boolean;
var
  I: Integer;
  lab: Integer;
  iTotalFaceDetect : Integer;
  confidence: double;
  sAux : AnsiString;

  inputFace: pCvSeq;
  faceOutimage: pCvRect;
  inputFacePt1, inputFacePt2: TCvPoint;

  inputEye: pCvSeq;
  EyeOut:pCvRect;
  inputEyePt1, inputEyePt2: TCvPoint;

  inputMouth: pCvSeq;
  MouthOut:pCvRect;
  inputMouthPt1, inputMouthPt2: TCvPoint;

  iEyeIndex : Integer;

begin
  try
    inputFace := cvHaarDetectObjects(faceImage.MyInputImage, FaceCascade, faceImage.MyStorage,  1.2, 10, 2, cvSize(99, 99), cvSize(0, 0));
    cvClearMemStorage(faceImage.MyStorage);
    iTotalFaceDetect := inputFace.total;
    if inputFace.total=0 then exit;
    for I := 1 to inputFace^.total do
    begin
      faceOutimage := pCvRect(cvGetSeqElem(inputFace, I));

      inputFacePt1.x := faceOutimage^.x;
      inputFacePt2.x := (faceOutimage^.x + faceOutimage^.width);
      inputFacePt1.y := faceOutimage^.y;
      inputFacePt2.y := (faceOutimage^.y + faceOutimage^.height);

      inputPt1.x := faceOutimage^.x + 13;
      inputPt2.x := (faceOutimage^.x + faceOutimage^.width) - 13;
      inputPt1.y := faceOutimage^.y + 26;
      inputPt2.y := (faceOutimage^.y + faceOutimage^.height);

      ipllivefaceshow := CropIplImage(faceImage.MyInputImage,CvRect(inputFacePt1.x{left},inputFacePt1.y{top},{right}inputFacePt2.x-inputFacePt1.x,{bottom}inputFacePt2.y-inputFacePt1.y));

      oBitmapDisplay := cvImage2Bitmap(ipllivefaceshow);
      image2.Picture.Bitmap := oBitmapDisplay;

      ipllivefaceshow := CropIplImage(faceImage.MyInputImage,CvRect(inputPt1.x,inputPt1.y,inputPt2.x-inputPt1.x,inputPt2.y-inputPt1.y));
      cvResize(ipllivefaceshow, inComingfaces0, CV_INTER_LINEAR);


      cvCvTColor(inComingfaces0, LiveFacesToTest, Cv_BGR2GRAY);

      if (dbNumfaces<1) then
      begin
        cvRectangle(faceImage.MyInputImage, inputFacePt1, inputFacePt2, CV_RGB(0, 0, 255), 2, 8, 0);
        ipllivefaceshow := CropIplImage(faceImage.MyInputImage,CvRect(inputPt1.x,inputPt1.y,inputPt2.x-inputPt1.x,inputPt2.y-inputPt1.y));
        cvResize(ipllivefaceshow, inComingfaces0, CV_INTER_LINEAR);
        cvCvTColor(inComingfaces0, LiveFacesToTest, Cv_BGR2GRAY);
        iplImage2Bitmap(LiveFacesToTest, oBitmapDisplay);
        Exit;
      end;

      confidence := 0;
      lbpface_id.predict(LiveFacesToTest,lab,confidence);
      if (lab = -1) then
      begin
        cvRectangle(faceImage.MyInputImage, inputFacePt1, inputFacePt2, CV_RGB(255, 0, 0), 2, 8, 0);
        cvPutText(faceImage.MyInputImage, '???', cvPoint(inputFacePt1.x, inputFacePt1.y - 20), @font, cvScalar(0, 0, 255));
      end
      else
      begin
        sAux := fxnames[lab] + ' ' + FormatFloat('0.0%', confidence);
        lbName.Caption := 'Nome: ' + fxnames[lab];
        cvRectangle(faceImage.MyInputImage, inputFacePt1, inputFacePt2, CV_RGB(0, 255, 0), 2, 8, 0);
        cvPutText(faceImage.MyInputImage, PAnsiChar(sAux), cvPoint(inputFacePt1.x, inputFacePt1.y - 20), @font, cvScalar(0, 255, 0));
      end;


      if (CheckDetectEyes.Checked) then
      begin
        inputEye := cvHaarDetectObjects(ipllivefaceshow, EyesCascade, faceImage.MyStorage, 1.1, 10, 2,
        cvSize(0, 0), cvSize(0, 0));
        if (inputEye.total>1) then
        begin
           EyeOut := pCvRect(cvGetSeqElem(inputEye, 1));
           inputEyePt1.x := EyeOut.x;
           inputEyePt2.x := (EyeOut.x + EyeOut.width);
           inputEyePt1.y := EyeOut.y;
           inputEyePt2.y := (EyeOut.y + EyeOut.height);
           cvRectangle(ipllivefaceshow, inputEyePt1, inputEyePt2, CV_RGB(160,255,60), 2, 8, 0);
           EyeOut := pCvRect(cvGetSeqElem(inputEye, 2));
           inputEyePt1.x := EyeOut.x;
           inputEyePt2.x := (EyeOut.x + EyeOut.width);
           inputEyePt1.y := EyeOut.y;
           inputEyePt2.y := (EyeOut.y + EyeOut.height);
           cvRectangle(ipllivefaceshow, inputEyePt1, inputEyePt2, CV_RGB(60,255,160), 2, 8, 0);
           oBitmapDisplay := cvImage2Bitmap(ipllivefaceshow);
           image2.Picture.Bitmap := oBitmapDisplay;
        end;
      end;
      iplImage2Bitmap(LiveFacesToTest, oBitmapDisplay);
      oBitmapDisplay.FreeImage;
      cvReleaseImage(ipllivefaceshow);
    end;
   except
  end;
  Result:=iTotalFaceDetect>0;
end;

procedure TfrmMain.ClearWindow;
begin
  MyfaceImage := nil;
  MyfaceImage := AllocMem(SizeOf(TfaceImage));

  Image2.Picture.Graphic := nil;
  Image1.Picture.Graphic := nil;

  lbName.Caption := 'Nome: ';
  Edit1.Text := '';
  Edit1.Font.Color := clWindowText;
end;

procedure TfrmMain.ReadConfig;
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

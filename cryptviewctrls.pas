unit CryptViewCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, ExtCtrls, StdCtrls, Dialogs,
  AppStrings, Graphics, LResources, Buttons, FileUtil, LCLProc,
  FileStores, KeyStores, MemoryFiles, LCLIntf, ConfigReader;

type

  { TCryptView }
  {$WARNING Протестить на винде :-(}

  TCryptView = class(TPanel)
    ListView: TListView;
    PanelRight: TPanel;
    ImageFile: TImage;
    BtnDecrypt: TBitBtn;
    BtnPreview: TBitBtn;
    BtnDelete: TBitBtn;
    LabelFileName: TLabel;
    LabelFileDesc: TLabel;
  public
    type
    TViewError = set of (veNoError, veSystemError);
  protected
    procedure DisabledButtons;
    procedure EnabledButtons;
    procedure NoSelectedCtrls;
    procedure ClickDecryptAndSave(Sender: TObject);
    procedure ClickPreview(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickListView(Sender: TObject);
    procedure SelectedUpdate;
    function GetListItemByData(AData: integer): TListItem;
  private
    {** Данный компонент должен обеспечивать хранение файлов ==========================}
    FileStore: TFileStore;
    {** Данный компонент нужен для централизованного хранения паролей =================}
    KeyStore: TKeyStore;
    {** Хранение настроек =============================================================}
    Config: TConfigReader;
    {** Тесты **}
    procedure TestFileStore;
    procedure TestKeyStore;
    procedure TestConfig;
  private
    FLastError: TViewError;
    function CreateListView: boolean;
    function CreateRightPanel: boolean;
    function GetFileInfoItem: TKeyInfoItem;
    function GetSelected: boolean;
    procedure SetLastError(AValue: TViewError);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Add(FileName: string);
    procedure SelectedSaveToFile(FileName: string);
  public
    property SelectedFileItem: TKeyInfoItem read GetFileInfoItem;
    property IsSelected: boolean read GetSelected;
    property LastError: TViewError read FLastError write SetLastError;
  end;

// Отложенное удаление файла в потоке
function WaitDeleteFile(parameter: pointer): ptrint;

implementation

{ TCryptView }

function WaitDeleteFile(parameter: pointer): ptrint;
const
  MAX_STR_LEN = 4096;
var
  FilePath: PChar;
begin
  GetMem(FilePath, MAX_STR_LEN);
  try
    Move(PChar(parameter)^, FilePath^, strlen(PChar(parameter)) + 1);
    Sleep(10000);
  {$IFDEF UNIX}
    DeleteFile(FilePath);
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
    DeleteFile(PChar(FilePath));
  {$ENDIF WINDOWS}
  finally
    FreeMem(FilePath);
    Result := 0;
  end;
end;

function TCryptView.GetListItemByData(AData: integer): TListItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ListView.Items.Count - 1 do
    {$WARNINGS OFF}
    {$HINTS OFF}
    if PtrInt(Pointer(ListView.Items[i].Data)) = PtrInt(AData) then
    {$HINTS ON}
    {$WARNINGS ON}
      Result := ListView.Items[i];
end;

procedure TCryptView.TestFileStore;
  {$IFDEF TEST}
var
  FileInfo: TKeyInfoItem;
  FileKey: string;
  {$ENDIF TEST}
begin
  {$IFDEF TEST}
  FileStore := TFileStore.Create('D:\');
  FileInfo := FileStore.PutFile('D:\SecretCloud2\RES\USES\decrypt.bmp');
  ShowMessage('FileStore Key: ' + FileInfo.Key);
  FileKey := string(FileInfo.Key);
  SetLength(FileKey, FileInfo.KeySize);
  SaveMemoryFile(FileStore.GetFile(FileInfo.TarPath, FileKey), 'D:\1.bmp');
  FileStore.Free;
  {$ENDIF TEST}
end;

procedure TCryptView.TestKeyStore;
begin
  {$IFDEF TEST}
  {$ENDIF TEST}
end;

procedure TCryptView.TestConfig;
begin
  {$IFDEF TEST}
  {$ENDIF TEST}
end;

procedure TCryptView.SelectedSaveToFile(FileName: string);
begin
  SaveMemoryFile(FileStore.GetFile(SelectedFileItem.TarPath, SelectedFileItem.Key),
    FileName);
end;

procedure TCryptView.Add(FileName: string);
var
  Info: TKeyInfoItem;
begin
  if veSystemError in LastError then
    exit;
  if DirectoryExists(FileName) then
    Info := FileStore.PutFolder(FileName)
  else
    Info := FileStore.PutFile(FileName);
  KeyStore.Add(Info);
  ListView.BeginUpdate;
  ListView.Clear;
  KeyStore.UpdateListView(ListView);
  ListView.EndUpdate;
end;

procedure TCryptView.DisabledButtons;
begin
  BtnDecrypt.Enabled := False;
  BtnPreview.Enabled := False;
  BtnDelete.Enabled := False;
end;

procedure TCryptView.NoSelectedCtrls;
begin
  DisabledButtons;
  LabelFileDesc.Caption := '';
  LabelFileName.Caption := VIEW_NO_SELECTED;
  ImageFile.Picture.Clear;
end;

procedure TCryptView.EnabledButtons;
begin
  BtnDecrypt.Enabled := True;
  BtnPreview.Enabled := True;
  BtnDelete.Enabled := True;
end;

procedure TCryptView.ClickDecryptAndSave(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  if veSystemError in LastError then
    exit;
  if not IsSelected then
    exit;
  SaveDialog := TSaveDialog.Create(Self as TComponent);
  SaveDialog.FileName := ExtractFileName(SelectedFileItem.Name);
  if SaveDialog.Execute then
    SelectedSaveToFile(SaveDialog.FileName);
  SaveDialog.Free;
end;

procedure TCryptView.ClickPreview(Sender: TObject);
var
  FilePath: string;
begin
  if veSystemError in LastError then
    exit;
  if not IsSelected then
    Exit;
  case MessageDlg(DIALOG_WARNING, TEXT_PREVIEW, mtWarning, [mbYes, mbNo], '') of
    mrYes:
    begin
        {$IFDEF WINDOWS}
      FilePath := GetTempDir + ExtractFileName(SelectedFileItem.Name);
      ShowMessage(FilePath);
        {$ENDIF WINDOWS}
        {$IFDEF UNIX}
      FilePath := '/tmp/' + ExtractFileName(SelectedFileItem.Name);
        {$ENDIF UNIX}
      SelectedSaveToFile(FilePath);
      if OpenDocument(FilePath) then
        BeginThread(@WaitDeleteFile, PChar(FilePath));
    end;
  end;
end;

procedure TCryptView.ClickDelete(Sender: TObject);
var
  Tar: LongString;
  SelData: PtrInt;
  i: PtrInt;
begin
  if veSystemError in LastError then
    exit;
  if not IsSelected then
    Exit;
  case MessageDlg(DIALOG_WARNING, TEXT_DELETE, mtWarning, [mbYes, mbNo], '') of
    mrYes:
    begin
      try
        Tar := SelectedFileItem.TarPath;
        DeleteFile(Tar);
        {$HINTS OFF}
        SelData := PtrInt(ListView.Selected.Data);
        {$HINTS ON}
        KeyStore.Delete(SelData);
        if (SelData) <= ListView.Items.Count - 1 then
          for i := SelData to ListView.Items.Count - 1 do
            {$HINTS OFF}
            GetListItemByData(i).Data := Pointer(PtrInt(i - 1));
            {$HINTS ON}
      finally
        if ListView.Selected <> nil then
          ListView.Selected.Free;
        NoSelectedCtrls;
      end;
    end;
  end;
end;

procedure TCryptView.ClickListView(Sender: TObject);
begin
  SelectedUpdate;
end;

procedure TCryptView.SelectedUpdate;
begin
  try
    case ListView.SelCount of
      0: NoSelectedCtrls;
      1:
      begin
        LabelFileName.Caption := SelectedFileItem.Name;
        ImageFile.Picture.LoadFromLazarusResource('file_locked');
        LabelFileDesc.Caption :=
          ' ' + VIEW_CREATION_DATE + #9 + DateTimeToStr(
          FileDateToDateTime(FileAge(SelectedFileItem.TarPath))) +
          #13 + #10 + GetSizeCorrect(FileSize(SelectedFileItem.TarPath));
        EnabledButtons;
      end
      else
      begin
        NoSelectedCtrls;
        LabelFileName.Caption := VIEW_SEVERAL_SELECT;
      end;
    end;
  except
  {$IFDEF UNIX}
    if not IsSelected then
      Exit;
  {$ENDIF UNIX}
  end;
end;

function TCryptView.CreateListView: boolean;
begin
  Result := True;
  try
    ListView := TListView.Create(Self as TWinControl);
    ListView.Parent := Self as TWinControl;
    ListView.Align := alClient;
    ListView.OnClick := @ClickListView;
    ListView.MultiSelect := True;
  except
    Result := False;
  end;
end;

function TCryptView.CreateRightPanel: boolean;
begin
  Result := True;
  try
    // Panel
    PanelRight := TPanel.Create(Self as TWinControl);
    PanelRight.Parent := self as TWinControl;
    PanelRight.Align := alRight;
    PanelRight.Constraints.MinWidth := 240;
    // Control Buttons
    BtnDecrypt := TBitBtn.Create(PanelRight);
    with BtnDecrypt do
    begin
      Parent := PanelRight;
      Top := (Self as TWinControl).Height - 44;
      Left := 4;
      Height := 42;
      Width := 42;
      Anchors := [akLeft, akBottom];
      Glyph.LoadFromLazarusResource('decrypt');
      OnClick := @ClickDecryptAndSave;
    end;
    BtnPreview := TBitBtn.Create(PanelRight);
    with BtnPreview do
    begin
      Parent := PanelRight;
      Top := (Self as TWinControl).Height - 44;
      Left := 48;
      Height := 42;
      Width := 42;
      Anchors := [akLeft, akBottom];
      Glyph.LoadFromLazarusResource('preview');
      OnClick := @ClickPreview;
    end;
    BtnDelete := TBitBtn.Create(PanelRight);
    with BtnDelete do
    begin
      Parent := PanelRight;
      Top := (Self as TWinControl).Height - 44;
      Left := (Self as TWinControl).Width - 44;
      Height := 42;
      Width := 42;
      Anchors := [akRight, akBottom];
      Glyph.LoadFromLazarusResource('delete');
      OnClick := @ClickDelete;
    end;
    // File Description
    LabelFileDesc := TLabel.Create(PanelRight);
    with LabelFileDesc do
    begin
      Parent := PanelRight;
      Constraints.MinHeight := 30;
      Align := alTop;
      Alignment := taLeftJustify;
      Layout := tlCenter;
      Caption := VIEW_NO_SELECTED;
      Update;
    end;
    // File Name
    LabelFileName := TLabel.Create(PanelRight);
    with LabelFileName do
    begin
      Parent := PanelRight;
      Font.Style := [fsBold];
      Constraints.MinHeight := 30;
      Align := alTop;
      Alignment := taCenter;
      Layout := tlCenter;
      Caption := VIEW_NO_SELECTED;
      Update;
    end;
    // File Image
    ImageFile := TImage.Create(PanelRight);
    with ImageFile do
    begin
      Parent := PanelRight;
      Constraints.MinHeight := 180;
      Align := alTop;
      Center := True;
      Picture.LoadFromLazarusResource('file_locked');
      Update;
    end;
  except
    Result := False;
  end;
end;

function TCryptView.GetFileInfoItem: TKeyInfoItem;
begin
  {$HINTS OFF}
  {$WARNINGS OFF}
  if ListView.Selected <> nil then
    Result := KeyStore.Item[PtrInt(ListView.Selected.Data)]
  else
    Result.KeySize := -1;
  {$WARNINGS ON}
  {$HINTS ON}
end;

function TCryptView.GetSelected: boolean;
begin
  Result := not ((SelectedFileItem.KeySize = -1) or (ListView.Selected = nil));
end;

procedure TCryptView.SetLastError(AValue: TViewError);
begin
  if FLastError = AValue then
    Exit;
  FLastError := AValue;
end;

constructor TCryptView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LastError := [veNoError];
  // Для работы тестов нужно определить {$DEFINE TEST}
  TestFileStore;
  TestKeyStore;
  TestConfig;
  // Код инициализации
  if CreateListView and CreateRightPanel then
  begin
    Caption := VIEW_INIT;
    Config := TConfigReader.Create;
    if Config.LastError = [ceWrongPassword] then
    begin
      LastError := [veSystemError];
      NoSelectedCtrls;
      exit;
    end;
    FileStore := TFileStore.Create(Config.PathToSecretDir);
    FileStore.Test;
    KeyStore := TKeyStore.Create(Config.Password);
    KeyStore.LoadFromFile(Config.PathToFilesList);
    KeyStore.UpdateListView(ListView);
    NoSelectedCtrls;
  end;
end;

destructor TCryptView.Destroy;
begin
  PanelRight.FreeOnRelease;
  if Assigned(KeyStore) then
  begin
    if KeyStore.LastError = [keNoError] then
      KeyStore.SaveToFile(Config.PathToFilesList);
    FreeAndNil(KeyStore);
  end;
  if Assigned(FileStore) then
    FreeAndNil(FileStore);
  if Assigned(Config) then
    FreeAndNil(Config);
  FreeAndNil(ListView);
  FreeAndNil(BtnDecrypt);
  FreeAndNil(BtnPreview);
  FreeAndNil(BtnDelete);
  FreeAndNil(LabelFileDesc);
  FreeAndNil(LabelFileName);
  FreeAndNil(ImageFile);
  inherited Destroy;
end;

initialization
  {$I images.lrs}

end.

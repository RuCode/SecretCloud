{
        Класс отвечающий за хранение структур с информацией о файлах и ключах

}

unit KeyStores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EngineBlowFish, ComCtrls, Dialogs, AppStrings;

const
  MAX_KEY_SIZE = 128;

type

  LongString = array[0..2048] of char;

  TKeyInfoItem = record
    GUID: array[0..37] of char;
    Image: integer;
    Name: ShortString;
    TarPath: LongString;
    Key: array[0..MAX_KEY_SIZE] of char;
    KeySize: integer;
  end;
  PKeyInfoItem = ^TKeyInfoItem;

  { TKeyStore }

  TKeyStore = class(TObject)
  private const
    ERROR_RANGE = 'Выход индекса за пределы массива';
  public type
    TKeyError = set of (keNoError, keErrorFindFile);
  private
    fFiles: array of TKeyInfoItem;
    FLastError: TKeyError;
    fPassword: string;
    function GetFileInfo(Index: integer): TKeyInfoItem;
    procedure SetFileInfo(Index: integer; AValue: TKeyInfoItem);
    procedure SetLastError(AValue: TKeyError);
    procedure SetPassword(AValue: string);
  public
    constructor Create(BlowKey: string); virtual;
    destructor Destroy; override;
  public
    // !!! Вывести список файлов в компонент ListView
    procedure UpdateListView(ListView: TListView);
    procedure Add(FileInfo: TKeyInfoItem);           // Добавление новой записи о файле
    procedure Delete(Index: integer);                // Удаление записи по индексу
    procedure SaveToFile(FileName: string);          // Сохранение массива в файл
    procedure LoadFromFile(FileName: string);        // Загрузка массива из файла
    function Count: integer;                         // Количество файлов
  public
    // Структуры
    property Item[Index: integer]: TKeyInfoItem read GetFileInfo write SetFileInfo;
    // Пароль для расшифровки и шифровки записей
    property Password: string read FPassword write SetPassword;
    // Последняя ошибка в удобочитаемом виде
    property LastError: TKeyError read FLastError write SetLastError;
  end;

implementation

{ TKeyStore }

function TKeyStore.GetFileInfo(Index: integer): TKeyInfoItem;
begin
  if (Index < 0) or (Index > Count) then
    raise Exception.Create(ERROR_RANGE);
  Result := fFiles[Index];
end;

procedure TKeyStore.SetFileInfo(Index: integer; AValue: TKeyInfoItem);
begin
  if (Index < 0) or (Index > Count) then
    raise Exception.Create(ERROR_RANGE);
  fFiles[Index] := AValue;
end;

procedure TKeyStore.SetLastError(AValue: TKeyError);
begin
  if FLastError = AValue then
    Exit;
  FLastError := AValue;
end;

procedure TKeyStore.SetPassword(AValue: string);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
end;

constructor TKeyStore.Create(BlowKey: string);
begin
  inherited Create;
  SetLength(fFiles, 0);
  fPassword := BlowKey;
  LastError := [keNoError];
end;

destructor TKeyStore.Destroy;
begin
  SetLength(fFiles, 0);
  inherited Destroy;
end;

procedure TKeyStore.UpdateListView(ListView: TListView);
var
  i: integer;
  ListItem: TListItem;
begin
  LastError := [keNoError];
  for i := 0 to Count do
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := Item[i].Name;
    ListItem.ImageIndex := Item[i].Image;
  {$HINTS OFF}
    ListItem.Data := Pointer(PtrInt(i));
  {$HINTS ON}
  end;
end;

procedure TKeyStore.Add(FileInfo: TKeyInfoItem);
begin
  LastError := [keNoError];
  SetLength(fFiles, Count + 2);
  fFiles[Count] := FileInfo;
end;

procedure TKeyStore.Delete(Index: integer);
var
  i: integer;
begin
  LastError := [keNoError];
  if (Index < 0) or (Index > Count) then
    raise Exception.Create(ERROR_RANGE);
  if Count > 0 then
    for i := Index to Count - 1 do
      fFiles[i] := fFiles[i + 1];
  SetLength(fFiles, Count);
end;

procedure TKeyStore.SaveToFile(FileName: string);
var
  hFile: file of TKeyInfoItem;
  PKeyBuf: PKeyInfoItem;
  EngineBF: TCustomBFEngine;
  i: integer;
begin
  LastError := [keNoError];
  AssignFile(hFile, UTF(FileName));
  Rewrite(hFile);
  EngineBF := TCustomBFEngine.Create;
  EngineBF.SetKey(fPassword);
  GetMem(PKeyBuf, SizeOf(TKeyInfoItem));
  for i := 0 to Count do
  begin
    PKeyBuf^ := Item[i];
    EngineBF.Encrypt(PKeyBuf, SizeOf(TKeyInfoItem), PKeyBuf);
    Write(hFile, PKeyBuf^);
  end;
  FreeMem(PKeyBuf);
  EngineBF.Free;
  CloseFile(hFile);
end;

procedure TKeyStore.LoadFromFile(FileName: string);
var
  hFile: file of TKeyInfoItem;
  PKeyBuf: PKeyInfoItem;
  EngineBF: TCustomBFEngine;
begin
  LastError := [keNoError];
  if not FileExists(FileName) then
  begin
    LastError := [keErrorFindFile];
    exit;
  end;
  AssignFile(hFile, FileName);
  Reset(hFile);
  EngineBF := TCustomBFEngine.Create;
  EngineBF.SetKey(fPassword);
  GetMem(PKeyBuf, SizeOf(TKeyInfoItem));
  while not EOF(hFile) do
  begin
    Read(hFile, PKeyBuf^);
    EngineBF.Decrypt(PKeyBuf, SizeOf(TKeyInfoItem), PKeyBuf);
    Add(PKeyBuf^);
  end;
  FreeMem(PKeyBuf);
  EngineBF.Free;
  CloseFile(hFile);
end;

function TKeyStore.Count: integer;
begin
  Result := High(fFiles);
end;

end.

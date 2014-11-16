{
        Класс отвечающий за шифрование, расшифровку файлов
}

unit FileStores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, KeyStores, LibTar, MemoryFiles, BlowFish, AppStrings,
  EngineBlowFish, FileUtil;

type
  TBlowFishEngine = TCustomBFEngine;

const
  EXT_TAR = '.TAR';
  CONFIG_NAME = 'A1B3-2221FCFFAC34';
  DATA_NAME = 'F084B006-58EF-4C9E';

type

  { TFileStore }

  TFileStore = class(TObject)
  private
    // Путь сохранения
    fPath: string;
    // Получения индекса иконки по расширению файла
    function GetFileImageIndex(FileName: string): integer;
    // Создание GUID
    function NewGUID: string;
  public
    constructor Create(DstPath: string); virtual;
    destructor Destroy; override;
  public
    // Положить файл в галерею
    function PutFile(FileName: string): TKeyInfoItem;
    // Положить папку
    function PutFolder(FileName: string): TKeyInfoItem;
    // Взять файл из галереи
    function GetFile(FileName: string; AKey: string): TMemoryFile;
    // Получить директорию
    function GetFolder(FileName: string; AKey: string): TMemoryFile;
    procedure Test;
  end;

function GetSizeCorrect(Size: integer): string;

implementation

function GetSizeCorrect(Size: integer): string;
const
  MSG = ' Размер файла: ' + #9;
var
  Buffer: integer;
begin
  Buffer := Size;
  if Buffer <= 1024 then
  begin
    Result := MSG + IntToStr(Buffer) + ' байт';
    exit;
  end;
  Buffer := Buffer div 1024;
  if Buffer <= 1024 then
  begin
    Result := MSG + IntToStr(Buffer) + ' Кбайт';
    exit;
  end;
  Buffer := Buffer div 1024;
  if Buffer <= 1024 then
  begin
    Result := MSG + IntToStr(Buffer) + ' Мбайт';
    exit;
  end;
  Buffer := Buffer div 1024;
  if Buffer <= 1024 then
  begin
    Result := MSG + IntToStr(Buffer) + ' Гбайт';
    exit;
  end;
end;

{ TGallery }

function TFileStore.GetFileImageIndex(FileName: string): integer;
begin
  Result := 0;
  case ExtractFileExt(FileName) of
    '.doc': Result := 0;
  end;
end;

function TFileStore.NewGUID: string;
var
  Res: HResult;
  Uid: TGuid;
begin
  Result := '';
  Res := CreateGUID(Uid);
  if Res = S_OK then
    Result := GUIDToString(Uid);
end;

constructor TFileStore.Create(DstPath: string);
begin
  inherited Create;
  fPath := DstPath;
end;

destructor TFileStore.Destroy;
begin
  inherited Destroy;
end;

function TFileStore.PutFile(FileName: string): TKeyInfoItem;
var
  i: integer;
  Buffer: string;
  Blow: TBlowFishEngine;
  MemoryStream: TMemoryStream;
  TarStream: TFileStream;
  TarWriter: TTarWriter;
  MemoryFile: TMemoryFile;
  SrcFile: TMemoryFile;
  BlowKey: PBlowFishKey;
  CurrTime: TDateTime;
begin
  // Init Result
  with Result do
  begin
    GUID := NewGUID;
    Name := ExtractFileName(FileName);
    Image := GetFileImageIndex(FileName);
    repeat
      Buffer := NewGUID;
      SetLength(Buffer, 14);
      Delete(Buffer, 1, 1);
      Buffer := fPath + Buffer + EXT_TAR;
    until not FileExists(Buffer);
    TarPath := Buffer;
  end;

  // Init
  CurrTime := Now;
  Blow := TBlowFishEngine.Create;
  MemoryStream := TMemoryStream.Create;
  TarStream := TFileStream.Create(Result.TarPath, fmCreate);
  TarWriter := TTarWriter.Create(TarStream);
  Getmem(BlowKey, SizeOf(TBlowFishKey));
  Blow.GenKey(SizeOf(TBlowFishKey));
  MemoryFile := CreateMemoryFile(FileSize(FileName) * 2);

  // Load Source file to Memory
  SrcFile := LoadMemoryFile(FileName);
  try
    MemoryStream.Clear;
    MemoryFile.Length := Blow.Encrypt(SrcFile.Data, SrcFile.Length,
      MemoryFile.Data);
    SaveMemoryFile(MemoryFile, MemoryStream);
    MemoryStream.Seek(0, soBeginning);
  finally
    TarWriter.AddStream(MemoryStream, DATA_NAME, CurrTime);
    TarWriter.AddString(Result.GUID, CONFIG_NAME, CurrTime);
  end;

  // save key
  Result.KeySize := Blow.KeySize;
  for i := 0 to Result.KeySize do
    Result.Key[i] := chr(Blow.BlowFishKey[i]);

  // Delete Files
  CloseMemoryFile(MemoryFile);
  CloseMemoryFile(SrcFile);
  Blow.Free;
  Freemem(BlowKey);
  TarWriter.Free;
  MemoryStream.Free;
  TarStream.Free;
end;

function TFileStore.PutFolder(FileName: string): TKeyInfoItem;
var
  SearchRec: TSearchRec;
  i: integer;
  Buffer: string;
  Blow: TBlowFishEngine;
  MemoryStream: TMemoryStream;
  TarStream: TFileStream;
  TarWriter: TTarWriter;
  MemoryFile: TMemoryFile;
  SrcFile: TMemoryFile;
  BlowKey: PBlowFishKey;
  CurrTime: TDateTime;
begin
  {$WARNING Переписать всё}
  // Init Result
  with Result do
  begin
    GUID := NewGUID;
    Name := ExtractFileName(FileName);
    Image := GetFileImageIndex(FileName);
    repeat
      Buffer := NewGUID;
      SetLength(Buffer, 14);
      Delete(Buffer, 1, 1);
      Buffer := fPath + Buffer + EXT_TAR;
    until not FileExists(Buffer);
    TarPath := Buffer;
  end;
  // Init
  CurrTime := Now;
  Blow := TBlowFishEngine.Create;
  MemoryStream := TMemoryStream.Create;
  TarStream := TFileStream.Create(Result.TarPath, fmCreate);
  TarWriter := TTarWriter.Create(TarStream);
  Getmem(BlowKey, SizeOf(TBlowFishKey));
  Blow.GenKey(SizeOf(TBlowFishKey));
  MemoryFile := CreateMemoryFile(FileSize(FileName) * 2);
  // Сжать директорию в TAR, затем зашифровать
  FindFirst(Utf(FileName + '*.*'), faAnyFile, SearchRec);
  try
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      if SearchRec.Attr <> faDirectory then
      begin
        // Load Source file to Memory
        SrcFile := LoadMemoryFile(FileName + SearchRec.Name);
        try
          MemoryStream.Clear;
          SaveMemoryFile(MemoryFile, MemoryStream);
          MemoryStream.Seek(0, soBeginning);
        finally
          TarWriter.AddStream(MemoryStream, SearchRec.Name, CurrTime);
        end;

      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

function ListFolder(Path: string): TStringList;
var
  SearchRec: TSearchRec;
  TmpList: TStringList;
  i: integer;
begin
  Result := TStringList.Create;
  // Folders
  if FindFirst(Utf(Path + '/*'), faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        Result.Add(Path + SearchRec.Name);
        TmpList := ListFolder(Path + SearchRec.Name + '/');
        for i := 0 to TmpList.Count - 1 do
          Result.Add(TmpList[i]);
        TmpList.Free;
      end;
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
  // Files
  FindFirst(Utf(Path + '*.*'), faAnyFile, SearchRec);
  repeat
    if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
      Continue;
    if (SearchRec.Attr and faDirectory) <> faDirectory then
      Result.Add(Path + SearchRec.Name);
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

procedure AddFileToTar(TarWriter: TTarWriter; FileName: string; ExludePath: string = '');
var
  MemoryFile: TMemoryFile;
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  MemoryFile := LoadMemoryFile(FileName);
  try
    MemoryStream.Clear;
    SaveMemoryFile(MemoryFile, MemoryStream);
    MemoryStream.Seek(0, soBeginning);
  finally
    TarWriter.AddStream(MemoryStream, FileName, Now);
  end;
  MemoryStream.Free;
  CloseMemoryFile(MemoryFile);
end;

function TFileStore.GetFolder(FileName: string; AKey: string): TMemoryFile;
begin
  {$WARNING Написать метод}
end;

procedure TFileStore.Test;
var
  Path: String;
  StringList: TStringList;
  TarStream: TMemoryStream;
  TarWriter: TTarWriter;
  i: Integer;
begin
  Path := ExtractFilePath(ParamStr(0));
  ShowMessage('Пробуем перебрать дирректорию: ' + Path);

  StringList := ListFolder(Path);
  StringList.Sort;

  TarStream := TMemoryStream.Create;
  TarWriter := TTarWriter.Create(TarStream);

  for i := 0 to StringList.Count - 1 do
    AddFileToTar(TarWriter, StringList[i], Path);

  TarStream.SaveToFile('data.tar');

  TarWriter.Free;
  TarStream.Free;

  StringList.Free;
end;

function TFileStore.GetFile(FileName: string; AKey: string): TMemoryFile;
var
  TarRec: TTarDirRec;
  Blow: TBlowFishEngine;
  TarReader: TTarArchive;
  MemoryFile: TMemoryFile;
  MemoryStream: TMemoryStream;
begin
  //////////////////////////////////////////////////////////////////////////////
  // Init
  //////////////////////////////////////////////////////////////////////////////
  Blow := TBlowFishEngine.Create;
  Blow.SetKey(AKey);
  MemoryStream := TMemoryStream.Create;
  TarReader := TTarArchive.Create(FileName, fmOpenRead);
  TarRec.Size := 0;

  //////////////////////////////////////////////////////////////////////////////
  // Read and decrypt Public Rsa Key
  //////////////////////////////////////////////////////////////////////////////
  try
    MemoryStream.Clear;
    TarReader.Reset;
    repeat
      TarReader.FindNext(TarRec);
    until TarRec.Name = DATA_NAME;
    TarReader.ReadFile(MemoryStream);
    Result := MemoryFiles.CreateMemoryFile(TarRec.Size * 2);
    MemoryFile := LoadMemoryFile(MemoryStream);
    Result.Length := Blow.Decrypt(MemoryFile.Data, TarRec.Size, Result.Data);
  finally
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Free memory
  //////////////////////////////////////////////////////////////////////////////
  CloseMemoryFile(MemoryFile);
  MemoryStream.Free;
  TarReader.Free;
  Blow.Free;
end;

end.

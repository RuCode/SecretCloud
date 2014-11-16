unit MemoryFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, AppStrings;

type
  TMemoryFile = record
    Length: int64;
    Data: Pointer;
  end;

function CreateMemoryFile(const iSize: integer): TMemoryFile;

function LoadMemoryFile(const FileName: string): TMemoryFile;
function LoadMemoryFile(FileStream: TMemoryStream): TMemoryFile;
function LoadMemoryFile(Data: Pointer; iLength: int64): TMemoryFile;

function SaveMemoryFile(MemFile: TMemoryFile; const FileName: string): boolean;
function SaveMemoryFile(MemFile: TMemoryFile; FileStream: TMemoryStream): boolean;
function SaveMemoryFile(MemFile: TMemoryFile; var Data: Pointer): boolean;

procedure CloseMemoryFile(MemFile: TMemoryFile);

implementation

function CreateMemoryFile(const iSize: integer): TMemoryFile;
begin
  GetMem(Result.Data, iSize);
  Result.Length := iSize;
end;

function LoadMemoryFile(const FileName: string): TMemoryFile;
var
  hFile: THandle;
  iFileSize: int64;
begin
  hFile := FileOpen(UTF(FileName), fmOpenRead);
  iFileSize := FileSize(FileName);
  GetMem(Result.Data, iFileSize);
  Result.Length := iFileSize;
  FileRead(hFile, Pointer(Result.Data)^, iFileSize);
  FileClose(hFile);
end;

function LoadMemoryFile(FileStream: TMemoryStream): TMemoryFile;
begin
  FileStream.Seek(0, soFromBeginning);
  GetMem(Result.Data, FileStream.Size);
  Result.Length := FileStream.Size;
  FileStream.ReadBuffer(Pointer(Result.Data)^, Result.Length);
end;

function LoadMemoryFile(Data: Pointer; iLength: int64): TMemoryFile;
var
  i: integer;
begin
  GetMem(Result.Data, iLength);
  Result.Length := iLength;
  for i := 0 to iLength do
    PByte(Result.Data + i)^ := PByte(Data + i)^;
end;

function SaveMemoryFile(MemFile: TMemoryFile; const FileName: string): boolean;
var
  hFile: THandle;
begin
  Result := False;
  try
    hFile := FileCreate(UTF(FileName));
    FileWrite(hFile, Pointer(MemFile.Data)^, MemFile.Length);
    FileClose(hFile);
  finally
    Result := True;
  end;
end;

function SaveMemoryFile(MemFile: TMemoryFile; FileStream: TMemoryStream): boolean;
begin
  Result := False;
  try
    FileStream.SetSize(MemFile.Length);
    FileStream.WriteBuffer(Pointer(MemFile.Data)^, MemFile.Length);
  finally
    Result := True;
  end;
end;

function SaveMemoryFile(MemFile: TMemoryFile; var Data: Pointer): boolean;
var
  i: integer;
begin
  Result := False;
  try
    for i := 0 to MemFile.Length do
      PByte(Data + i)^ := PByte(MemFile.Data + i)^;
  finally
    Result := True;
  end;
end;

procedure CloseMemoryFile(MemFile: TMemoryFile);
begin
  Freemem(MemFile.Data);
  MemFile.Length := 0;
end;

end.

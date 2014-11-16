{
        Класс отвечающий за шифрование в ОЗУ
}
unit EngineBlowFish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlowFish, OpenSSL, base64, Dialogs;

const
  MAX_BLOWFISH_KEY = 55;

type

  { TCustomBFEngine }

  TCustomBFEngine = class(TObject)
  private
    fBlowFishKey: TBlowFishKey;
    fKeySize: integer;
    function GetLastError: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    // Генерация ключей
    function GenKey(AKeySize: integer): boolean;
    function SetKey(AKey: string): boolean;
    function SetKey(AKey: TBlowFishKey; AKeySize: integer): boolean;

    // Загрузка и сохранение ключей
    function KeyLoadFromFile(FileName: TFilename): boolean;
    function KeySaveToFile(FileName: TFilename): boolean;

    // Шифровка и расшифровка
    function Encrypt(SrcBuf: Pointer; SrcBufLen: integer;
      var DstBuf: Pointer): integer;
    function Decrypt(SrcBuf: Pointer; SrcBufLen: integer;
      var DstBuf: Pointer): integer;
    procedure SaveKey(var BlowFishKey: TBlowFishKey);
  public
    property BlowFishKey: TBlowFishKey read fBlowFishKey;
    property KeySize: integer read fKeySize;
    property LastError: string read GetLastError;
  end;

function EncodeStringBase64(const Src: string): string;
function DecodeStringBase64(const Src: string): string;
function BlowFishEncryptStr(const Src, AKey: string): string;
function BlowFishDecryptStr(const Src, AKey: string): string;
function BlowFishEncryptStr(const Src: string; AKey: TBlowFishKey;
  KeySize: integer): string;
function BlowFishDecryptStr(const Src: string; AKey: TBlowFishKey;
  KeySize: integer): string;
function BlowFishEncrypt(var SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer; AKey: TBlowFishKey; KeySize: integer): integer;
function BlowFishDecrypt(var SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer; AKey: TBlowFishKey; KeySize: integer): integer;

implementation

function BlowFishEncryptStr(const Src, AKey: string): string;
var
  VInput: TStringStream;
  VBF: TBlowFishEncryptStream;
begin
  VInput := TStringStream.Create('');
  VBF := TBlowFishEncryptStream.Create(AKey, VInput);
  try
    VBF.Write(Pointer(Src)^, Length(Src));
  finally
    VBF.Free;
    Result := VInput.DataString;
    VInput.Free;
  end;
end;

function BlowFishDecryptStr(const Src, AKey: string): string;
var
  VOutput: TStringStream;
  VBF: TBlowFishDeCryptStream;
begin
  VOutput := TStringStream.Create(Src);
  VBF := TBlowFishDeCryptStream.Create(AKey, VOutput);
  try
    SetLength(Result, VOutput.Size);
    VBF.Read(Pointer(Result)^, VOutput.Size);
  finally
    VBF.Free;
    VOutput.Free;
  end;
end;

function BlowFishEncryptStr(const Src: string; AKey: TBlowFishKey;
  KeySize: integer): string;
var
  VInput: TStringStream;
  VBF: TBlowFishEncryptStream;
begin
  VInput := TStringStream.Create('');
  VBF := TBlowFishEncryptStream.Create(AKey, KeySize, VInput);
  try
    VBF.Write(Pointer(Src)^, Length(Src));
  finally
    VBF.Free;
    Result := VInput.DataString;
    VInput.Free;
  end;
end;

function BlowFishDecryptStr(const Src: string; AKey: TBlowFishKey;
  KeySize: integer): string;
var
  VOutput: TStringStream;
  VBF: TBlowFishDeCryptStream;
begin
  VOutput := TStringStream.Create(Src);
  VBF := TBlowFishDeCryptStream.Create(AKey, KeySize, VOutput);
  try
    SetLength(Result, VOutput.Size);
    VBF.Read(Pointer(Result)^, VOutput.Size);
  finally
    VBF.Free;
    VOutput.Free;
  end;
end;

function DecodeStringBase64(const Src: string): string;
var
  Instream, Outstream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  Instream := TStringStream.Create(Src);
  try
    Outstream := TStringStream.Create('');
    try
      Decoder := TBase64DecodingStream.Create(Instream, bdmMIME);
      try
        Outstream.CopyFrom(Decoder, Decoder.Size);
        Outstream.Position := 0;
        Result := Outstream.ReadString(Outstream.Size);
      finally
        Decoder.Free;
      end;
    finally
      Outstream.Free;
    end;
  finally
    Instream.Free;
  end;
end;

function BlowFishEncrypt(var SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer; AKey: TBlowFishKey; KeySize: integer): integer;
var
  VInput: TMemoryStream;
  VBF: TBlowFishEncryptStream;
begin
  VInput := TMemoryStream.Create();
  VBF := TBlowFishEncryptStream.Create(AKey, KeySize, VInput);
  try
    VBF.Write(Pointer(SrcBuf)^, SrcBufLen);
    VBF.Flush;
    VInput.Seek(0, soBeginning);
    VInput.Read(Pointer(DstBuf)^, VInput.Size);
  finally
    VBF.Free;
    Result := VInput.Size;
    VInput.Free;
  end;
end;

function BlowFishDecrypt(var SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer; AKey: TBlowFishKey; KeySize: integer): integer;
var
  VOutput: TMemoryStream;
  VBF: TBlowFishDeCryptStream;
begin
  VOutput := TMemoryStream.Create();
  VOutput.Write(Pointer(SrcBuf)^, SrcBufLen);
  VOutput.Position := 0;
  VBF := TBlowFishDeCryptStream.Create(AKey, KeySize, VOutput);
  try
    VBF.Read(Pointer(DstBuf)^, VOutput.Size);
  finally
    Result := VOutput.Size;
    VBF.Free;
    VOutput.Free;
  end;
end;

function EncodeStringBase64(const Src: string): string;
var
  Outstream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  Outstream := TStringStream.Create('');
  try
    Encoder := TBase64EncodingStream.Create(outstream);
    try
      Encoder.Write(Src[1], Length(Src));
    finally
      Encoder.Free;
    end;
    Outstream.Position := 0;
    Result := Outstream.ReadString(Outstream.Size);
  finally
    Outstream.Free;
  end;
end;

{ TCustomBFEngine }

function TCustomBFEngine.GetLastError: string;
begin
  Result := '';
end;

constructor TCustomBFEngine.Create;
begin
  inherited Create;
end;

destructor TCustomBFEngine.Destroy;
begin
  inherited Destroy;
end;

function TCustomBFEngine.GenKey(AKeySize: integer): boolean;
const
  StrTable: string =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ!";%:?*()_+-=\/|&^%$#@<>.,abcdefghijklmnopqrstuvwxyz0123456789';
var
  i: integer;
  LenTable: integer;
  BufStr: string;
begin
  BufStr := '';
  Result := False;
  try
    LenTable := Length(StrTable) - 1;
    for i := 1 to AKeySize do
      BufStr += StrTable[Random(LenTable) + 1];
    SetKey(BufStr);
  finally
    Result := True;
  end;
end;

function TCustomBFEngine.SetKey(AKey: string): boolean;
var
  i: integer;
begin
  Result := False;
  try
    fKeySize := Length(AKey);
    if fKeySize > 54 then
      fKeySize := 54;
    for i := 0 to fKeySize-1 do
      fBlowFishKey[i] := Ord(AKey[i + 1]);
  finally
    Result := True;
  end;
end;

function TCustomBFEngine.SetKey(AKey: TBlowFishKey; AKeySize: integer): boolean;
var
  i: integer;
begin
  Result := False;
  try
    fKeySize := AKeySize;
    if fKeySize > 54 then
      fKeySize := 54;
    for i := 0 to fKeySize do
      fBlowFishKey[i] := AKey[i];
  finally
    Result := True;
  end;
end;

function TCustomBFEngine.KeyLoadFromFile(FileName: TFilename): boolean;
var
  hFile: file of TBlowFishKey;
begin
  Result:= False;
  try
  AssignFile(hFile, FileName);
  Reset(hFile);
  Read(hFile, fBlowFishKey);
  CloseFile(hFile);
  finally
    Result:= True;
  end;
end;

function TCustomBFEngine.KeySaveToFile(FileName: TFilename): boolean;
var
  hFile: file of TBlowFishKey;
begin
  Result:= False;
  AssignFile(hFile, FileName);
  Rewrite(hFile);
  Write(hFile, fBlowFishKey);
  CloseFile(hFile);
end;

function TCustomBFEngine.Encrypt(SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer): integer;
begin
  Result := BlowFishEncrypt(SrcBuf, SrcBufLen, DstBuf, fBlowFishKey, KeySize);
end;

function TCustomBFEngine.Decrypt(SrcBuf: Pointer; SrcBufLen: integer;
  var DstBuf: Pointer): integer;
begin
  Result := BlowFishDecrypt(SrcBuf, SrcBufLen, DstBuf, fBlowFishKey, KeySize);
end;

procedure TCustomBFEngine.SaveKey(var BlowFishKey: TBlowFishKey);
begin
  BlowFishKey := fBlowFishKey;
end;

end.

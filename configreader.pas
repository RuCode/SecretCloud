{
        Класс отвечающий за чтение, запись и модификацию конфигурации

}

unit ConfigReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, ConfigForms, Dialogs, Controls, sha1, AppStrings;

const
  SECTION_MAIN = 'MAIN';
  IDENT_SECRET_DIR = 'SECRET_DIR';
  IDENT_FILES_LIST = 'FILES_LIST';
  IDENT_HASH = 'HASH_PASSWD';

type

  { TConfigReader }

  TConfigReader = class
  public type
    TConfigError = set of (ceNoError, ceWrongPassword);
  private
    FHash: string;
    fConfigFile: string;
    FLastError: TConfigError;
    FPassword: string;
    FPathToFilesList: string;
    FPathToSecretDir: string;
    function GetIsConfigExists: boolean;
    procedure SetLastError(AValue: TConfigError);
    procedure SetPassword(AValue: string);
    procedure SetPathToFilesList(AValue: string);
    procedure SetPathToSecretDir(AValue: string);
    procedure LoadDataPaths;
    procedure SaveDataPaths;
    procedure AlwaysCreateSecretDir;
    {** PASSWORD **}
    function InitPassword: boolean;
    // Был ли введён пароль и записан в файл конфигурации? ====================
    function IsPasswordExists: boolean;
    // Ввод пароля в первый раз ===============================================
    function EnterPasswordFirst: boolean;
    // Ввод пароля ============================================================
    function EnterPassword: boolean;
    // Проверка пароля ========================================================
    function ValidatePassword: boolean;
    // Сохранение пароля в файл конфигурации ==================================
    function SavePasswordToFile: boolean;
    // Загрузка пароля из файла конфигурации ==================================
    function LoadPasswordFromFile: boolean;
    procedure UpdatePasswordHash;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property IsConfigExists: boolean read GetIsConfigExists;
    // Путь к файлу настроек (не настраивается)
    property PathToConfigFile: string read fConfigFile;
    // Путь к зашифрованным файлам
    property PathToSecretDir: string read FPathToSecretDir write SetPathToSecretDir;
    // Путь к списку файлов и ключей к ним
    property PathToFilesList: string read FPathToFilesList write SetPathToFilesList;
    // Пароль от списка ключей
    property Password: string read FPassword write SetPassword;
    property LastError: TConfigError read FLastError write SetLastError;
  end;

implementation

{ TConfigReader }

procedure TConfigReader.SetPassword(AValue: string);
begin
  if FPassword = AValue then
    Exit;
  FPassword := AValue;
end;

function TConfigReader.GetIsConfigExists: boolean;
begin
  Result := FileExists(PathToConfigFile);
end;

procedure TConfigReader.SetLastError(AValue: TConfigError);
begin
  if FLastError = AValue then
    Exit;
  FLastError := AValue;
end;

procedure TConfigReader.SetPathToFilesList(AValue: string);
begin
  if FPathToFilesList = AValue then
    Exit;
  FPathToFilesList := AValue;
  SaveDataPaths;
end;

procedure TConfigReader.SetPathToSecretDir(AValue: string);
begin
  if FPathToSecretDir = AValue then
    Exit;
  FPathToSecretDir := AValue;
  SaveDataPaths;
end;

procedure TConfigReader.LoadDataPaths;
var
  IniFile: TIniFile;
begin
  if IsConfigExists then
  begin
    IniFile := TIniFile.Create(PathToConfigFile);
    FPathToSecretDir := IniFile.ReadString(SECTION_MAIN, IDENT_SECRET_DIR, '');
    FPathToFilesList := IniFile.ReadString(SECTION_MAIN, IDENT_FILES_LIST, '');
    IniFile.Free;
    AlwaysCreateSecretDir;
  end;
end;

procedure TConfigReader.SaveDataPaths;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(UTF(PathToConfigFile));
  IniFile.WriteString(SECTION_MAIN, IDENT_SECRET_DIR, PathToSecretDir);
  IniFile.WriteString(SECTION_MAIN, IDENT_FILES_LIST, PathToFilesList);
  IniFile.Free;
  AlwaysCreateSecretDir;
end;

procedure TConfigReader.AlwaysCreateSecretDir;
begin
  CreateDir(ExtractFileDir(PathToSecretDir));
end;

constructor TConfigReader.Create;
begin
  LastError := [ceNoError];
  fPassword := '';
  FHash := '';
  fConfigFile := GetUserFolder + 'secret.conf';
  if IsConfigExists then
    LoadDataPaths;
  InitPassword;
end;

destructor TConfigReader.Destroy;
begin
  SaveDataPaths;
  inherited Destroy;
end;

{// Наличие конфига
*************************************************************************************
**  ЗАДАЧА: ВВОД И ВАЛИДАЦИЯ ПАРОЛЯ                                                **
*************************************************************************************
 1) Изначально, при первом запуске приложения, должен быть введён пароль и записан его
 sha1 в конфиг.файл. Соответственно алгоритм такой:
    1.1) Проверить был ли введён пароль до этого
    1.2) Записать в конфиг.файл sha1
    1.3) Проверять в программе sha1 текущего пароля и сравнивать с эталонным
 2) При повторном запуске приложения
    2.1) Проверить был ли введён пароль до этого
    2.2) Попросить пользователя ввести пароль
    2.3) Проверить sha1

*************************************************************************************}
function TConfigReader.InitPassword: boolean;
begin
  Result := False;
  // Пароль уже был введён ранее
  if IsPasswordExists then
    if EnterPassword then
    begin
      SavePasswordToFile;
      Result := True;
      Exit;
    end
    else
    begin
      LastError := [ceWrongPassword];
      Exit;
    end;
  // Пароля не существовало
  if not IsPasswordExists then
    if EnterPasswordFirst then
    begin
      SavePasswordToFile;
      Result := True;
    end
    else
      Exit;
end;

function TConfigReader.IsPasswordExists: boolean;
begin
  Result := LoadPasswordFromFile;
end;

function TConfigReader.EnterPasswordFirst: boolean;
begin
  Password := '';
  Result := False;
  try
    ConfigForm := TConfigForm.Create(nil);
    if ConfigForm.ShowModal = mrOk then
    begin
      PathToSecretDir := ConfigForm.PathToSecretDir;
      PathToFilesList := ConfigForm.PathToFilesList;
      Password := ConfigForm.Password;
      UpdatePasswordHash;
      AlwaysCreateSecretDir;
    end;
  finally
    if Password <> '' then
      Result := True;
    ConfigForm.Free;
  end;
end;

function TConfigReader.EnterPassword: boolean;
const
  MAX_ATTEMPTS = 2;
var
  PasswordAttempts: integer;
begin
  Result := False;
  for PasswordAttempts := MAX_ATTEMPTS downto 0 do
  begin
    Password := PasswordBox(DIALOG_ENTER, TEXT_ENTER_PASSWORD);
    if Password = '' then
    begin
      MessageDlg(DIALOG_WARNING, (TEXT_PASSWORD_WRONG + IntToStr(PasswordAttempts)),
        mtWarning, [mbOK], '');
      Continue;
    end;
    if SHA1Print(SHA1String(Password)) = FHash then
    begin
      Result := True;
      UpdatePasswordHash;
      break;
    end
    else
      MessageDlg(DIALOG_WARNING, (TEXT_PASSWORD_WRONG + IntToStr(PasswordAttempts)),
        mtWarning, [mbOK], '');
  end;
end;

function TConfigReader.ValidatePassword: boolean;
begin
  Result := (SHA1Print(SHA1String(Password)) = FHash);
end;

function TConfigReader.SavePasswordToFile: boolean;
var
  IniFile: TIniFile;
begin
  Result := False;
  try
    IniFile := TIniFile.Create(UTF(PathToConfigFile));
    IniFile.WriteString(SECTION_MAIN, IDENT_HASH, SHA1Print(SHA1String(Password)));
  finally
    IniFile.Free;
    Result := True;
  end;
end;

function TConfigReader.LoadPasswordFromFile: boolean;
var
  IniFile: TIniFile;
begin
  Result := False;
  if IsConfigExists then
  begin
    try
      IniFile := TIniFile.Create(PathToConfigFile);
      FHash := IniFile.ReadString(SECTION_MAIN, IDENT_HASH, '');
    finally
      if FHash <> '' then
        Result := True;
      IniFile.Free;
    end;
  end;
end;

procedure TConfigReader.UpdatePasswordHash;
begin
  FHash := SHA1Print(SHA1String(Password));
  SavePasswordToFile;
end;

end.
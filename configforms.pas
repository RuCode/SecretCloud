{
        Форма на которой мы можем настроить конфигурацию

}

unit ConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    EditJailFiles: TLabeledEdit;
    EditJailKeys: TLabeledEdit;
    EditPassword: TLabeledEdit;
    SaveDialog: TSaveDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure SaveConfig;
  public
    { public declarations }
    PathToSecretDir: string;
    PathToFilesList: string;
    Password: string;
  end;

var
  ConfigForm: TConfigForm;

function GetUserFolder: string;

implementation

{$R *.lfm}

function GetUserFolder: string;
begin
  {$IFDEF WINDOWS}
  Result := GetTempDir;
  SetLength(Result, Length(Result) - 1);
  Result := ExtractFilePath(Result) + 'SecretCloud\';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  Result := GetUserDir + '.secret/';
  {$ENDIF UNIX}
end;

{ TConfigForm }

procedure TConfigForm.FormShow(Sender: TObject);
begin
  EditJailFiles.Text := GetUserFolder +
  {$IFDEF WINDOWS}
  'cloud\';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  'cloud/';
  {$ENDIF UNIX}
  EditJailKeys.Text := GetUserFolder + 'jail.sc';
  PathToFilesList := '';
  PathToSecretDir := '';
  Password := '';
end;

procedure TConfigForm.SaveConfig;
begin
  PathToSecretDir:= EditJailFiles.Text;
  PathToFilesList:= EditJailKeys.Text;
  Password:= EditPassword.Text;
end;

procedure TConfigForm.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    EditJailFiles.Text := SelectDirectoryDialog.FileName;
end;

procedure TConfigForm.Button2Click(Sender: TObject);
begin
  if SaveDialog.Execute then
    EditJailKeys.Text := SaveDialog.FileName;
end;

procedure TConfigForm.Button4Click(Sender: TObject);
begin
  SaveConfig;
end;

end.


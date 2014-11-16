unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, CryptViewCtrls, AppStrings;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuAdd: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure MenuAddClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CryptView: TCryptView;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  CryptView := TCryptView.Create(MainForm);
  CryptView.Parent := MainForm;
  CryptView.Align := alClient;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseAction = caFree then
    CryptView.Free;
end;

procedure TMainForm.FormDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  if veSystemError in CryptView.LastError then
    Accept := False
  else
    Accept := True;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  FilesIndex: integer;
begin
  if veSystemError in CryptView.LastError then
    exit;
  for FilesIndex := Low(FileNames) to High(FileNames) do
    CryptView.Add(FileNames[FilesIndex]);
end;

procedure TMainForm.MenuAddClick(Sender: TObject);
begin
  if veSystemError in CryptView.LastError then
  begin
    MessageDlg(DIALOG_ERROR, TEXT_DISABLED_PASS, mtWarning, [mbOK], '');
    exit;
  end;
  if OpenDialog.Execute then
    CryptView.Add(OpenDialog.FileName);
end;

end.

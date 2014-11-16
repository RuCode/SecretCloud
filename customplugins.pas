unit CustomPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, dynlibs;

const
  PLUGIN_GETTYPE = 'GetPluginType';


type
  TGetTypeProc = function():Integer;

type
  TPluginItem = class
    Name: ShortString;
  end;

type

  { TCustomPluginList }

  TCustomPluginList = class
    private
      fList: specialize TFPGList<TPluginItem>;
      function GetItem(i: Integer): TPluginItem;
      procedure SetItem(i: Integer; AValue: TPluginItem);
    public
      procedure Add(Plugin: TPluginItem);
      procedure Clear;
    public
      property Item[i: Integer]: TPluginItem read GetItem write SetItem;
  end;

implementation

{ TCustomPluginList }

function TCustomPluginList.GetItem(i: Integer): TPluginItem;
begin
   Result:= fList.Items[i];
end;

procedure TCustomPluginList.SetItem(i: Integer; AValue: TPluginItem);
begin
   fList.Items[i]:= AValue;
end;

procedure TCustomPluginList.Add(Plugin: TPluginItem);
var
  hLib: TLibHandle;
  GetTypeProc: TGetTypeProc;
begin
   hLib:= LoadLibrary(Plugin.Name);
   //@GetTypeProc:= GetProcAddress(hLib, PLUGIN_GETTYPE);
   fList.Add(Plugin);
end;

procedure TCustomPluginList.Clear;
begin
   fList.Clear;
end;

end.


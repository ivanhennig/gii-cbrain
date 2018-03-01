unit ufunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, superobject, ufunctionssys, LazFileUtils;

{ TJson }
type
  TJson = class
  private
    function GetB(const path: SOString): boolean;
    function GetD(const path: SOString): Double;
    function GetI(const path: SOString): SuperInt;
    function GetO(const path: SOString): ISuperObject;
    function GetS(const path: SOString): SOString;
    procedure PutB(const path: SOString; AValue: boolean);
    procedure PutD(const path: SOString; AValue: Double);
    procedure PutI(const path: SOString; AValue: SuperInt);
    procedure PutO(const path: SOString; AValue: ISuperObject);
    procedure PutS(const path: SOString; AValue: SOString);
  public
    Instance : ISuperObject;
    FPath    : String;
    FChanged : Boolean;
    function Exists(k : String) : Boolean;
    function AsJson : String;
    constructor Create(); overload;
    constructor Create(aSO : ISuperObject); overload;
    destructor Destroy; override;
    property O[const path: SOString]: ISuperObject read GetO write PutO; default;
    property B[const path: SOString]: boolean read GetB write PutB;
    property I[const path: SOString]: SuperInt read GetI write PutI;
    property D[const path: SOString]: Double read GetD write PutD;
    property S[const path: SOString]: SOString read GetS write PutS;
  end;

  PJson = ^TJson;

  TIntArray = array of Integer;

procedure array_push(var aArray : TIntArray; aVal : Integer);
function array_search(aVal : Integer; aArray : TIntArray) : Integer;
function in_array(aVal : Integer; aArray : TIntArray) : Boolean;
function getSettings: ISuperObject;

implementation

function getSettings: ISuperObject;
begin
  if (FileExistsUTF8(Common.UserDir + '.giiCBRain.json')) then
  begin
    Result := TSuperObject.ParseFile(Common.UserDir + '.giiCBRain.json', False);
  end else
  begin
    Result := SO('{"pathmongod":"mongodb\\bin\\mongod.exe","pathphp": "php\\php.exe","uselocalservice": true,"rpcurl": "http://127.0.0.1:8080","rpcport": 8080}');
  end;
end;
procedure array_push(var aArray : TIntArray; aVal : Integer);
var
  lLen : Integer;
begin
  lLen:=Length(aArray);
  SetLength(aArray, lLen + 1);
  aArray[lLen]:=aVal;
end;

function array_search(aVal : Integer; aArray : TIntArray) : Integer;
var i : Integer;
begin
  Result:=-1;
  for i:= Low(aArray) to High(aArray) do
  begin
    if (aArray[i]=aVal) then
    begin
      Result:=i;
      Break;
    end;
  end;
end;
function in_array(aVal : Integer; aArray : TIntArray) : Boolean;
begin
  Result:= array_search(aVal,aArray)>=0;
end;

{ TJson }

function TJson.GetB(const path: SOString): boolean;
begin
  Result:=Instance.GetB(path);
end;

function TJson.GetD(const path: SOString): Double;
begin
  Result:=Instance.GetD(path);
end;

function TJson.GetI(const path: SOString): SuperInt;
begin
  Result:=Instance.GetI(path);
end;

function TJson.GetO(const path: SOString): ISuperObject;
begin
  Result:=Instance.GetO(path);
end;

function TJson.GetS(const path: SOString): SOString;
begin
  Result:=Instance.GetS(path);
end;

procedure TJson.PutB(const path: SOString; AValue: boolean);
begin
  Instance.PutB(path,AValue);
end;

procedure TJson.PutD(const path: SOString; AValue: Double);
begin
  Instance.PutD(path,AValue);
end;

procedure TJson.PutI(const path: SOString; AValue: SuperInt);
begin
  Instance.PutI(path,AValue);
end;

procedure TJson.PutO(const path: SOString; AValue: ISuperObject);
begin
  Instance.PutO(path,AValue);
end;

procedure TJson.PutS(const path: SOString; AValue: SOString);
begin
  Instance.PutS(path,AValue);
end;

function TJson.Exists(k: String): Boolean;
begin
  Result:=Instance.GetO(k) <> nil;
end;

function TJson.AsJson: String;
begin
  Result:=Instance.AsJSon();
end;

constructor TJson.Create;
begin
  Instance := SO();
end;

constructor TJson.Create(aSO: ISuperObject);
begin
  if (Assigned(aSO)) then
  begin
    Instance := aSO;
  end else
  begin
    Instance := SO();
  end;
end;

destructor TJson.Destroy;
begin
  Instance._Release;
  inherited Destroy;
end;


end.


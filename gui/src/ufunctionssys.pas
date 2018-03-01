unit ufunctionssys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IfDef LINUX}
    ,users
    ,baseunix
  {$EndIf}
  ,lazutf8;

type

  { Common }

  Common = class
    public
      class function UserDir : String;
      class function UserName : String;
  end;

implementation


{ Common }

class function Common.UserDir: String;
begin
  {$IFDEF LINUX}
    Result := GetEnvironmentVariable('HOME') + '/';
  {$ENDIF}
  {$IFDEF WINDOWS}
    Result := GetUserDir();
  {$ENDIF}
end;

class function Common.UserName: String;
begin
  Result := '';
  {$IFDEF WINDOWS}
  Result:='x';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
    {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
    Result := SysToUtf8(GetUserName(fpgetuid));
    {$ENDIF}
    if Result = '' then
      Result := GetEnvironmentVariableUTF8('USER'); //fallback or other unixes which export $USER and don't support GetUserName
  {$ENDIF}
end;

end.


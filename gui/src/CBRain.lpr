program CBRain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  udefaulttranslator,//Versão modificada
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, memdslaz, umain, ustrings,
  ucommon, usettings, ucommongrid, ucase, uattribute, ufunctions, superobject,
  udglsymboledit, uproject, udglsymboleditordered, ujsonrpc, udlgnumberedit,
  uinputform, udlgmetaphoneoptions, udlgsymboleditunordered, ucaseresult, 
ureuseform, ufunctionssys, udlgnumberstepfunction;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.


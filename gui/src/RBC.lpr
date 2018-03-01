program RBC;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  udefaulttranslator,//Versão modificada
  Interfaces, // this includes the LCL widgetset
  Forms, virtualtreeview_package, lazcontrols, memdslaz, umain, ustrings,
  ucommon, usettings, ucommongrid, ucase, uattribute, ufunctions, superobject,
  udglsymboledit, uproject, udglsymboleditordered, ujsonrpc, udlgnumberedit,
  uinputform, udlgmetaphoneoptions, udlgsymboleditunordered, ucaseresult, 
ureuseform, ufunctionssys, udlgnumberstepfunction;

{$R *.res}

begin
  Application.Title:='CBRain';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.


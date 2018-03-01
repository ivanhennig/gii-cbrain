unit udefaulttranslator;

{$mode objfpc}{$H+}

interface

uses
  LCLTranslator, LazFileUtils;

implementation

initialization
  if (DirectoryExistsUTF8('languages')) then
  begin
    SetDefaultLang('', 'languages', false);
  end else if (DirectoryExistsUTF8('../../languages')) then //Desenvolvimento
  begin
    SetDefaultLang('', '../../languages', false);
  end else
  begin
    //Não precisa detectar...
  end;


end.

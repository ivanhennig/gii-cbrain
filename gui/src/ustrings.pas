unit ustrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring
  SSelectOne = '-- Select one --';
  SDisabled = '-- Disabled --';
  SDefault = '-- Default --';

  SValidation = 'Validation';
  SValidationMessageEmpty  = 'Field "%s" must have a value';
  SValidationNumberGreat   = 'Use a number greater than "%s" in "%s"';
  SValidationNumberLess    = 'Use a number lesser than "%s" in "%s"';

  SValidationWeight = 'Field "Weight" must have a value between 0.1 and 1.0';

  STypeInput = 'Input';
  STypeOutput = 'Output';
  STypeInputOutput = 'Input and output';

  SDataTypeString = 'String';
  SDataTypeNumber = 'Number';
  SDataTypeSymbol = 'Symbol';
  SDataTypeSymbolOrdered   = 'Ordered symbol';
  SDataTypeSymbolUnordered = 'Unordered symbol';
  SDataTypePhrase = 'Phrase';

  SParamSymbolInfo = 'Symbols';
  SParamSymbolOrderedInfo = 'Ordered symbols';
  SParamSymbolUnorderedInfo = 'Unordered symbols ';
  SParamMetaphoneInfo = 'Metaphone language';
  SParamNumberLinearInfo = 'Linear function options';
  SParamNumberStepInfo = 'Step function options';
  SParamNoOptions = 'No options';

  SNoResults = 'No results';

  SYes = 'Yes';
  SNo = 'No';

  SOKRetrieve = 'Retrieve cases';
  SOKReuse = 'Reuse case';
  SOKSaveSettings = 'Save settings';
  SOKSaveProject = 'Save project';
  SOKSaveCase = 'Save case';
  SOKSaveAttr = 'Save attribute';
  SOKSaveSymbols = 'Save symbols';
  SOKSaveDef = 'Save options';

  SFunctionStringSensitive = 'Case-sensitive';
  SFunctionStringInsensitive = 'Case-insensitive';
  SFunctionNumberLinear = 'Linear';
  SFunctionNumberStep = 'Step';
  SFunctionEquality = 'Exact';
  SFunctionStringMetaphone = 'Metaphone';
  SFunctionStringIntersectMetaphone = 'Intersect and metaphone';
  SFunctionStringIntersect = 'Intersect';

  SServiceDBStarting = 'Starting storage engine';
  SServicePHPStarting = 'Starting CBR engine';

  SErrSelectProject = 'Select a project';
  SErrSelectCase = 'Select a case';

  SErrCantSave = 'Unable to save';
  SErrCantDelete = 'Unable to delete';
  SErrEmptyResponse = 'Empty response';

  SErrMustBeGreaterThanPrevious = 'Must be greater than previous';

  SErrAlreadyRunning = 'Already running';
  SErrServiceNotRunning = 'Service is not running';
  SErrSettingsPathPHPMissing = 'Path to "php" executable must be set';
  SErrSettingsPathPHPWrong = 'Path to "php" is wrong';
  SErrSettingsPathMongoMissing = 'Path to "mongo" executable must be set';
  SErrSettingsPathMongoWrong = 'Path to "mongo" is wrong';
  SErrCreatingDir = 'Failed creating dir "%s"';
  SErrOpening = 'Failed opening "%s"';

  SConfirmDeleteFile = 'Delete file "%s"?';
  SConfirmRemove = 'Confirm remove?';
  SConfirmExit = 'Confirm exit?';

  SCaseTypeRelation = 'Table, view or collection';
  SCaseTypeQuery    = 'Query statement';
  SCaseTypeFile     = 'CSV file';

  SGlobalSimEuclidean = 'Euclidean';
  SGlobalSimNearest   = 'Nearest neighbour';

  STreeColProjects    = 'Projects';
  STreeColEdit        = 'Edit';
  STreeColDelete      = 'Delete';
  STreeColDescription = 'Description';
  STreeColWeigth      = 'Weigth';

  SGridColSource = 'Source';
  SGridColScore = 'Score';
  SGridStoredBase = 'Stored';
  SGridCaseBase = 'New';


implementation



end.


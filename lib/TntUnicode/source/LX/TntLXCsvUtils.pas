
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXCsvUtils;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Controls, Windows, Dialogs, Grids, DB, DBGrids,
  TntClasses, TntComCtrls, TntDialogs, TntGrids,
  TntLXVarArrayDataSet;

type
  TCSVRowDoneEvent = procedure(Row: Integer) of object;
  TCSVCellDoneEvent = procedure(Row, Col: Integer; Cell: WideString) of object;

function GetNextWideChar(Stream: TStream; CharSet: TTntStreamCharSet): WideChar;

function Clipboard_ContainsTabDelimitedText(_DelimiterChar: WideChar = WideChar(VK_TAB); _FirstRowFieldNames: Boolean = False): Boolean;

type
  TTntCSVTable = class(TTntVarArrayDataSet)
  private
    FFileName: WideString;
    FFieldNames: TTntStringList;
    FLastRow: Integer;
    FDelimiterChar: WideChar;
    FFirstRowFieldNames: Boolean;
    FAutoTrim: Boolean;
    FMinFieldSize: Integer;
    procedure CellDone(Row, Col: Integer; Cell: WideString);
    procedure SetFileName(const Value: WideString);
    procedure ParseCSV;
  protected
    procedure InitFieldDefs; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(CSVFileName: WideString);
    procedure LoadFromStrings(sList: TTntStrings);
    procedure LoadFromClipboard(_DelimiterChar: WideChar = WideChar(VK_TAB); _FirstRowFieldNames: Boolean = False);
    procedure LoadFromStringGrid(SGrid: TTntStringGrid);
    property DelimiterChar: WideChar read FDelimiterChar write FDelimiterChar default #0;
    property FirstRowFieldNames: Boolean read FFirstRowFieldNames write FFirstRowFieldNames default True;
  published
    property AutoTrim: Boolean read FAutoTrim write FAutoTrim default True;
    property MinFieldSize: Integer read FMinFieldSize write FMinFieldSize default 1;
    property FileName: WideString read FFileName write SetFileName;
  end;

// csv file dialog
procedure PrepDialogForCSV(Dialog: TTntOpenDialog);
function PromptToOpenCsvFile(var CsvFile: WideString): Boolean;
function PromptToSaveCsvFile(var CsvFile: WideString; _Options: TOpenOptions = [ofHideReadOnly, ofEnableSizing]): Boolean;
procedure PromptToExportDataSet(DataSet: TDataSet; IncludeFieldNames: Boolean = True);

// csv export
procedure Csv_AddValue(var line: WideString; const value: WideString);
function GetCsvLineForFieldDisplayNames(DataSet: TDataSet): WideString;
function GetCsvLineForFieldNames(DataSet: TDataSet): WideString;
function GetCsvLineForRecord(DataSet: TDataSet): WideString;
procedure ExportDataSetToCsvStrings(DataSet: TDataSet; CsvStrings: TTntStrings; IncludeFieldNames: Boolean = True);

procedure ExportDataSetToCsvFile(DataSet: TDataSet; CsvFileName: WideString;
  IncludeFieldNames: Boolean; UnicodeFile: Boolean = False);
procedure ExportStringGridToCSVStrings(SGrid: TTntStringGrid; CSVStrings: TTntStrings);
procedure ExportStringGridToCSVFile(SGrid: TTntStringGrid; CSVFileName: WideString);
procedure ExportListViewToCSVStrings(ListView: TTntCustomListView; CSVStrings: TTntStrings; SelectedOnly: Boolean);
procedure ExportListViewToCSVFile(ListView: TTntCustomListView; CSVFileName: WideString; SelectedOnly: Boolean);

function FindGridInParentChain(Control: TWinControl): TCustomDBGrid{TNT-ALLOW TCustomDBGrid};
procedure PasteClipboardToGrid(_Grid: TCustomDBGrid{TNT-ALLOW TCustomDBGrid});

implementation

uses
  SysUtils, Math, Variants, Clipbrd, ShellApi, ADODB,
  TntDB, TntClipbrd, TntSysUtils,
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ELSE} TntWideStrUtils, {$ENDIF}
  TntLXUtils, TntLXClasses, TntLXRichEdits, TntLXDialogs;

function GetNextWideChar(Stream: TStream; CharSet: TTntStreamCharSet): WideChar;

    function WideCharFromMBCS(leadbyte, trailbyte: AnsiChar): WideChar;
    var
      s: AnsiString;
      w: WideString;
    begin
      s := leadbyte + trailbyte;
      w := s;
      Result := w[1];
    end;

var
  c_Ansi: AnsiChar;
  leadbyte_Ansi: AnsiChar;
  trailbyte_Ansi: AnsiChar;
begin
  if Stream.Position = Stream.Size then
    Result := #0 { instead of failure, just return a zero }
  else begin
    case CharSet of
      csUnicode:
        begin
          Stream.Read(Result, SizeOf(Result));
        end;
      csUnicodeSwapped:
        begin
          Stream.Read(Result, SizeOf(Result));
          Result := WideChar(MakeWord(HiByte(Word(Result)), LoByte(Word(Result))));
        end;
      csAnsi:
        begin
          Stream.Read(c_Ansi, SizeOf(c_Ansi));
          if not (c_Ansi in LeadBytes) then
            // SBCS
            Result := WideCharFromMBCS(c_Ansi, #0)
          else begin
            // MBCS
            leadbyte_Ansi := c_Ansi;
            if Stream.Read(trailbyte_Ansi, SizeOf(trailbyte_Ansi)) > 0 then
              Result := WideCharFromMBCS(leadbyte_Ansi, trailbyte_Ansi)
            else
              Result := WideCharFromMBCS(leadbyte_Ansi, #0); { instead of failure, just return the lead byte }
          end;
        end;
      csUtf8:
        raise ETntInternalError.Create('Internal Error: UTF8 encoding not supported.')
      else
        raise ETntInternalError.Create('Internal Error: Unexptected character set.');
    end;
  end;
end;

function ReadFirstLine(InputStream: TStream): WideString;
var
  Stream: TStream;
  CharSet: TTntStreamCharSet;
  ResultBuilder: TBufferedWideString;
  wc: WideChar;
begin
  ResultBuilder := TBufferedWideString.Create;
  try
    Stream := TBufferedStreamReader.Create(InputStream);
    try
      CharSet := AutoDetectCharacterSet(Stream);
      while Stream.Position < Stream.Size do begin
        wc := GetNextWideChar(Stream, CharSet);
        if  (wc = CR) or (wc = LF) then
          break
        else
          ResultBuilder.AddChar(wc);
      end;
    finally
      Stream.Free;
    end;
    Result := ResultBuilder.Value;
  finally
    ResultBuilder.Free;
  end;
end;

{ TTntCSVParser }
type
  TTntCSVParser = class(TObject)
  private
    FOnRowDone: TCSVRowDoneEvent;
    FOnCellDone: TCSVCellDoneEvent;
  protected
    AutoTrim: Boolean;
    procedure DoRowDone(Row: Integer);
    procedure DoCellDone(Row, Col: Integer; Cell: WideString);
  public
    procedure ParseCSVStream(InputStream: TStream; SeparatorChar: WideChar);
    property OnRowDone: TCSVRowDoneEvent read FOnRowDone write FOnRowDone;
    property OnCellDone: TCSVCellDoneEvent read FOnCellDone write FOnCellDone;
  end;

procedure TTntCSVParser.DoCellDone(Row, Col: Integer; Cell: WideString);
begin
  Cell := TntAdjustLineBreaks(Cell);
  Cell := Tnt_WideStringReplace(Cell, WideString(WideChar($201C)) + WideChar($201C), WideChar($201C), [rfReplaceAll]);
  Cell := Tnt_WideStringReplace(Cell, WideString(WideChar($201D)) + WideChar($201D), WideChar($201D), [rfReplaceAll]);
  if AutoTrim then
    Cell := Trim(Cell);
  if Assigned(OnCellDone) then
    OnCellDone(Row, Col, Cell);
end;

procedure TTntCSVParser.DoRowDone(Row: Integer);
begin
  if Assigned(OnRowDone) then
    OnRowDone(Row);
end;

procedure TTntCSVParser.ParseCSVStream(InputStream: TStream; SeparatorChar: WideChar);
var
  Stream: TStream;
  CharSet: TTntStreamCharSet;
  row, col: integer;
  cell: TBufferedWideString;
  ReadingValue: Boolean;
  ReadingQuotedValue: Boolean;

  function PeekChar: WideChar;
  var
    SavePosition: Integer;
  begin
    SavePosition := Stream.Position;
    Result := GetNextWideChar(Stream, CharSet);
    Stream.Position := SavePosition;
  end;

  function NextChar: WideChar;
  begin
    Result := GetNextWideChar(Stream, CharSet);
  end;

  procedure FinishCell;
  begin
    DoCellDone(row, col, cell.Value);
    inc(col);
    cell.Clear;
    ReadingQuotedValue := False;
    ReadingValue := False;
  end;

var
  wc: WideChar;
begin
  Stream := TBufferedStreamReader.Create(InputStream);
  try
    CharSet := AutoDetectCharacterSet(Stream);
    row := 0;
    col := 0;
    ReadingQuotedValue := False;
    ReadingValue := False;
    cell := TBufferedWideString.Create;
    try
      while Stream.Position < Stream.Size do begin
        wc := NextChar;

        if  ((wc = CR) or (wc = LF))
        and (not ReadingQuotedValue) then begin

          {-- end of row --}
          if (wc = CR) and (PeekChar = LF) then begin
            NextChar;
          end;
          if ReadingValue then begin
            FinishCell;
          end;
          DoRowDone(row);
          inc(row);
          col := 0;

        end else if (not ReadingValue) then begin

          {-- not reading value --}
          if (wc = '"') then begin
            { start a quoted value }
            ReadingValue := True;
            ReadingQuotedValue := True;
          end else if wc = SeparatorChar then begin
            { finish an empty value }
            FinishCell;
          end else begin
            { start a non-quoted value }
            cell.AddChar(wc);
            ReadingValue := True;
          end;

        end else if (not ReadingQuotedValue) then begin

          {-- reading non-quoted value --}
          if wc <> SeparatorChar then
            cell.AddChar(wc)
          else
            FinishCell;

        end else begin

          {-- reading quoted value --}
          if (wc <> '"') then
            cell.AddChar(wc)
          else if (PeekChar = '"') then begin
            {reduce double-quote}
            NextChar;
            cell.AddChar('"');
          end else begin
            {end of quote}
            ReadingQuotedValue := False;
          end;

        end;
      end;
    finally
      cell.Free;
    end;
  finally
    Stream.Free;
  end;
end;

{ TTntCSVTable }

constructor TTntCSVTable.Create(AOwner: TComponent);
begin
  inherited;
  FFieldNames := TTntStringList.Create;
  FFirstRowFieldNames := True;
  FDelimiterChar := #0;
  FAutoTrim := True;
  FMinFieldSize := 1;
end;

destructor TTntCSVTable.Destroy;
begin
  inherited;
  FreeAndNil(FFieldNames);
end;

procedure TTntCSVTable.SetFileName(const Value: WideString);
begin
  CheckInactive;
  FFileName := Value;
end;

procedure TTntCSVTable.InitFieldDefs;
begin
  if (not IsCursorOpen) then begin
    ParseCSV;
  end;
  InternalInitFieldDefs;
end;

procedure TTntCSVTable.InternalInitFieldDefs;
var
  i: integer;
begin
  FieldDefs.Clear;
  for i := 0 to FFieldNames.Count - 1 do
    TFieldDef.Create(FieldDefs, FFieldNames[i], ftWideString,
      Integer(FFieldNames.Objects[i]), False, i + 1);
end;

procedure TTntCSVTable.CellDone(Row, Col: Integer; Cell: WideString);

  procedure AddFieldName(FieldName: WideString);
  var
    Test: Integer;
  begin
    Test := Col + 1;
    while (FieldName = '')
    or    (FFieldNames.IndexOf(FieldName) <> -1) do
    begin
      FieldName := 'Field' + IntToStr(Test);
      Inc(Test);
    end;
    FFieldNames.AddObject(FieldName, TObject(FMinFieldSize))
  end;

  procedure CheckFieldSize;
  var
    CellSize: Integer;
  begin
    CellSize := Length(Cell);
    if Integer(FFieldNames.Objects[Col]) < CellSize then
      FFieldNames.Objects[Col] := TObject(CellSize);
  end;

  procedure AddAdditionalField;
  var
    i: integer;
  begin
    AddFieldName('');
    for i := 0 to Row - 2 do begin
      SetLength(FRecords[i]^, Col + 1);
      FRecords[i]^[Col] := Unassigned;
    end;
  end;

const
  ROW_ALLOC_COUNT = 64;
var
  i: integer;
begin
  if (not FirstRowFieldNames) then
    Inc(Row);
  if (Row = 0) then
    AddFieldName(Cell)
  else begin
    if Col = FFieldNames.Count then begin
      AddAdditionalField;
    end;
    CheckFieldSize;
    if FRecords.Count < Row then begin
      FRecords.Count := Row + ROW_ALLOC_COUNT - 1;
    end;
    if FRecords[Row - 1] = nil then
      New(PVariantArray(FRecords.List[Row - 1]));
    if (Length(FRecords[Row - 1]^) < FFieldNames.Count) then
      SetLength(FRecords[Row - 1]^, FFieldNames.Count);
    FRecords[Row - 1]^[Col] := Cell;
    // check for any skipped rows (completely blank)
    for i := FLastRow to Row - 2 do begin
      if FRecords[i] = nil then begin
        New(PVariantArray(FRecords.List[i]));
        SetLength(FRecords[i]^, FFieldNames.Count);
      end;
    end;
    FLastRow := Row;
  end;
end;

procedure TTntCSVTable.ParseCSV;
var
  FS: TTntFileStream;
  CSVParser: TTntCSVParser;
  FirstLine: WideString;
  AutoDelimiter: WideChar;
  AutoDelimiterCount: Integer;
  i, j: integer;
  EntireRowIsBlank: Boolean;
begin
  Assert(WideFileExists(FFileName), WideFormat('%s doesn''t exists.', [FFileName]));
  FFieldNames.Clear;
  FLastRow := 0;

  CSVParser := TTntCSVParser.Create;
  try
    CSVParser.AutoTrim := AutoTrim;
    CSVParser.OnCellDone := CellDone;
    FS := TTntFileStream.Create(FFileName, fmOpenRead);
    try
      if DelimiterChar <> #0 then begin
        // delimiter explicitly set
        CSVParser.ParseCSVStream(FS, DelimiterChar)
      end else begin
        // auto pick delimiter
        FirstLine := ReadFirstLine(FS);
        FS.Position := 0;
        // assume comma
        AutoDelimiter := ',';
        AutoDelimiterCount := CountWord(',', FirstLine);
        // test for semicolon
        if CountWord(';', FirstLine) > AutoDelimiterCount then begin
          AutoDelimiter := ';';
          AutoDelimiterCount := CountWord(';', FirstLine);
        end;
        // finally test for locale based ListSeparator...
        if (ListSeparator <> ',') and (ListSeparator <> ';') and (ListSeparator <> #0)
        and (CountWord(ListSeparator, FirstLine) > AutoDelimiterCount) then
          AutoDelimiter := WideChar(ListSeparator);
        // and finally do the parse...
        CSVParser.ParseCSVStream(FS, AutoDelimiter);
      end;
    finally
      FS.Free;
    end;
  finally
    CSVParser.Free;
  end;

  // do final preparation to FRecords
  FRecords.Count := FLastRow;
  for i := FRecords.Count - 1 downto 0 do begin
    Assert(Length(FRecords[i]^) = FFieldNames.Count);
    EntireRowIsBlank := True;
    for j := 0 to FFieldNames.Count - 1 do begin
      // assign empty strings to Unassigned
      if VarIsEmpty(FRecords[i]^[j]) then
        FRecords[i]^[j] := '';
      if EntireRowIsBlank and (Trim(WideString(FRecords[i]^[j])) <> '') then
        EntireRowIsBlank := False;
    end;
    // trim any completey blank rows (if AutoTrim is turned on)
    if AutoTrim and EntireRowIsBlank then begin
      FreeRecord(FRecords[i]);
    end;
  end;
end;

procedure TTntCSVTable.InternalOpen;
begin
  ParseCSV;
  inherited;
end;

procedure TTntCSVTable.LoadFromFile(CSVFileName: WideString);
begin
  Close;
  FileName := CSVFileName;
  Open;
end;

procedure TTntCSVTable.LoadFromStrings(sList: TTntStrings);
var
  TempFile: WideString;
begin
  TempFile := GetGarbageCollectedTempFile;
  sList.SaveToFile(TempFile);
  LoadFromFile(TempFile);
  ForceDeleteFile(TempFile);
end;

function Clipboard_ContainsTabDelimitedText(_DelimiterChar: WideChar = WideChar(VK_TAB); _FirstRowFieldNames: Boolean = False): Boolean;
var
  sList: TTntStringList;
  CSVTable: TTntCSVTable;
begin
  Result := False;
  if TntClipboard.HasFormat(CF_TEXT) or TntClipboard.HasFormat(CF_UNICODETEXT) then begin
    sList := TTntStringList.Create;
    try
      sList.Text := TntClipboard.AsWideText;
      if (sList.Count > 1) then
        Result := (sList[0] <> '')
      else if (sList.Count = 1)
      and (sList[0] <> '') then begin
        CSVTable := TTntCSVTable.Create(nil);
        try
          CSVTable.DelimiterChar := _DelimiterChar;
          CSVTable.FirstRowFieldNames := _FirstRowFieldNames;
          CSVTable.LoadFromStrings(sList);
          Result := CSVTable.FieldCount > 1;
        finally
          CSVTable.Free;
        end;
      end;
    finally
      sList.Free;
    end;
  end;
end;

procedure TTntCSVTable.LoadFromClipboard(_DelimiterChar: WideChar = WideChar(VK_TAB); _FirstRowFieldNames: Boolean = False);
var
  sList: TTntStringList;
begin
  if (not Clipboard_ContainsTabDelimitedText) then
    raise ETntGeneralError.Create('Clipboard does not contain rows or columns.');
  sList := TTntStringList.Create;
  try
    sList.Text := TntClipboard.AsWideText;
    DelimiterChar := _DelimiterChar;
    FirstRowFieldNames := _FirstRowFieldNames;
    LoadFromStrings(sList);
  finally
    sList.Free;
  end;
end;

procedure TTntCSVTable.LoadFromStringGrid(SGrid: TTntStringGrid);
var
  CSVStrings: TTntStringList;
  col: Integer;
begin
  CSVStrings := TTntStringList.Create;
  try
    ExportStringGridToCSVStrings(SGrid, CSVStrings);
    LoadFromStrings(CSVStrings);
  finally
    CSVStrings.Free;
  end;
  for col := 0 to SGrid.ColCount - 1 do
    Fields[col].Visible := SGrid.ColWidths[col] > 0
end;

//======================================== CSV FILE DIALOG
procedure PrepDialogForCSV(Dialog: TTntOpenDialog);
begin
  Dialog.DefaultExt := 'csv';
  Dialog.Filter := 'Comma Delimited Text (*.csv)|*.csv'
                + '|Comma Delimited Text (*.txt)|*.txt'; { do not localize }
end;

function PromptToOpenCsvFile(var CsvFile: WideString): Boolean;
var
  Dlg: TTntOpenDialogLX;
begin
  Dlg := TTntOpenDialogLX.Create(nil);
  with Dlg do
  try
    PrepDialogForCSV(Dlg);
    Result := Execute;
    if Result then
      CsvFile := FileName;
  finally
    Free;
  end;
end;

function PromptToSaveCsvFile(var CsvFile: WideString; _Options: TOpenOptions = [ofHideReadOnly, ofEnableSizing]): Boolean;
var
  Dlg: TTntSaveDialogLX;
begin
  Dlg := TTntSaveDialogLX.Create(nil);
  with Dlg do
  try
    PrepDialogForCSV(Dlg);
    Dlg.Options := _Options;
    Result := Execute;
    if Result then
      CsvFile := FileName;
  finally
    Free;
  end;
end;

procedure PromptToExportDataSet(DataSet: TDataSet; IncludeFieldNames: Boolean = True);
var
  FileName: WideString;
begin
  if PromptToSaveCsvFile(FileName, [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]) then
    ExportDataSetToCsvFile(DataSet, FileName, IncludeFieldNames);
end;

//======================================== CSV EXPORT
function CSV_Cell(const Value: WideString): WideString;
begin
  Result := Value;
  if Pos(CRLF, Result) <> 0 then
    Result := Tnt_WideStringReplace(Result, CRLF, LF, [rfReplaceAll]);
  if Pos(CR, Result) <> 0 then
    Result := Tnt_WideStringReplace(Result, CR,  LF, [rfReplaceAll]);
  if Pos(WideChar($201C), Result) <> 0 then
    Result := Tnt_WideStringReplace(Result, WideChar($201C), WideString(WideChar($201C)) + WideChar($201C), [rfReplaceAll]);
  if Pos(WideChar($201D), Result) <> 0 then
    Result := Tnt_WideStringReplace(Result, WideChar($201D), WideString(WideChar($201D)) + WideChar($201D), [rfReplaceAll]);
  Result := WideQuotedStr(Result, '"');
end;

procedure Csv_AddValue(var line: WideString; const value: WideString);
begin
  ListAddItem(line, CSV_Cell(value), ',');
end;

function GetCsvLineForFieldNames(DataSet: TDataSet): WideString;
var
  FieldIdx: Integer;
begin
  Result := '';
  for FieldIdx := 0 to DataSet.Fieldcount - 1 do begin
    if DataSet.Fields[FieldIdx].Visible then
      Csv_AddValue(Result, DataSet.Fields[FieldIdx].FieldName);
  end;
end;

function GetCsvLineForFieldDisplayNames(DataSet: TDataSet): WideString;
var
  FieldIdx: Integer;
begin
  Result := '';
  for FieldIdx := 0 to DataSet.Fieldcount - 1 do begin
    if DataSet.Fields[FieldIdx].Visible then
      Csv_AddValue(Result, DataSet.Fields[FieldIdx].DisplayName);
  end;
end;

function GetCsvLineForRecord(DataSet: TDataSet): WideString;
var
  FieldIdx: Integer;
begin
  Result := '';
  for FieldIdx := 0 to DataSet.Fieldcount - 1 do begin
    if DataSet.Fields[FieldIdx].Visible then
      Csv_AddValue(Result, RTFToPlainText(GetWideDisplayText(DataSet.Fields[FieldIdx])));
  end;
end;

procedure ExportDataSetToCsvStrings(DataSet: TDataSet; CsvStrings: TTntStrings; IncludeFieldNames: Boolean = True);
begin
  ForceAssigned(DataSet, 'DataSet');
  ForceAssigned(CsvStrings, 'CsvStrings');
  CsvStrings.Clear;

  // field names
  if IncludeFieldNames then
    CsvStrings.Add(GetCsvLineForFieldDisplayNames(DataSet));

  // loop through each record
  DataSet.DisableControls;
  try
    if not DataSet.Bof then
      DataSet.First;
    while not DataSet.EOF do begin
      // field values
      CsvStrings.Add(GetCsvLineForRecord(DataSet));
      DataSet.Next;
    end;
  finally
    DataSet.EnableControls;
  end;
end;

procedure ExportDataSetToCsvFile(DataSet: TDataSet; CsvFileName: WideString;
  IncludeFieldNames: Boolean; UnicodeFile: Boolean = False);
var
  CSVStrings: TTntStringList;
begin
  CSVStrings := TTntStringList.Create;
  try
    ExportDataSetToCsvStrings(DataSet, CSVStrings, IncludeFieldNames);
    if UnicodeFile then
      CSVStrings.SaveToFile(CsvFileName) { Excel doesn't handle Unicode CSV files. }
    else
      CSVStrings.AnsiStrings.SaveToFile(CsvFileName); { Excel doesn't handle Unicode CSV files. }
  finally
    CSVStrings.Free;
  end;
end;

procedure ExportStringGridToCSVStrings(SGrid: TTntStringGrid; CSVStrings: TTntStrings);
var
  row, col: Integer;
  s: WideString;
begin
  // build out file
  for row := 0 to SGrid.RowCount - 1 do begin
    s := '';
    for col := 0 to SGrid.ColCount - 1 do begin
      Csv_AddValue(s, SGrid.Cells[col, row]);
    end;
    CsvStrings.Add(s);
  end;
end;

procedure ExportStringGridToCSVFile(SGrid: TTntStringGrid; CSVFileName: WideString);
var
  CSVStrings: TTntStringList;
begin
  CSVStrings := TTntStringList.Create;
  try
    ExportStringGridToCSVStrings(SGrid, CSVStrings);
    CSVStrings.AnsiStrings.SaveToFile(CsvFileName); { Excel doesn't handle Unicode CSV files. }
  finally
    CSVStrings.Free;
  end;
end;

type
  THackCustomListView = class(TTntCustomListView);

procedure ExportListViewToCSVStrings(ListView: TTntCustomListView; CSVStrings: TTntStrings; SelectedOnly: Boolean);
var
  row, col: Integer;
  s: WideString;
  HackListView: THackCustomListView;
begin
  HackListView := THackCustomListView(ListView);
  // field names
  s := '';
  for col := 0 to HackListView.Columns.Count - 1 do
    Csv_AddValue(s, HackListView.Columns[col].Caption);
  CSVStrings.Add(s);

  // data
  for row := 0 to HackListView.Items.Count - 1 do begin
    if (not SelectedOnly) or HackListView.Items[row].Selected then begin
      s := '';
      Csv_AddValue(s, HackListView.Items[row].Caption);
      for col := 0 to HackListView.Items[row].SubItems.Count - 1 do begin
        Csv_AddValue(s, HackListView.Items[row].SubItems[col]);
      end;
      CsvStrings.Add(s);
    end;
  end;
end;

procedure ExportListViewToCSVFile(ListView: TTntCustomListView; CSVFileName: WideString; SelectedOnly: Boolean);
var
  CSVStrings: TTntStringList;
begin
  CSVStrings := TTntStringList.Create;
  try
    ExportListViewToCSVStrings(ListView, CSVStrings, SelectedOnly);
    CSVStrings.AnsiStrings.SaveToFile(CsvFileName); { Excel doesn't handle Unicode CSV files. }
  finally
    CSVStrings.Free;
  end;
end;

function FindGridInParentChain(Control: TWinControl): TCustomDBGrid{TNT-ALLOW TCustomDBGrid};
begin
  if Control = nil then
    Result := nil
  else if Control is TCustomDBGrid{TNT-ALLOW TCustomDBGrid} then
    Result := TCustomDBGrid{TNT-ALLOW TCustomDBGrid}(Control)
  else
    Result := FindGridInParentChain(Control.Parent);
end;

type TAccessCustomDBGrid = class(TCustomDBGrid{TNT-ALLOW TCustomDBGrid});

procedure PasteClipboardToGrid(_Grid: TCustomDBGrid{TNT-ALLOW TCustomDBGrid});
var
  Grid: TAccessCustomDBGrid;
  Data: TDataSet;
  ADOData: TCustomADODataSet;
  CSVTable: TTntCSVTable;
  i: Integer;
  Field: TField;
begin
  Grid := TAccessCustomDBGrid(_Grid);
  ForceAssigned(Grid, 'Grid');
  ForceAssigned(Grid.DataSource, 'Grid.DataSource');
  ForceAssigned(Grid.DataSource.DataSet, 'Grid.DataSource.DataSet');
  Data := Grid.DataSource.DataSet;

  if (Data is TCustomAdoDataSet)
  and (TCustomADODataSet(Data).Connection <> nil) then
    ADOData := TCustomADODataSet(Data)
  else
    ADOData := nil;

  CSVTable := TTntCSVTable.Create(nil);
  try
    CSVTable.LoadFromClipboard;
    if ADOData <> nil then
      ADOData.Connection.BeginTrans;
    try
      while not CSVTable.EOF do begin
        Data.Append;
        for i := 0 to Min(Grid.Columns.Count - 1, CSVTable.FieldCount - 1) do begin
          Field := Grid.Columns[i].Field;
          if (Field <> nil) then begin
            if Field.Lookup then begin
              Data.FieldByName(Field.KeyFields).Value := Field.LookupDataSet.Lookup(Field.LookupResultField, CSVTable.Fields[i].Value, Field.LookupKeyFields)
            end else if (Field.CanModify) then begin
              SetAsWideString(Field, GetAsWideString(CSVTable.Fields[i]));
            end;
          end;
        end;
        Data.Post;
        CSVTable.Next;
      end;
      if ADOData <> nil then
        ADOData.Connection.CommitTrans;
    except
      if ADOData <> nil then
        ADOData.Connection.RollbackTrans;
      raise;
    end;
  finally
    CSVTable.Free;
  end;
end;

end.

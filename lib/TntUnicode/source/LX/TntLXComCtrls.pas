
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXComCtrls;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, ComCtrls, TntComCtrls, Controls;

type
  TTntMonthCalendarLX = class(TTntMonthCalendar)
  private
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
  published
    property Date: TDate read GetDate write SetDate;
  end;

implementation

uses
  SysUtils, CommCtrl, TntLXUtils, TntSysUtils;

// ---------------------- from monthcal.c from WINE project (0.9.24)
type
  WINE_MONTHCAL_INFO = packed record
    hwndSelf: HWND;
    x_bk: COLORREF;
    x_txt: COLORREF;
    x_titlebk: COLORREF;
    x_titletxt: COLORREF;
    x_monthbk: COLORREF;
    x_trailingtxt: COLORREF;
    x_hFont: HFONT;
    x_hBoldFont: HFONT;
    x_textHeight: LongInt;
    x_textWidth: LongInt;
    x_height_increment: LongInt;
    x_width_increment: LongInt;
    x_firstDayplace: LongInt;
    x_delta: LongInt;
    x_visible: LongInt;
    x_firstDay: LongInt;
    x_monthRange: LongInt;
    x_monthdayState: ^MONTHDAYSTATE;
    todaysDate: SYSTEMTIME;
    currentMonth: DWORD;
    currentYear: DWORD;
  end;
  PWINE_MONTHCAL_INFO = ^WINE_MONTHCAL_INFO;

function MONTHCAL_GetInfoPtr(hwnd: HWND): PWINE_MONTHCAL_INFO;
var
  Today: TSystemTime;
begin
  Assert(Is_WINE);
  Result := PWINE_MONTHCAL_INFO(GetWindowLongW(hwnd, 0));
  if Result <> nil then begin
    DateTimeToSystemTime(Date, Today);
    if (Result.hwndSelf <> hwnd)
    or (Result.todaysDate.wYear <> Today.wYear)
    or (Result.todaysDate.wMonth <> Today.wMonth)
    or (Result.todaysDate.wDay <> Today.wDay) then
      Result := nil; // failed validation test!
  end;
end;

{ TTntMonthCalendarLX }

function TTntMonthCalendarLX.GetDate: TDate;
var
  Year, Month, Day: Word;
  InfoPtr: PWINE_MONTHCAL_INFO;
begin
  Result := inherited Date;
  if Is_WINE and (HandleAllocated) then begin
    // try to get the current month/year and use it (bug in WINE)
    DecodeDate(Result, Year, Month, Day);
    InfoPtr := MONTHCAL_GetInfoPtr(Handle);
    if InfoPtr <> nil then begin
      Year := InfoPtr.currentYear;
      Month := InfoPtr.currentMonth;
      Result := SafeEncodeDate(Year, Month, Day);
    end;
  end;
end;

procedure TTntMonthCalendarLX.SetDate(const Value: TDate);
begin
  inherited Date := Value;
end;

end.

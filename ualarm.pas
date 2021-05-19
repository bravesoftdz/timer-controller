//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// Timer configuration frame.
// 
// Copyright (C) 2020 SriKIT contributors.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Last updated: Dilshan Jayakody [24th Nov 2020]
//
// Update log:
// [24/11/2020] - Initial version - Dilshan Jayakody.
//-----------------------------------------------------------------------------

unit ualarm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, ucommon,
  Dialogs, Menus, Clipbrd;

type
  TAlarmComponent = (alNone, alStartYear, alEndYear, alStartMonth, alEndMonth, alStartDay, alEndDay,
                   alStartHour, alEndHour, alStartMinute, alEndMinute, alStartSecond, alEndSecond);

  { TfmTimer }

  TfmTimer = class(TFrame)
    chkSelect: TCheckBox;
    cmbStartMonth: TComboBox;
    cmbEndMonth: TComboBox;
    imglMenu: TImageList;
    lblEnd: TLabel;
    lblChannel: TLabel;
    lblStartDate1: TLabel;
    lblStartHour1: TLabel;
    lblStartMinute1: TLabel;
    lblStartMonth1: TLabel;
    lblStartSec1: TLabel;
    lblStartYear1: TLabel;
    mnuDuplicate: TMenuItem;
    mnuPasteValues: TMenuItem;
    mnuMuteDate: TMenuItem;
    mnuDelete: TMenuItem;
    mnuSep2: TMenuItem;
    mnuSep1: TMenuItem;
    mnuPaste: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuItem: TPopupMenu;
    txtEndDate: TEdit;
    txtChannel: TEdit;
    txtStartHour: TEdit;
    txtEndHour: TEdit;
    txtStartMinute: TEdit;
    txtEndMinute: TEdit;
    txtStartSeconds: TEdit;
    lblStartHour: TLabel;
    lblStartMinute: TLabel;
    lblStartSec: TLabel;
    lblStartMonth: TLabel;
    lblStart: TLabel;
    lblStartDate: TLabel;
    txtEndSeconds: TEdit;
    txtStartYear: TEdit;
    gpAlarm: TGroupBox;
    imgWarning: TImage;
    lblStartYear: TLabel;
    txtStartDate: TEdit;
    txtEndYear: TEdit;
    updEndDate: TUpDown;
    updEndHour: TUpDown;
    updEndMinute: TUpDown;
    updEndSeconds: TUpDown;
    updStartYear: TUpDown;
    updStartDate: TUpDown;
    updStartHour: TUpDown;
    updStartMinute: TUpDown;
    updStartSeconds: TUpDown;
    updEndYear: TUpDown;
    updChannel: TUpDown;
    procedure cmbStartMonthChange(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuDuplicateClick(Sender: TObject);
    procedure mnuItemPopup(Sender: TObject);
    procedure mnuMuteDateClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuPasteValuesClick(Sender: TObject);
    procedure updChannelChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure updStartDateChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure updStartHourChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure updStartMinuteChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection
      );
    procedure updStartSecondsChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection
      );
    procedure updStartYearChanging(Sender: TObject; var AllowChange: Boolean);
    procedure updStartYearChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
  private
    channelReset: boolean;
    mainWindow: TForm;
    FOnValueChanged: TNotifyEvent;
  public
    Constructor Create(AOwner: TComponent; baseWindow: TForm); reintroduce;
    procedure SetAlarmInfo(info: TAlarmInfo; maxChannelCount: Byte);

    function GetAlarmInfo() : TAlarmInfo;
    function IsSelected() : Boolean;
    function IsChannelReset() : Boolean;
    function IsValidConfiguration(component: TAlarmComponent; val: Integer) : Boolean;

    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;

const
  // Minimum values for date / time fields.
  MIN_YEAR: Integer = 2020;
  MIN_DATE: Integer = 1;
  MIN_MONTH: Integer = 1;

  NOT_USED_LABEL: string = 'None';

implementation

uses
  uMain;

{$R *.lfm}

{ TfmTimer }

Constructor TfmTimer.Create(AOwner: TComponent; baseWindow: TForm);
begin
  inherited Create(AOwner);
  channelReset := false;
  mainWindow :=  baseWindow;
end;

{$hints off}
procedure TfmTimer.updStartYearChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  txtEdit: TEdit;
  component: TAlarmComponent;
begin
  // Select either start-year or end-year text box.
  if(TControl(Sender).Tag = 1) then
  begin
    txtEdit := txtEndYear;
    component := alEndYear;
  end
  else
  begin
    txtEdit := txtStartYear;
    component := alStartYear;
  end;

  // Apply new value into selected text box.
  if(NewValue < MIN_YEAR) then
  begin
    // Remove year component from the UI.
    txtEdit.Text := NOT_USED_LABEL;
  end
  else
  begin
    txtEdit.Text := IntToStr(NewValue);
  end;

  // Perform validations for user inputs.
  imgWarning.Visible := not IsValidConfiguration(component, NewValue);
end;
{$hints on}

{$hints off}
procedure TfmTimer.updStartDateChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  txtEdit: TEdit;
  component: TAlarmComponent;
begin
  // Select either start-date or end-date text box.
  if(TControl(Sender).Tag = 1) then
  begin
    txtEdit := txtEndDate;
    component := alEndDay;
  end
  else
  begin
    txtEdit := txtStartDate;
    component := alStartDay;
  end;

  // Apply new value into selected text box.
  if(NewValue < MIN_DATE) then
  begin
    // Remove date component from the UI.
    txtEdit.Text := NOT_USED_LABEL;
  end
  else
  begin
    txtEdit.Text := IntToStr(NewValue);
  end;

  // Perform validations for user inputs.
  imgWarning.Visible := not IsValidConfiguration(component, NewValue);
end;
{$hints on}

{$hints off}
procedure TfmTimer.updChannelChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  txtChannel.Text := IntToStr(NewValue);
end;
{$hints on}

procedure TfmTimer.cmbStartMonthChange(Sender: TObject);
var
  component: TAlarmComponent;
  cmbPos : Integer;
begin
  if(TControl(Sender).Tag = 1) then
  begin
    component := alEndMonth;
    cmbPos := cmbEndMonth.ItemIndex;
  end
  else
  begin
    component := alStartMonth;
    cmbPos := cmbStartMonth.ItemIndex;
  end;

  imgWarning.Visible := not IsValidConfiguration(component, cmbPos);

  if(Assigned(FOnValueChanged))then
  begin
    FOnValueChanged(self);
  end;
end;

procedure TfmTimer.mnuCopyClick(Sender: TObject);
var
  tmpAlarmInfo : TAlarmInfo;
  clipbrdStream : TMemoryStream;
begin
  // Get current values from the UI and save it to memory stream.
  tmpAlarmInfo := GetAlarmInfo();

  clipbrdStream := TMemoryStream.Create;
  SaveAlarmInfoToStream(tmpAlarmInfo, clipbrdStream);
  clipbrdStream.Position := 0;

  // Save memory stream into clipboard buffer.
  Clipboard.AddFormat((TfrmMain(mainWindow)).ClipboardRegFormat, clipbrdStream);
  FreeAndNil(clipbrdStream);
end;

procedure TfmTimer.mnuCutClick(Sender: TObject);
begin
  mnuCopyClick(Sender);
  Application.ProcessMessages;
  mnuDeleteClick(Sender);
end;

procedure TfmTimer.mnuDeleteClick(Sender: TObject);
begin
  // Perform async delete to avoid component communication / render errors.
  Application.ProcessMessages;
  TfrmMain(mainWindow).DeleteTimerAsync(self.Name);
end;

procedure TfmTimer.mnuDuplicateClick(Sender: TObject);
begin
  TfrmMain(mainWindow).DuplicateTimer(GetAlarmInfo());
end;

procedure TfmTimer.mnuItemPopup(Sender: TObject);
begin
  // Enable paste menu if clipboard format is available.
  mnuPaste.Enabled := Clipboard.HasFormat((TfrmMain(mainWindow)).ClipboardRegFormat);
  mnuPasteValues.Enabled := mnuPaste.Enabled;
  Application.ProcessMessages;
end;

procedure TfmTimer.mnuMuteDateClick(Sender: TObject);
var
  tempChangeFlag: Boolean;
begin
  // Set all date components to NONE.
  tempChangeFlag := true;

  updStartYear.Position := updStartYear.Min;
  updStartYearChangingEx(updStartYear, tempChangeFlag, updStartYear.Min, TUpDownDirection.updNone);
  updEndYear.Position := updEndYear.Min;
  updStartYearChangingEx(updEndYear, tempChangeFlag, updEndYear.Min, TUpDownDirection.updNone);

  updStartDate.Position := updStartDate.Min;
  updStartDateChangingEx(updStartDate, tempChangeFlag, updStartDate.Min, TUpDownDirection.updNone);
  updEndDate.Position := updEndDate.Min;
  updStartDateChangingEx(updEndDate, tempChangeFlag, updEndDate.Min, TUpDownDirection.updNone);

  cmbStartMonth.ItemIndex := 0;
  cmbStartMonthChange(cmbStartMonth);
  cmbEndMonth.ItemIndex := 0;
  cmbStartMonthChange(cmbEndMonth);
end;

procedure TfmTimer.mnuPasteClick(Sender: TObject);
begin
  TfrmMain(mainWindow).PasteNewTimer();
  Application.ProcessMessages;
end;

procedure TfmTimer.mnuPasteValuesClick(Sender: TObject);
var
  clipboardBuffer: TMemoryStream;
  alarmInfo: TAlarmInfo;
  mainForm: TfrmMain;
begin
  mainForm := TfrmMain(mainWindow);

  if(Clipboard.HasFormat(mainForm.ClipboardRegFormat)) then
  begin
    try
      clipboardBuffer := TMemoryStream.Create;

      // Get alarm information from clipboard.
      Clipboard.GetFormat(mainForm.ClipboardRegFormat, clipboardBuffer);
      clipboardBuffer.Position := 0;
      alarmInfo := LoadAlarmInfoFromStream(clipboardBuffer);

      // Assign clipboard data into current timer-panel.
      SetAlarmInfo(alarmInfo, mainForm.MaxChannel);

      // Update main window's change flags.
      mainForm.SetChangeFlag(true);
    finally
      FreeAndNil(clipboardBuffer);
      self.Invalidate;
    end;
  end;
end;

{$hints off}
procedure TfmTimer.updStartHourChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  txtEdit: TEdit;
  component: TAlarmComponent;
begin
    // Select either start-hour or end-hour text box.
  if(TControl(Sender).Tag = 1) then
  begin
    txtEdit := txtEndHour;
    component := alEndHour;
  end
  else
  begin
    txtEdit := txtStartHour;
    component := alStartHour;
  end;

  // Apply new value into selected text box.
  txtEdit.Text := IntToStr(NewValue);

  // Perform validations for user inputs.
  imgWarning.Visible := not IsValidConfiguration(component, NewValue);
end;
{$hints on}

{$hints off}
procedure TfmTimer.updStartMinuteChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  txtEdit: TEdit;
  component: TAlarmComponent;
begin
  // Select either start-hour or end-hour text box.
  if(TControl(Sender).Tag = 1) then
  begin
    txtEdit := txtEndMinute;
    component := alEndMinute;
  end
  else
  begin
    txtEdit := txtStartMinute;
    component := alStartMinute;
  end;

  // Apply new value into selected text box.
  txtEdit.Text := IntToStr(NewValue);

  // Perform validations for user inputs.
  imgWarning.Visible := not IsValidConfiguration(component, NewValue);
end;
{$hints on}

{$hints off}
procedure TfmTimer.updStartSecondsChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  txtEdit: TEdit;
  component: TAlarmComponent;
begin
    // Select either start-seconds or end-seconds text box.
  if(TControl(Sender).Tag = 1) then
  begin
    txtEdit := txtEndSeconds;
    component := alEndSecond;
  end
  else
  begin
    txtEdit := txtStartSeconds;
    component := alStartSecond;
  end;

  // Apply new value into selected text box.
  txtEdit.Text := IntToStr(NewValue);

  // Perform validations for user inputs.
  imgWarning.Visible := not IsValidConfiguration(component, NewValue);
end;
{$hints on}

{$hints off}
procedure TfmTimer.updStartYearChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if(Assigned(FOnValueChanged))then
  begin
    FOnValueChanged(self);
  end;
end;
{$hints on}

procedure TfmTimer.SetAlarmInfo(info: TAlarmInfo; maxChannelCount: Byte);
var
  tempChange : Boolean;
begin
  // Assign start values into up-down counters.
  updStartYear.Position := info.StartTime.Year;
  cmbStartMonth.ItemIndex := info.StartTime.Month;
  updStartDate.Position := info.StartTime.Date;
  updStartHour.Position := info.StartTime.Hour;
  updStartMinute.Position := info.StartTime.Minute;
  updStartSeconds.Position := info.StartTime.Second;

  // Assign end values into up-down counters.
  updEndYear.Position := info.EndTime.Year;
  cmbEndMonth.ItemIndex := info.EndTime.Month;
  updEndDate.Position := info.EndTime.Date;
  updEndHour.Position := info.EndTime.Hour;
  updEndMinute.Position := info.EndTime.Minute;
  updEndSeconds.Position := info.EndTime.Second;

  tempChange := false;

  // Update text fields with up-down counter values.
  updStartYearChangingEx(updStartYear, tempChange, updStartYear.Position, TUpDownDirection.updNone);
  updStartDateChangingEx(updStartDate, tempChange, updStartDate.Position, TUpDownDirection.updNone);
  updStartHourChangingEx(updStartHour, tempChange, updStartHour.Position, TUpDownDirection.updNone);
  updStartMinuteChangingEx(updStartMinute, tempChange, updStartMinute.Position, TUpDownDirection.updNone);
  updStartSecondsChangingEx(updStartSeconds, tempChange, updStartSeconds.Position, TUpDownDirection.updNone);

  updStartYearChangingEx(updEndYear, tempChange, updEndYear.Position, TUpDownDirection.updNone);
  updStartDateChangingEx(updEndDate, tempChange, updEndDate.Position, TUpDownDirection.updNone);
  updStartHourChangingEx(updEndHour, tempChange, updEndHour.Position, TUpDownDirection.updNone);
  updStartMinuteChangingEx(updEndMinute, tempChange, updEndMinute.Position, TUpDownDirection.updNone);
  updStartSecondsChangingEx(updEndSeconds, tempChange, updEndSeconds.Position, TUpDownDirection.updNone);

  // Update channel information.
  updChannel.Max := maxChannelCount;

  if(info.Channel > updChannel.Max) then
  begin
    // Specified channel number is out-of range!
    updChannel.Position := updChannel.Max;
    channelReset := true;
  end
  else
  begin
    updChannel.Position := info.Channel;
  end;

  updChannelChangingEx(updChannel, tempChange, updChannel.Position, TUpDownDirection.updNone);
end;

function TfmTimer.GetAlarmInfo() : TAlarmInfo;
var
  tmpInfo: TAlarmInfo;
  tmpStartInfo, tmpEndInfo: TTimeInfo;
begin
  // Extract channel details.
  tmpInfo.Channel := updChannel.Position;

  // Extract start date / time values.
  tmpStartInfo.Year := updStartYear.Position;
  tmpStartInfo.Month := cmbStartMonth.ItemIndex;
  tmpStartInfo.Date := updStartDate.Position;
  tmpStartInfo.Hour := updStartHour.Position;
  tmpStartInfo.Minute := updStartMinute.Position;
  tmpStartInfo.Second := updStartSeconds.Position;
  tmpInfo.StartTime := tmpStartInfo;

  // Extract end date / time values.
  tmpEndInfo.Year := updEndYear.Position;
  tmpEndInfo.Month := cmbEndMonth.ItemIndex;
  tmpEndInfo.Date := updEndDate.Position;
  tmpEndInfo.Hour := updEndHour.Position;
  tmpEndInfo.Minute := updEndMinute.Position;
  tmpEndInfo.Second := updEndSeconds.Position;
  tmpInfo.EndTime := tmpEndInfo;

  result := tmpInfo;
end;

function TfmTimer.IsSelected() : Boolean;
begin
  result := chkSelect.Checked;
end;

function TfmTimer.IsChannelReset() : Boolean;
begin
  result := channelReset;
end;

function TfmTimer.IsValidConfiguration(component: TAlarmComponent; val: Integer) : Boolean;
var
  startYear, endYear, startMonth, endMonth, startDay, endDay : Integer;
  startHour, endHour, startMinute, endMinute, startSeconds, endSeconds : Integer;

  startDateComp, startTimeComp, endDateComp, endTimeComp : Int64;

  function extractValue(comp: TAlarmComponent; componentVal: Integer) : Integer;
  begin
    if (component = comp) then
    begin
      result := val;
    end
    else
    begin
      result := componentVal;
    end;
  end;

begin
  result := false;

  // Extract current value or component value.
  startYear := extractValue(alStartYear, updStartYear.Position);
  endYear := extractValue(alEndYear, updEndYear.Position);

  startMonth := extractValue(alStartMonth, cmbStartMonth.ItemIndex);
  endMonth := extractValue(alEndMonth, cmbEndMonth.ItemIndex);

  startDay := extractValue(alStartDay, updStartDate.Position);
  endDay := extractValue(alEndDay, updEndDate.Position);

  startHour := extractValue(alStartHour, updStartHour.Position);
  endHour := extractValue(alEndHour, updEndHour.Position);

  startMinute := extractValue(alStartMinute, updStartMinute.Position);
  endMinute := extractValue(alEndMinute, updEndMinute.Position);

  startSeconds := extractValue(alStartSecond, updStartSeconds.Position);
  endSeconds := extractValue(alEndSecond, updEndSeconds.Position);

  // Perform identical value check for date + time.
  if((startYear = endYear) and (startMonth = endMonth) and (startDay = endDay) and
    (startHour = endHour) and (startMinute = endMinute) and (startSeconds = endSeconds)) then
  begin
    exit;
  end;

  // Check either date + time or only a time mode is selected.
  if((startYear < MIN_YEAR) or (endYear < MIN_YEAR) or (startMonth < MIN_MONTH) or
    (endMonth < MIN_MONTH) or (startDay < MIN_DATE) or (endDay < MIN_DATE)) then
  begin
    if(not ((startYear < MIN_YEAR) and (startYear = endYear) and (startMonth < MIN_MONTH) and
      (startMonth = endMonth) and (startDay < MIN_DATE) and (startDay = endDay))) then
    begin
      exit;
    end;
  end;

  // Perform identical value check for time.
  if((startYear <= MIN_YEAR) and (startMonth <= MIN_MONTH) and (startDay <= MIN_DATE) and
    (startHour = endHour) and (startMinute = endMinute) and (startSeconds = endSeconds)) then
  begin
    exit;
  end;

  // In date + time mode : check start date/time is less than the end date/time.
  if(startYear >= MIN_YEAR) then
  begin
    // Start date/time should less then end date/time.
    startDateComp := StrToInt64(Format('%.*d', [4, startYear]) + Format('%.*d', [2, startMonth]) + Format('%.*d', [2, startDay]));
    endDateComp := StrToInt64(Format('%.*d', [4, endYear]) + Format('%.*d', [2, endMonth]) + Format('%.*d', [2, endDay]));

    startTimeComp := StrToInt64(Format('%.*d', [2, startHour]) + Format('%.*d', [2, startMinute]) + Format('%.*d', [2, startSeconds]));
    endTimeComp := StrToInt64(Format('%.*d', [2, endHour]) + Format('%.*d', [2, endMinute]) + Format('%.*d', [2, endSeconds]));

    // Validate date components.
    if(startDateComp <= endDateComp) then
    begin
      // Date component is OK, checking time component.
      if(startTimeComp > endTimeComp) then
      begin
        // Time component is invalid.
        exit;
      end;
    end
    else
    begin
      // Date component is invalid.
      exit;
    end;
  end;

  // Date/time validations are sucessful!
  result := true;
end;

end.


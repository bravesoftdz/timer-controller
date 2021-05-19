//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// Common routines.
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

unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

const
  DEFAULT_ALARM_LENGTH : Int64 = 60 * 60;
  DEFAULT_CHANNEL : Byte = 1;

type
  TTimeInfo = record
    Year: Word;
    Month: Byte;
    Date: Byte;
    Hour: Byte;
    Minute: Byte;
    Second: Byte;
  end;

  TAlarmInfo = record
    StartTime: TTimeInfo;
    EndTime: TTimeInfo;
    Channel: Byte;
  end;

function DateTimeToTimeInfo(date: TDateTime) : TTimeInfo;
function TimeInfoToDateTime(date: TTimeInfo) : TDateTime;

function NewAlarmInfo() : TAlarmInfo;

procedure SaveTimeInfoToStream(data: TTimeInfo; var stream: TMemoryStream);
procedure SaveAlarmInfoToStream(data: TAlarmInfo; var stream: TMemoryStream);

function LoadTimeInfoFromStream(var stream: TMemoryStream) : TTimeInfo;
function LoadAlarmInfoFromStream(var stream: TMemoryStream) : TAlarmInfo;

implementation

function DateTimeToTimeInfo(date : TDateTime) : TTimeInfo;
var
  tempInfo: TTimeInfo;
  tmpYear, tmpMonth, tmpDate: Word;
  tmpHour, tmpMinute, tmpSecond, tmpMs: Word;
begin
  // Extract date and time components from sepcified TDateTime value.
  DecodeDateTime(date, tmpYear, tmpMonth, tmpDate, tmpHour, tmpMinute, tmpSecond, tmpMs);

  // Assign extracted values into data structure.
  tempInfo.Year := tmpYear;
  tempInfo.Month := tmpMonth;
  tempInfo.Date := tmpDate;

  tempInfo.Hour := tmpHour;
  tempInfo.Minute := tmpMinute;
  tempInfo.Second := tmpSecond;

  Result := tempInfo;
end;

function TimeInfoToDateTime(date : TTimeInfo) : TDateTime;
begin
  result := EncodeDateTime(date.Year, date.Month, date.Date, date.Hour, date.Minute, date.Second, 0);
end;

function NewAlarmInfo() : TAlarmInfo;
var
  unixTime: Int64;
  tmpAlarmInfo: TAlarmInfo;
begin
  // Create alarm start time.
  unixTime := DateTimeToUnix(Now);
  tmpAlarmInfo.StartTime := DateTimeToTimeInfo(UnixToDateTime(unixTime));

  // Create alarm stop time.
  unixTime := unixTime + DEFAULT_ALARM_LENGTH;
  tmpAlarmInfo.EndTime := DateTimeToTimeInfo(UnixToDateTime(unixTime));

  // Specify default channel;
  tmpAlarmInfo.Channel := DEFAULT_CHANNEL;
  result := tmpAlarmInfo;
end;

procedure SaveTimeInfoToStream(data: TTimeInfo; var stream: TMemoryStream);
begin
  // 1 Year (2 bytes).
  stream.WriteWord(data.Year);
  // 2 Month (1 byte).
  stream.WriteByte(data.Month);
  // 3 Day (1 byte).
  stream.WriteByte(data.Date);
  // 4 Hour (1 byte).
  stream.WriteByte(data.Hour);
  // 5 Minute (1 byte).
  stream.WriteByte(data.Minute);
  // 6 Seconds (1 byte).
  stream.WriteByte(data.Second);
end;

procedure SaveAlarmInfoToStream(data: TAlarmInfo; var stream: TMemoryStream);
begin
  // Save alarm start configuration.
  SaveTimeInfoToStream(data.StartTime, stream);

  // Save alarm stop configuration.
  SaveTimeInfoToStream(data.EndTime, stream);

  // 13 Channel Number (1 byte).
  stream.WriteByte(data.Channel);
end;

function LoadTimeInfoFromStream(var stream: TMemoryStream) : TTimeInfo;
var
  tmpTimeInfo: TTimeInfo;
begin
  // 1 Year (2 bytes).
  tmpTimeInfo.Year := stream.ReadWord();
  // 2 Month (1 byte).
  tmpTimeInfo.Month := stream.ReadByte();
  // 3 Day (1 byte).
  tmpTimeInfo.Date := stream.ReadByte();
  // 4 Hour (1 byte).
  tmpTimeInfo.Hour := stream.ReadByte();
  // 5 Minute (1 byte).
  tmpTimeInfo.Minute := stream.ReadByte();
  // 6 Seconds (1 byte).
  tmpTimeInfo.Second := stream.ReadByte();

  result := tmpTimeInfo;
end;

function LoadAlarmInfoFromStream(var stream: TMemoryStream) : TAlarmInfo;
var
  tempAlarmInfo: TAlarmInfo;
  tempStartTime, tempEndTime: TTimeInfo;
begin
  // Load alarm start configuration.
  tempStartTime := LoadTimeInfoFromStream(stream);

  // Load alarm stop configuration.
  tempEndTime := LoadTimeInfoFromStream(stream);

  tempAlarmInfo.StartTime := tempStartTime;
  tempAlarmInfo.EndTime := tempEndTime;

  // 13 Channel Number (1 byte).
  tempAlarmInfo.Channel := stream.ReadByte();

  result := tempAlarmInfo;
end;

end.


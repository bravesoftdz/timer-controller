//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// System time window.
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

unit utimeconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ucommon;

type

  { TfrmTimeConfig }

  TfrmTimeConfig = class(TForm)
    bvPCTimeFrame: TBevel;
    btnSync: TButton;
    btnClose: TButton;
    btnSyncToPC: TButton;
    btnReload: TButton;
    cmbMonth: TComboBox;
    lblTimeView: TLabel;
    lblDate: TLabel;
    lblMonth: TLabel;
    lblHour: TLabel;
    lblMinute: TLabel;
    lblSeconds: TLabel;
    lblSystemDate: TLabel;
    lblSystemTime: TLabel;
    lblPCTime: TLabel;
    lblYear: TLabel;
    tmrPCClockSync: TTimer;
    txtDate: TEdit;
    txtHour: TEdit;
    txtMinute: TEdit;
    txtSeconds: TEdit;
    txtYear: TEdit;
    updDate: TUpDown;
    updHour: TUpDown;
    updMinute: TUpDown;
    updSeconds: TUpDown;
    updYear: TUpDown;
    mainWindow: TForm;
    procedure btnCloseClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure btnSyncToPCClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrPCClockSyncTimer(Sender: TObject);

  private

  public
    procedure SetMainWindow(mForm:  TForm);
    procedure SetSystemTime(sysTime: TTimeInfo);
  end;

var
  frmTimeConfig: TfrmTimeConfig;

implementation

uses
  umain;

{$R *.lfm}

{ TfrmTimeConfig }

procedure TfrmTimeConfig.SetMainWindow(mForm:  TForm);
begin
  mainWindow := mForm;
end;

procedure TfrmTimeConfig.tmrPCClockSyncTimer(Sender: TObject);
begin
  lblTimeView.Caption := FormatDateTime('mmmm d - yyyy         hh:nn:ss', Now);
end;

procedure TfrmTimeConfig.FormShow(Sender: TObject);
begin
  // Set current PC time on UI.
  Self.CancelControl := btnClose;
  tmrPCClockSyncTimer(Sender);
end;

procedure TfrmTimeConfig.btnReloadClick(Sender: TObject);
begin
  TfrmMain(mainWindow).GetSystemTime();
end;

procedure TfrmTimeConfig.btnSyncClick(Sender: TObject);
begin
  // Set date/time to the given values.
  TfrmMain(mainWindow).SetSystemTime((updYear.Position - 2000), (cmbMonth.ItemIndex + 1), updDate.Position, updHour.Position, updMinute.Position, updSeconds.Position);
  ModalResult := mrOK;
end;

procedure TfrmTimeConfig.btnSyncToPCClick(Sender: TObject);
var
  pcTime: TTimeInfo;
begin
  // Set date/time to the PC date/time.
  pcTime := DateTimeToTimeInfo(Now);
  TfrmMain(mainWindow).SetSystemTime((pcTime.Year - 2000), pcTime.Month, pcTime.Date, pcTime.Hour, pcTime.Minute, pcTime.Second);
  ModalResult := mrOK;
end;

procedure TfrmTimeConfig.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TfrmTimeConfig.SetSystemTime(sysTime: TTimeInfo);
begin
  updYear.Position := sysTime.Year + 2000;
  cmbMonth.ItemIndex := (sysTime.Month - 1);
  updDate.Position := sysTime.Date;
  updHour.Position := sysTime.Hour;
  updMinute.Position := sysTime.Minute;
  updSeconds.Position := sysTime.Second;
end;

end.


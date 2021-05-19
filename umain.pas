//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// Main window.
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

unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, ualarm, ucommon, LCLType, uabout, utimeconfig, Clipbrd,
  Menus, LazSerial, LazSynaSer, userial;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    beSep1: TBevel;
    beSep2: TBevel;
    beSep3: TBevel;
    beSep4: TBevel;
    beSep5: TBevel;
    beSep6: TBevel;
    cmbPortName: TComboBox;
    imglMenu: TImageList;
    comDrv: TLazSerial;
    lblMaxChannels: TLabel;
    lblMaxTimers: TLabel;
    lblPortName: TLabel;
    dlgOpenFile: TOpenDialog;
    mnuAddTimer: TMenuItem;
    mnuDelTimer: TMenuItem;
    mnuPaste: TMenuItem;
    mnuSelect: TMenuItem;
    mnuClear: TMenuItem;
    N2: TMenuItem;
    mnuSep1: TMenuItem;
    pnlStatus: TPanel;
    pnlToolbar: TPanel;
    btnConnect: TSpeedButton;
    btnInfo: TSpeedButton;
    btnDisconnect: TSpeedButton;
    btnAddTimer: TSpeedButton;
    btnDelTimer: TSpeedButton;
    btnSetSysTime: TSpeedButton;
    btnSync: TSpeedButton;
    btnSave: TSpeedButton;
    btnOpen: TSpeedButton;
    btnClear: TSpeedButton;
    dlgSaveFile: TSaveDialog;
    mnuMainPopup: TPopupMenu;
    progMain: TProgressBar;
    scrTimerList: TScrollBox;
    tmrSerial: TTimer;
    tmrInit: TTimer;
    tmrDelete: TTimer;
    procedure btnAddTimerClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDelTimerClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSetSysTimeClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure fmTimerOnValueChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuMainPopupPopup(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuSelectClick(Sender: TObject);
    procedure SetButtonState(isItemsExist : Boolean);
    procedure ShowSystemTimeWindow(sysTime: TTimeInfo);
    procedure SetConnectionState();
    procedure tmrDeleteTimer(Sender: TObject);
    procedure tmrInitTimer(Sender: TObject);
    procedure tmrSerialTimer(Sender: TObject);
  private
    alarmUid : Int64;
    valueChanged: Boolean;
    currentFileName: string;
    timerName: string;
    channelResetCount: byte;
    currentDevCmd: TDeviceCommand;
    commandDevRetry: Word;
    responseData: string;
    responseLen: Word;
    FMaxChannelCount: byte;
    FMaxTimerCount: byte;
    activeAlarmCount: byte;
    currentAlarmPos: byte;
    tempTimeInfo: TAlarmInfo;
    FClipboardFormat: TClipboardFormat;
    fmSystemTime : TfrmTimeConfig;
    function ConvByteToStr(inData: Byte) : string;
    procedure SaveDevNameToMRU(devName: string);
    procedure LoadMRUList();
    procedure OnHandShakeComplete(isTimeout: Boolean);
    procedure OnDeviceInfoAvailable(isTimeout: Boolean; infoData: string);
    procedure OnGetAlarmCount(isTimeout: Boolean; infoData: string);
    procedure OnGetSystemTime(isTimeout: Boolean; infoData: string);
    procedure OnGetAlarmStartTime(isTimeout: Boolean; infoData: string);
    procedure OnGetAlarmEndTime(isTimeout: Boolean; infoData: string);
    procedure OnSetAlarmCount(isTimeout: Boolean);
    procedure OnSetSystemTime(isTimeout: Boolean);
    procedure OnSetAlarmStartTime(isTimeout: Boolean);
    procedure OnSetAlarmEndTime(isTimeout: Boolean);
    procedure OnTimeout(cmdID: TDeviceCommand; var ignore: Boolean);
  public
    alarmList : TList;
    property MaxChannel : byte read FMaxChannelCount write FMaxChannelCount;
    property TimerCount : byte read FMaxTimerCount write FMaxTimerCount;
    property ClipboardRegFormat : TClipboardFormat read FClipboardFormat write FClipboardFormat;
    function AddTimer(alarmData: TAlarmInfo; maxChannelCount: Byte) : TfmTimer;
    procedure DeleteTimerAsync(timer: string);
    procedure DeleteTimer(timer: string);
    procedure SetChangeFlag(isChanged : Boolean);
    procedure OpenConfigurationFile(filename: string);
    procedure PasteNewTimer();
    procedure DuplicateTimer(alarmData: TAlarmInfo);
    procedure DeviceHandShake();
    procedure DeviceInfo();
    procedure GetAlarmCount();
    procedure GetSystemTime();
    procedure GetAlarmStartTime(alarmID : Byte);
    procedure GetAlarmEndTime(alarmID : Byte);
    procedure SetAlarmCount(alarmCount: Byte);
    procedure SetSystemTime(year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
    procedure SetAlarmStartTime(alarmID: Byte; channelID: Byte; year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
    procedure SetAlarmEndTime(alarmID: Byte; year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
    function CheckDeviceConnection() : boolean;
  end;

const
  DEFAULT_CHANNEL_COUNT: byte = 1;
  DEFAULT_TIMER_COUNT: byte = 8;

  MRU_CONFIG_FILE_NAME: string = 'timerprog.mru';
  DEFAULT_FILE_NAME: string = 'untitled.tpf';
  CLIPBOARD_FORMAT: string = 'application/x-timerprog';

  FILE_ID_BYTE: Byte = $21;

  DEV_RETRY_COUNT: Word = 5;
var
  frmMain: TfrmMain;

resourcestring
  srDeleteNoItems = 'Select timer configuration(s) to delete';
  srDeleteConfirm = 'Delete selected alarm configuration(s)?';
  srMaxAlarm = 'Maximum number of alarm configurations reached';
  srMaxLoad = 'Only the maximum number of alarm configurations are loaded';
  srFileSaveError = 'An error occurred while saving the file';
  srFileOpenError = 'An error occurred while openning the file';
  srUnsupportedFile = 'Unsupported file or corrupted file';
  srClearConfirmation = 'Clear all alarm configuration(s) and start new session?';
  srChannelReset = ' timer channel(s) got reset to the maximum channel limit';
  srUnsavedData = 'Existing configuration is changed!' + LineEnding + 'Continue without saving the existing configuration?';
  srCommunicationError = 'Communication error';
  srComPortNotDefined = 'Communication port is not specified';
  srDevieNotResponsive = 'Connected device is not responsive';
  srDeviceNotConnected = 'Connection to the device is not available';
  srDataCorrupted = 'Received data is invalid or corrupted';
  srInvalidAlarmConfig = 'Invalid timer configuration(s) detected';

implementation

{$R *.lfm}

{ TfrmMain }

function TfrmMain.CheckDeviceConnection() : boolean;
begin
  result := comDrv.Active;
  if(not result) then
  begin
    // Device connection is not available.
    SetConnectionState();
    MessageDlg(Application.Title, srDeviceNotConnected, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
end;

procedure TfrmMain.SetConnectionState();
begin
  btnDisconnect.Enabled := comDrv.Active;
  btnConnect.Enabled := not btnDisconnect.Enabled;
  btnSetSysTime.Enabled := btnDisconnect.Enabled;
  btnSync.Enabled := btnDisconnect.Enabled;
end;

procedure TfrmMain.SetButtonState(isItemsExist : Boolean);
begin
  btnDelTimer.Enabled := isItemsExist;
  btnSave.Enabled := isItemsExist;
  btnClear.Enabled := isItemsExist;

  mnuDelTimer.Enabled := isItemsExist;
  mnuSelect.Enabled := isItemsExist;
end;

procedure TfrmMain.tmrDeleteTimer(Sender: TObject);
begin
  tmrDelete.Enabled := false;
  DeleteTimer(timerName);
end;

procedure TfrmMain.tmrInitTimer(Sender: TObject);
begin
  tmrInit.Enabled := false;

  // Send handshake request to the device.
  DeviceHandShake();
end;

procedure TfrmMain.SetChangeFlag(isChanged : Boolean);
begin
  valueChanged := isChanged;
  self.Caption := Application.Title;

  if(isChanged) then
  begin
    // Place mark on title bar if the content is changed.
    self.Caption := self.Caption + '*';
  end;
end;

procedure TfrmMain.DeleteTimerAsync(timer: string);
begin
  timerName := timer;
  tmrDelete.Enabled := true;
end;

procedure TfrmMain.DeleteTimer(timer: string);
var
  selPos: Word;
  tmpAlarmItem: TfmTimer;
begin
  selPos := 0;

  // Looking for the specified timer name.
  while(selPos < alarmList.Count) do
  begin
    tmpAlarmItem := TfmTimer(alarmList.Items[selPos]);
    if((tmpAlarmItem <> nil) and (tmpAlarmItem.Name = timer)) then
    begin
      // Delete current timer from the list.
      Application.ProcessMessages;

      FreeAndNil(tmpAlarmItem);
      alarmList.Delete(selPos);

      // Reset value changed flag.
      SetChangeFlag((alarmList.Count > 0));
      break;
    end;

    inc(selPos);
  end;

  // changed UI button state based on available alarm configurations.
  SetButtonState((alarmList.Count > 0));
end;

procedure TfrmMain.PasteNewTimer();
var
  clipboardBuffer: TMemoryStream;
  alarmInfo: TAlarmInfo;
begin
  if(alarmList.Count < TimerCount) then
  begin
    // Check current clipboard format and try to load data.
    if(Clipboard.HasFormat(ClipboardRegFormat)) then
    begin
      try
        clipboardBuffer := TMemoryStream.Create;

        // Get alarm information from clipboard.
        Clipboard.GetFormat(ClipboardRegFormat, clipboardBuffer);
        clipboardBuffer.Position := 0;
        alarmInfo := LoadAlarmInfoFromStream(clipboardBuffer);

        // Create new timer panel and apply values.
        AddTimer(alarmInfo, MaxChannel);

        // Reset value changed flag and UI button state.
        SetChangeFlag(true);
        SetButtonState(true);
      finally
        FreeAndNil(clipboardBuffer);
        self.Invalidate;
      end;
    end;
  end
  else
  begin
    // Maximum alarm count is reached.
    MessageDlg(Application.Title, srMaxAlarm, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end;
end;

function TfrmMain.AddTimer(alarmData: TAlarmInfo; maxChannelCount: Byte) : TfmTimer;
var
  tempAlarmUnit : TfmTimer;
begin
  // Create new alarm panel.
  tempAlarmUnit := TfmTimer.Create(self, frmMain);
  tempAlarmUnit.Name := 'fmAlarmUnit' + IntToStr(alarmUid);

  // Increment UID value to provide unique component names.
  inc(alarmUid);

  // Add new alarm panel to the end of the list.
  if(alarmList.Count > 0) then
  begin
    if(alarmList.Items[alarmList.Count - 1] <> nil) then
    begin
      tempAlarmUnit.Top := TfmTimer(alarmList.Items[alarmList.Count - 1]).Top + tempAlarmUnit.Height;
    end;
  end;

  tempAlarmUnit.Align := alTop;
  tempAlarmUnit.Parent := scrTimerList;
  alarmList.Add(tempAlarmUnit);

  // Assign dummy alarm information to the panel.
  tempAlarmUnit.SetAlarmInfo(alarmData, maxChannelCount);
  tempAlarmUnit.OnValueChanged := @fmTimerOnValueChanged;

  // Count number of channel resets.
  if(tempAlarmUnit.IsChannelReset()) then
  begin
    inc(channelResetCount);
  end;

  result := tempAlarmUnit;
end;

procedure TfrmMain.DuplicateTimer(alarmData: TAlarmInfo);
begin
  if(alarmList.Count < TimerCount) then
  begin
    AddTimer(alarmData, MaxChannel);

    // Reset value changed flag and UI button state.
    SetChangeFlag(true);
    SetButtonState(true);
  end
  else
  begin
    // Maximum alarm count is reached.
    MessageDlg(Application.Title, srMaxAlarm, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.btnAddTimerClick(Sender: TObject);
begin
  if(alarmList.Count < TimerCount) then
  begin
    AddTimer(NewAlarmInfo(), MaxChannel);

    // Reset value changed flag and UI button state.
    SetChangeFlag(true);
    SetButtonState(true);
  end
  else
  begin
    // Maximum alarm count is reached.
    MessageDlg(Application.Title, srMaxAlarm, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.fmTimerOnValueChanged(Sender: TObject);
begin
  SetChangeFlag(true);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  // Setup status bar values.
  lblMaxChannels.Caption := IntToStr(MaxChannel);
  lblMaxTimers.Caption := IntToStr(TimerCount);

  // Load MRU device names if exists.
  LoadMRUList();

  if((Application.ParamCount > 0) and (FileExists(Application.Params[1]))) then
  begin
    // Try to open file specified in commandline arguments.
    OpenConfigurationFile(Application.Params[1]);
  end
  else
  begin
    // Commandline arguments are not specified.
    SetButtonState(false);
  end;
end;

procedure TfrmMain.mnuMainPopupPopup(Sender: TObject);
begin
  mnuPaste.Enabled := Clipboard.HasFormat(ClipboardRegFormat);
end;

procedure TfrmMain.mnuPasteClick(Sender: TObject);
begin
  PasteNewTimer();
end;

procedure TfrmMain.mnuSelectClick(Sender: TObject);
var
  selPos: Word;
begin
  if(alarmList.Count > 0) then
  begin
    selPos := 0;

    // Get an each timer panel in the scroll-box to mark the checkbox.
    while(selPos < alarmList.Count) do
    begin
      if(alarmList.Items[selPos] <> nil) then
      begin
        TfmTimer(alarmList.Items[selPos]).chkSelect.Checked := true;
      end;
      inc(selPos);
    end;

    scrTimerList.Invalidate;
  end;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
var
  selPos: Word;
  tmpAlarmItem: TfmTimer;
begin
  if((Sender <> nil) and (alarmList.Count > 0)) then
  begin
    if(MessageDlg(Application.Title, srClearConfirmation, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo) then
    begin
      // Clear operation is canceled by the user.
      exit;
    end;
  end;

  if(alarmList.Count > 0) then
  begin
    selPos := 0;

    while(selPos < alarmList.Count) do
    begin
      // Remove selected panel.
      tmpAlarmItem := TfmTimer(alarmList.Items[selPos]);
      FreeAndNil(tmpAlarmItem);

      // Remove alarm panel reference from the list.
      alarmList.Delete(selPos);
      Continue;
    end;

    // Reset value changed flag and UI button state.
    SetChangeFlag(false);
    SetButtonState(false);
  end;

  // Reset session based variables.
  channelResetCount := 0;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  try
    if(Trim(cmbPortName.Text) = '') then
    begin
      // Communication port name is not specified;
      MessageDlg(Application.Title, srComPortNotDefined, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      exit;
    end;

    // Connecting with the specified communication port...
{$IFDEF LINUX}
    comDrv.Device := '/dev/' + Trim(cmbPortName.Text);
{$ELSE}
    comDrv.Device := Trim(cmbPortName.Text);
{$ENDIF}

    comDrv.Open;
    btnClearClick(nil);
    tmrInit.Enabled := comDrv.Active;
  except on E: Exception do
    // Communication error has occured.
    MessageDlg(Application.Title, (srCommunicationError + LineEnding + E.Message), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.btnDelTimerClick(Sender: TObject);
var
  selCount, selPos: Word;
  tmpAlarmItem: TfmTimer;
begin
  if(alarmList.Count > 0) then
  begin
    selCount := 0;
    selPos := 0;

    // Count number of selected alarm configurations.
    while(selPos < alarmList.Count) do
    begin
      if((alarmList.Items[selPos] <> nil) and (TfmTimer(alarmList.Items[selPos]).IsSelected())) then
      begin
        inc(selCount);
      end;
      inc(selPos);
    end;

    if(selCount = 0) then
    begin
      // Items are not available to delete!
      MessageDlg(Application.Title, srDeleteNoItems, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
      if(MessageDlg(Application.Title, srDeleteConfirm, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
      begin
        // Deleting selected alarm configurations!
        selPos := 0;

        while(selPos < alarmList.Count) do
        begin
          if((alarmList.Items[selPos] <> nil) and (TfmTimer(alarmList.Items[selPos]).IsSelected())) then
          begin
            // Remove selected panel.
            tmpAlarmItem := TfmTimer(alarmList.Items[selPos]);
            FreeAndNil(tmpAlarmItem);

            // Remove alarm panel reference from the list.
            alarmList.Delete(selPos);
            Continue;
          end;
          inc(selPos);
        end;

        // Reset value changed flag.
        SetChangeFlag((alarmList.Count > 0));
      end;
    end;

    // changed UI button state based on available alarm configurations.
    SetButtonState((alarmList.Count > 0));
  end;
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  comDrv.Close;

  // Update UI controls based on connection status.
  SetConnectionState();
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
var
  fmAbout: TfrmAbout;
begin
  // Show application version information.
  fmAbout := TfrmAbout.Create(self);
  fmAbout.ShowModal;
  FreeAndNil(fmAbout);
end;

procedure TfrmMain.OpenConfigurationFile(filename: string);
var
  dataStream: TMemoryStream;
  alarmRecordCount, alarmPos: Word;
  alarmRec: TAlarmInfo;
begin
  dataStream := TMemoryStream.Create;
  channelResetCount := 0;

  try
    // Clear existing alarm configuration(s).
    btnClearClick(nil);

    dataStream.LoadFromFile(filename);

    // Check for valid file header.
    if(dataStream.ReadByte <> FILE_ID_BYTE) then
    begin
      // Unsupported file type or corrupted file.
      raise Exception.Create(srUnsupportedFile);
    end;

    alarmRecordCount := dataStream.ReadWord();
    alarmPos := 0;

    if(alarmRecordCount > TimerCount) then
    begin
      // Only the maximum number of alarm configurations are loading.
      alarmRecordCount := TimerCount;
      MessageDlg(Application.Title, srMaxLoad, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    end;

    while(alarmRecordCount <> alarmPos) do
    begin
      // Get alarm record from the file.
      alarmRec := LoadAlarmInfoFromStream(dataStream);
      AddTimer(alarmRec, MaxChannel);
      inc(alarmPos);
    end;

    // Reset value changed flag and UI button state based on available alarm configuration.
    SetChangeFlag(false);
    SetButtonState((alarmList.Count > 0));

    // Set open file name as default file name.
    currentFileName := filename;

    // Notify channel reset count to the user.
    if(channelResetCount > 0) then
    begin
      MessageDlg(Application.Title, (IntToStr(channelResetCount) + srChannelReset), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    end;

  except on E : Exception do
    // File open / read operation has failed!
    MessageDlg(Application.Title, (srFileOpenError + LineEnding + E.Message), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;

  FreeAndNil(dataStream);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  // Check for any unsaved alarm configuration.
  if((valueChanged) and (MessageDlg(Application.Title, srUnsavedData, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo))then
  begin
    // Cancel file open operation as selected by user.
    exit;
  end;

  dlgOpenFile.FileName := DEFAULT_FILE_NAME;
  dlgOpenFile.InitialDir := ExtractFilePath(currentFileName);

  if(dlgOpenFile.Execute()) then
  begin
    OpenConfigurationFile(dlgOpenFile.FileName);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  dataStream: TMemoryStream;
  resPos: Word;
  tmpAlarmInfo: TAlarmInfo;
begin
  if(alarmList.Count > 0) then
  begin
    // Show save file dialog box.
    dlgSaveFile.FileName := ExtractFileName(currentFileName);
    dlgSaveFile.InitialDir := ExtractFilePath(currentFileName);
    if(dlgSaveFile.Execute()) then
    begin
      // Save alarm configuration into the specified file.
      dataStream := TMemoryStream.Create;

      // 1. File ID (1 byte)
      dataStream.WriteByte(FILE_ID_BYTE);

      // 2. Number of alarm records (2 bytes).
      dataStream.WriteWord(alarmList.Count);

      // 3. Alarm data values.
      resPos := 0;
      while(resPos < alarmList.Count) do
      begin
        tmpAlarmInfo := TfmTimer(alarmList.Items[resPos]).GetAlarmInfo();
        SaveAlarmInfoToStream(tmpAlarmInfo, dataStream);
        Inc(resPos);
      end;

      try
        // Save data stream into the file.
        dataStream.SaveToFile(dlgSaveFile.FileName);
        SetChangeFlag(false);

        // Set open file name as default file name.
        currentFileName := dlgSaveFile.FileName;
      except on E : Exception do
        // File save operation has failed!
        MessageDlg(Application.Title, (srFileSaveError + LineEnding + E.Message), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;

      FreeAndNil(dataStream);
    end;
  end;
end;

procedure TfrmMain.btnSetSysTimeClick(Sender: TObject);
begin
  if(CheckDeviceConnection())then
  begin
    // Get system time of the connected device.
    GetSystemTime();
  end;
end;

procedure TfrmMain.btnSyncClick(Sender: TObject);
var
  alarmPos: Word;
  tmpAlarmItem: TfmTimer;
begin
  if(CheckDeviceConnection())then
  begin
    // Verify the error state of the alarm configurations.
    alarmPos := 0;

    while(alarmPos < alarmList.Count)do
    begin
      tmpAlarmItem := TfmTimer(alarmList.Items[alarmPos]);
      if(not tmpAlarmItem.IsValidConfiguration(alNone, 0)) then
      begin
        // Validation failure detected.
        MessageDlg(Application.Title, srInvalidAlarmConfig, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
        exit;
      end;
      inc(alarmPos);
    end;

    // Alarm configuration validations are successful. Program current alarm count into device.
    currentAlarmPos := 0;
    activeAlarmCount := alarmList.Count;
    if(activeAlarmCount > TimerCount) then
    begin
      // Trim alarm count to the maximum limit.
      activeAlarmCount := TimerCount;
    end;

    btnSync.Enabled := false;
    SetAlarmCount(activeAlarmCount);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    // Close serial communication channel.
    if(comDrv.Active) then
    begin
      comDrv.Close;
    end;
  finally
    SetConnectionState();
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Create alarm list.
  alarmList := TList.Create;
  alarmUid := 0;
  channelResetCount := 0;
  commandDevRetry := 0;

  // Setup default data.
  valueChanged := false;
  currentFileName := DEFAULT_FILE_NAME;
  currentDevCmd := CmdNone;

  MaxChannel := DEFAULT_CHANNEL_COUNT;
  TimerCount := DEFAULT_TIMER_COUNT;

  // Register custom clipboard format.
  ClipboardRegFormat := RegisterClipboardFormat(CLIPBOARD_FORMAT);
end;

procedure TfrmMain.tmrSerialTimer(Sender: TObject);
var
  isIgnoreCommand: Boolean;
  payload: string;
begin
  tmrSerial.Enabled := false;

  // Check for retry limit.
  if(commandDevRetry >= DEV_RETRY_COUNT) then
  begin
    // Stop retry cycle.
    commandDevRetry := 0;

    // Raise timeout event.
    isIgnoreCommand := true;
    OnTimeout(currentDevCmd, isIgnoreCommand);

    if(not isIgnoreCommand) then
    begin
      // Continue to command event with timeout flag.
      case currentDevCmd of
        CmdHandshake:
          OnHandShakeComplete(true);
        CmdDevInfo:
          OnDeviceInfoAvailable(true, '');
        CmdGetAlarmCount:
          OnGetAlarmCount(true, '');
        CmdGetSysTime:
          OnGetSystemTime(true, '');
        CmdGetStartAlarmTime:
          OnGetAlarmStartTime(true, '');
        CmdGetEndAlarmTime:
          OnGetAlarmEndTime(true, '');
        CmdSetAlarmCount:
          OnSetAlarmCount(true);
        CmdSetSysTime:
          OnSetSystemTime(true);
        CmdSetStartAlarmTime:
          OnSetAlarmStartTime(true);
        CmdSetEndAlarmTime:
          OnSetAlarmEndTime(true);
      end;
    end;

    // Exit until next timer trigger.
    exit;
  end;

  responseData := responseData + comDrv.ReadData;

  // Check for complete response from device.
  if(currentDevCmd <> CmdNone) then
  begin
    // Check for complete response.
    if(Length(responseData) < responseLen) then
    begin
      // Need to wait for complete response.
      inc(commandDevRetry);
      tmrSerial.Enabled := true;
    end
    else if(Length(responseData) = responseLen) then
    begin
      // Complete response is received from device.
      commandDevRetry := 0;
      payload := Trim(Copy(responseData, (RESP_LEN_HEADER + 1), (Length(responseData) - RESP_LEN_HEADER)));

      // Raise command event.
      case currentDevCmd of
         CmdHandshake:
           OnHandShakeComplete(false);
         CmdDevInfo:
           OnDeviceInfoAvailable(false, payload);
         CmdGetAlarmCount:
          OnGetAlarmCount(false, payload);
        CmdGetSysTime:
          OnGetSystemTime(false, payload);
        CmdGetStartAlarmTime:
          OnGetAlarmStartTime(false, payload);
        CmdGetEndAlarmTime:
          OnGetAlarmEndTime(false, payload);
        CmdSetAlarmCount:
          OnSetAlarmCount(false);
        CmdSetSysTime:
          OnSetSystemTime(false);
        CmdSetStartAlarmTime:
          OnSetAlarmStartTime(false);
        CmdSetEndAlarmTime:
          OnSetAlarmEndTime(false);
      end;
    end;
  end;
end;

function TfrmMain.ConvByteToStr(inData: Byte) : string;
  begin
    result := UpperCase(IntToHex(inData, 2));
  end;

procedure TfrmMain.DeviceHandShake();
begin
  currentDevCmd := CmdHandshake;
  responseLen := RESP_LEN_HANDSHAKE;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_HANDSHAKE);
  tmrSerial.Interval := TIME_HANDHSAKE;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.DeviceInfo();
begin
  currentDevCmd := CmdDevInfo;
  responseLen := RESP_LEN_DEVICE_INFO;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_DEVICE_INFO);
  tmrSerial.Interval := TIME_DEVICE_INFO;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.GetAlarmCount();
begin
  currentDevCmd := CmdGetAlarmCount;
  responseLen := RESP_LEN_GET_ALARM_COUNT;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_GET_ALARM_COUNT);
  tmrSerial.Interval := TIME_GET_ALARM_COUNT;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.GetSystemTime();
begin
  currentDevCmd := CmdGetSysTime;
  responseLen := RESP_LEN_GET_SYS_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_GET_SYS_TIME);
  tmrSerial.Interval := TIME_GET_SYS_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.GetAlarmStartTime(alarmID : Byte);
begin
  currentDevCmd := CmdGetStartAlarmTime;
  responseLen := RESP_LEN_GET_START_ALARM_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_GET_START_ALARM_TIME + ConvByteToStr(alarmID));
  tmrSerial.Interval := TIME_GET_START_ALARM_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.GetAlarmEndTime(alarmID : Byte);
begin
  currentDevCmd := CmdGetEndAlarmTime;
  responseLen := RESP_LEN_GET_END_ALARM_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_GET_END_ALARM_TIME + ConvByteToStr(alarmID));
  tmrSerial.Interval := TIME_GET_END_ALARM_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.SetAlarmCount(alarmCount: Byte);
begin
  currentDevCmd := CmdSetAlarmCount;
  responseLen := RESP_LEN_SET_ALARM_COUNT;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_SET_ALARM_COUNT + ConvByteToStr(alarmCount));
  tmrSerial.Interval := TIME_SET_ALARM_COUNT;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.SetSystemTime(year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
begin
  currentDevCmd := CmdSetSysTime;
  responseLen := RESP_LEN_SET_SYS_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_SET_SYS_TIME + ConvByteToStr(year) + ConvByteToStr(month) + ConvByteToStr(date) + ConvByteToStr(hour) + ConvByteToStr(minutes) + ConvByteToStr(seconds));
  tmrSerial.Interval := TIME_SET_SYS_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.SetAlarmStartTime(alarmID: Byte; channelID: Byte; year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
begin
  currentDevCmd := CmdSetStartAlarmTime;
  responseLen := RESP_LEN_SET_START_ALARM_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_SET_START_ALARM_TIME + ConvByteToStr(alarmID) + ConvByteToStr(channelID) + ConvByteToStr(year) + ConvByteToStr(month) + ConvByteToStr(date) + ConvByteToStr(hour) + ConvByteToStr(minutes) + ConvByteToStr(seconds));
  tmrSerial.Interval := TIME_SET_START_ALARM_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.SetAlarmEndTime(alarmID: Byte; year: Byte; month: Byte; date: Byte; hour: Byte; minutes: Byte; seconds: Byte);
begin
  currentDevCmd := CmdSetEndAlarmTime;
  responseLen := RESP_LEN_SET_END_ALARM_TIME;
  responseData := '';
  commandDevRetry := 0;

  // Send data into device and start timeout / response capture timer.
  comDrv.WriteData(DEV_CMD_SET_END_ALARM_TIME + ConvByteToStr(alarmID) + ConvByteToStr(year) + ConvByteToStr(month) + ConvByteToStr(date) + ConvByteToStr(hour) + ConvByteToStr(minutes) + ConvByteToStr(seconds));
  tmrSerial.Interval := TIME_SET_END_ALARM_TIME;
  tmrSerial.Enabled := true;
end;

procedure TfrmMain.OnTimeout(cmdID: TDeviceCommand; var ignore: Boolean);
begin
  // Command timeout occurs.
  MessageDlg(Application.Title, srDevieNotResponsive, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);

  try
    // Close current communication session.
    comDrv.Close;
  finally
    // Update UI controls based on connection status.
    SetConnectionState();
  end;
end;

procedure TfrmMain.OnHandShakeComplete(isTimeout: Boolean);
begin
  // Received acknowledgement from the device. Save device name into MRU list.
  SaveDevNameToMRU(cmbPortName.Text);

  //next try to get device info.
  DeviceInfo();
end;

procedure TfrmMain.OnDeviceInfoAvailable(isTimeout: Boolean; infoData: string);
begin
  if(Length(infoData) = (RESP_LEN_DEVICE_INFO - RESP_LEN_HEADER)) then
  begin
    // Data length is accepted, decode the received data to get max alarm count and channel count.
    MaxChannel := StrToIntDef('$' + Copy(infoData, 1, 2), DEFAULT_CHANNEL_COUNT);
    TimerCount :=  StrToIntDef('$' + Copy(infoData, 3, 2), DEFAULT_TIMER_COUNT);

    // Update status bar values.
    lblMaxChannels.Caption := IntToStr(MaxChannel);
    lblMaxTimers.Caption := IntToStr(TimerCount);

    // Update UI controls based on connection status.
    SetConnectionState();
    progMain.Position := 0;

    // Get configured alarm count.
    GetAlarmCount();
  end
  else
  begin
    // Data content is invalid. Should receive string with 4 characters.
    MessageDlg(Application.Title, srDataCorrupted, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.OnGetAlarmCount(isTimeout: Boolean; infoData: string);
begin
  if(Length(infoData) = (RESP_LEN_GET_ALARM_COUNT - RESP_LEN_HEADER)) then
  begin
    // Data length is accepted, decode the received data to get configured alarm count.
    activeAlarmCount := StrToIntDef('$' + infoData, 0);
    currentAlarmPos := 0;

    // Load configured alarm configurations.
    if(activeAlarmCount > 0) then
    begin
      // Update progress bar based on the available alarm count.
      progMain.Max := activeAlarmCount;
      progMain.Position := 0;

      GetAlarmStartTime(currentAlarmPos);
    end
    else
    begin
      // No alarm configurations are available. Update the UI status.
      progMain.Position := 0;
      SetChangeFlag(false);
      SetButtonState((alarmList.Count > 0));
    end;
  end
  else
  begin
    // Data content is invalid. Should receive string with 2 characters.
    MessageDlg(Application.Title, srDataCorrupted, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.ShowSystemTimeWindow(sysTime: TTimeInfo);
begin
  fmSystemTime := TfrmTimeConfig.Create(self);
  fmSystemTime.SetSystemTime(sysTime);
  fmSystemTime.SetMainWindow(self);
  fmSystemTime.ShowModal();
  FreeAndNil(fmSystemTime);
end;

procedure TfrmMain.OnGetSystemTime(isTimeout: Boolean; infoData: string);
var
  sysDateTime: TTimeInfo;
begin
  if(Length(infoData) = (RESP_LEN_GET_SYS_TIME - RESP_LEN_HEADER)) then
  begin
    // Data length is accepted, decode the received time info.
    sysDateTime.Year := StrToIntDef('$' + Copy(infoData, 1, 2), 0);
    sysDateTime.Month := StrToIntDef('$' + Copy(infoData, 3, 2), 0);
    sysDateTime.Date := StrToIntDef('$' + Copy(infoData, 5, 2), 0);
    sysDateTime.Hour := StrToIntDef('$' + Copy(infoData, 7, 2), 12);
    sysDateTime.Minute := StrToIntDef('$' + Copy(infoData, 9, 2), 0);
    sysDateTime.Second := StrToIntDef('$' + Copy(infoData, 11, 2), 0);

    if(fmSystemTime = nil) then
    begin
      // Show system time view/edit window.
      ShowSystemTimeWindow(sysDateTime);
    end
    else
    begin
      // Update received time on existing window.
      fmSystemTime.SetSystemTime(sysDateTime);
    end;

  end
  else
  begin
    // Data content is invalid. Should receive string with 12 characters.
    MessageDlg(Application.Title, srDataCorrupted, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.OnGetAlarmStartTime(isTimeout: Boolean; infoData: string);
begin
  if(Length(infoData) = (RESP_LEN_GET_START_ALARM_TIME - RESP_LEN_HEADER)) then
  begin
    // Data length is accepted, decode the alarm start date/time and channel data.
    tempTimeInfo.Channel := StrToIntDef('$' + Copy(infoData, 1, 2), 0);

    tempTimeInfo.StartTime.Year := 2019 + StrToIntDef('$' + Copy(infoData, 3, 2), 0);
    tempTimeInfo.StartTime.Month := StrToIntDef('$' + Copy(infoData, 5, 2), 0);
    tempTimeInfo.StartTime.Date := StrToIntDef('$' + Copy(infoData, 7, 2), 0);
    tempTimeInfo.StartTime.Hour := StrToIntDef('$' + Copy(infoData, 9, 2), 12);
    tempTimeInfo.StartTime.Minute := StrToIntDef('$' + Copy(infoData, 11, 2), 0);
    tempTimeInfo.StartTime.Second := StrToIntDef('$' + Copy(infoData, 13, 2), 0);

    // Load current alarm end date/time configuration.
    GetAlarmEndTime(currentAlarmPos);
  end
  else
  begin
    // Data content is invalid. Should receive string with 16 characters.
    MessageDlg(Application.Title, srDataCorrupted, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.OnGetAlarmEndTime(isTimeout: Boolean; infoData: string);
begin
  if(Length(infoData) = (RESP_LEN_GET_END_ALARM_TIME - RESP_LEN_HEADER)) then
  begin
    // Data length is accepted, decode the alarm end date/time.
    tempTimeInfo.EndTime.Year := 2019 + StrToIntDef('$' + Copy(infoData, 1, 2), 0);
    tempTimeInfo.EndTime.Month := StrToIntDef('$' + Copy(infoData, 3, 2), 0);
    tempTimeInfo.EndTime.Date := StrToIntDef('$' + Copy(infoData, 5, 2), 0);
    tempTimeInfo.EndTime.Hour := StrToIntDef('$' + Copy(infoData, 7, 2), 12);
    tempTimeInfo.EndTime.Minute := StrToIntDef('$' + Copy(infoData, 9, 2), 0);
    tempTimeInfo.EndTime.Second := StrToIntDef('$' + Copy(infoData, 11, 2), 0);

    // Add new timer panel into list with captured values.
    AddTimer(tempTimeInfo, MaxChannel);

    // Load next alarm configuration from the device.
    inc(currentAlarmPos);
    if(currentAlarmPos < activeAlarmCount) then
    begin
      progMain.Position := currentAlarmPos;
      GetAlarmStartTime(currentAlarmPos);
    end
    else
    begin
      // End of the loading process. Reset value changed flag and UI button state based on available alarm configuration.
      progMain.Position := 0;
      SetChangeFlag(false);
      SetButtonState((alarmList.Count > 0));
    end;
  end
  else
  begin
    // Data content is invalid. Should receive string with 12 characters.
    MessageDlg(Application.Title, srDataCorrupted, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.OnSetAlarmCount(isTimeout: Boolean);
var
  alarmInfo: TAlarmInfo;
  startTime: TTimeInfo;
begin
  if(activeAlarmCount = 0) then
  begin
    // No alarms are configured in this session.
    btnSync.Enabled := true;
    exit;
  end
  else
  begin
    // Start to upload each alarm configuration into the device.
    progMain.Max := activeAlarmCount;
    progMain.Position := 0;
    currentAlarmPos := 0;

    // Update the first alarm start time.
    alarmInfo := TfmTimer(alarmList.Items[0]).GetAlarmInfo();
    startTime := alarmInfo.StartTime;
    SetAlarmStartTime(currentAlarmPos, alarmInfo.Channel, (startTime.Year - 2019), startTime.Month, startTime.Date, startTime.Hour, startTime.Minute, startTime.Second);
  end;
end;

procedure TfrmMain.OnSetSystemTime(isTimeout: Boolean);
begin

end;

procedure TfrmMain.OnSetAlarmStartTime(isTimeout: Boolean);
var
  alarmInfo: TAlarmInfo;
  endTime: TTimeInfo;
begin
  // Update current alarm end time.
  alarmInfo := TfmTimer(alarmList.Items[currentAlarmPos]).GetAlarmInfo();
  endTime := alarmInfo.EndTime;
  SetAlarmEndTime(currentAlarmPos, (endTime.Year - 2019), endTime.Month, endTime.Date, endTime.Hour, endTime.Minute, endTime.Second);
end;

procedure TfrmMain.OnSetAlarmEndTime(isTimeout: Boolean);
var
  alarmInfo: TAlarmInfo;
  startTime: TTimeInfo;
begin
  // Check and move to next available alarm configuration.
  Inc(currentAlarmPos);
  progMain.Position := currentAlarmPos;

  if(currentAlarmPos < activeAlarmCount) then
  begin
    // Upload next alarm configuration into the device.
    alarmInfo := TfmTimer(alarmList.Items[currentAlarmPos]).GetAlarmInfo();
    startTime := alarmInfo.StartTime;
    SetAlarmStartTime(currentAlarmPos, alarmInfo.Channel, (startTime.Year - 2019), startTime.Month, startTime.Date, startTime.Hour, startTime.Minute, startTime.Second);
  end
  else
  begin
    // End of alarm configurations.
    progMain.Position := 0;
    btnSync.Enabled := true;
  end;
end;

procedure TfrmMain.SaveDevNameToMRU(devName: string);
var
  mruList: TStringList;
begin
  try
    mruList := TStringList.Create;
    mruList.CaseSensitive := false;

    if(FileExists(MRU_CONFIG_FILE_NAME)) then
    begin
      // MRU file exists!, load existing MRU list.
      mruList.LoadFromFile(MRU_CONFIG_FILE_NAME);
    end;

    if(mruList.IndexOf(devName) < 0) then
    begin
      // Specified device name is not available in the list.
      mruList.Insert(0, Trim(devName));

      // Save updated file into the disk and update the selection dropdown.
      mruList.SaveToFile(MRU_CONFIG_FILE_NAME);

      cmbPortName.Items.Clear;
      cmbPortName.Items.AddStrings(mruList);
    end;
  finally
    // If error occured, ignore it, because this is going to be a background operation.
  end;
end;

procedure TfrmMain.LoadMRUList();
begin
  try
    if(FileExists(MRU_CONFIG_FILE_NAME)) then
    begin
      cmbPortName.Items.LoadFromFile(MRU_CONFIG_FILE_NAME);

      if(cmbPortName.Items.Count > 0) then
      begin
        // Select most recently used device name from the list.
        cmbPortName.ItemIndex := 0;
      end;
    end;
  finally
  end;
end;

end.


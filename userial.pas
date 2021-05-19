//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// Definitions for serial communication protocol.
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

unit userial;

{$mode objfpc}{$H+}

interface

type
  TDeviceCommand = (CmdNone, CmdHandshake, CmdDevInfo,
                    CmdGetAlarmCount, CmdGetSysTime, CmdGetStartAlarmTime, CmdGetEndAlarmTime,
                    CmdSetAlarmCount, CmdSetSysTime, CmdSetStartAlarmTime, CmdSetEndAlarmTime);

const
  // Timeout limits for commands.
  TIME_HANDHSAKE : Word = 5;
  TIME_DEVICE_INFO : Word = 5;

  TIME_GET_ALARM_COUNT : Word = 10;
  TIME_GET_SYS_TIME : Word = 10;
  TIME_GET_START_ALARM_TIME : Word = 15;
  TIME_GET_END_ALARM_TIME : Word = 15;

  TIME_SET_ALARM_COUNT : Word = 10;
  TIME_SET_SYS_TIME : Word = 15;
  TIME_SET_START_ALARM_TIME : Word = 15;
  TIME_SET_END_ALARM_TIME : Word = 15;

  // Response command length.
  RESP_LEN_HEADER : Word = 3;

  RESP_LEN_HANDSHAKE : Word = 3;
  RESP_LEN_DEVICE_INFO : Word = 7;

  RESP_LEN_GET_ALARM_COUNT : Word = 5;
  RESP_LEN_GET_SYS_TIME : Word = 15;
  RESP_LEN_GET_START_ALARM_TIME : Word = 17;
  RESP_LEN_GET_END_ALARM_TIME : Word = 15;

  RESP_LEN_SET_ALARM_COUNT : Word = 3;
  RESP_LEN_SET_SYS_TIME : Word = 3;
  RESP_LEN_SET_START_ALARM_TIME : Word = 3;
  RESP_LEN_SET_END_ALARM_TIME : Word = 3;

  // Request static data.
  DEV_CMD_HANDSHAKE : string = '#TH';
  DEV_CMD_DEVICE_INFO : string = '#TI';

  DEV_CMD_GET_ALARM_COUNT : string = '#TX';
  DEV_CMD_GET_SYS_TIME : string = '#TG';
  DEV_CMD_GET_START_ALARM_TIME : string = '#TB';
  DEV_CMD_GET_END_ALARM_TIME : string = '#TW';

  DEV_CMD_SET_ALARM_COUNT : string = '#TN';
  DEV_CMD_SET_SYS_TIME : string = '#TC';
  DEV_CMD_SET_START_ALARM_TIME : string = '#TS';
  DEV_CMD_SET_END_ALARM_TIME : string = '#TE';

  // Response static data.
  DEV_RESP_HANDSHAKE : string = '*TH';
  DEV_RESP_INFO : string = '*TI';

  DEV_RESP_GET_ALARM_COUNT : string = '*TX';
  DEV_RESP_GET_SYS_TIME : string = '*TG';
  DEV_RESP_GET_START_ALARM_TIME : string = '*TB';
  DEV_RESP_GET_END_ALARM_TIME : string = '*TW';

  DEV_RESP_SET_ALARM_COUNT : string = '*TN';
  DEV_RESP_SET_SYS_TIME : string = '*TC';
  DEV_RESP_SET_START_ALARM_TIME : string = '*TS';
  DEV_RESP_SET_END_ALARM_TIME : string = '*TE';

implementation

end.


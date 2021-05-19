//-----------------------------------------------------------------------------
// USB / Serial, Multi Channel Timer Programmer Console.
// About window.
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

unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fileinfo, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    imgIcon: TImage;
    imgSriKit: TImage;
    lblAppName: TLabel;
    lblCopyright: TLabel;
    lblRepoID: TLabel;
    lblURL: TLabel;
    lblVersion: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
var
  versionInfo: TFileVersionInfo;
begin
  // Extract version information of the application executable.
  versionInfo := TFileVersionInfo.Create(nil);
  versionInfo.ReadFileInfo;
  lblAppName.Caption := versionInfo.VersionStrings.Values['FileDescription'];
  lblVersion.Caption := 'Version: ' + versionInfo.VersionStrings.Values['ProductVersion'] + '   (' + versionInfo.VersionStrings.Values['FileVersion'] + ')';
  lblCopyright.Caption := versionInfo.VersionStrings.Values['LegalCopyright'];
  lblURL.Caption := versionInfo.VersionStrings.Values['Comments'];
  FreeAndNil(versionInfo);
end;

procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: char);
begin
  if(key = #27) then
  begin
    close;
  end;
end;

procedure TfrmAbout.lblURLClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
end;

end.


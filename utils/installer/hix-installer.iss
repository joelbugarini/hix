[Setup]
AppName=Hix
AppVersion=0.1
DefaultDirName={autopf}\Hix
DefaultGroupName=Hix
OutputDir=output
OutputBaseFilename=hix-setup
Compression=lzma
SolidCompression=yes
LicenseFile=..\..\LICENSE
DisableProgramGroupPage=yes

[Files]
Source: "..\..\dist\hix.exe"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\Hix CLI"; Filename: "{app}\hix.exe"
Name: "{group}\Uninstall Hix"; Filename: "{uninstallexe}"

[Run]
Filename: "{app}\hix.exe"; Description: "Run Hix now"; Flags: postinstall skipifsilent

[Registry]
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "Path"; \
  ValueData: "{code:GetUpdatedPath}"; Flags: preservestringtype

[Code]
function GetUpdatedPath(Value: string): string;
var
  currentPath: string;
begin
  currentPath := ExpandConstant('{reg:HKCU\Environment,Path|}');
  if Pos(ExpandConstant('{app}'), currentPath) = 0 then
    Result := currentPath + ';' + ExpandConstant('{app}')
  else
    Result := currentPath;
end;

function InitializeSetup(): Boolean;
begin
  MsgBox('Welcome to the Hix installer wizard!', mbInformation, MB_OK);
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then begin
    MsgBox('✅ Hix CLI was installed successfully!' + #13#10 +
           'ℹ️ You may need to restart your terminal for "hix" to work everywhere.',
           mbInformation, MB_OK);
  end;
end;

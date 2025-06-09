#define MyAppName "Hix"
#define MyAppVersion "0.3.4.0"
#define MyAppPublisher "StackPatterns"
#define MyAppURL "https://github.com/joelbugarini/hix"
#define MyAppExeName "hix.exe"

[Setup]
AppId={{YOUR-APP-ID-HERE}}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
LicenseFile=..\..\LICENSE
OutputDir=output
OutputBaseFilename=hix-setup-{#MyAppVersion}
Compression=lzma
SolidCompression=yes
WizardStyle=modern
VersionInfoVersion={#MyAppVersion}
VersionInfoCompany={#MyAppPublisher}
VersionInfoDescription=Hix Code Generator
VersionInfoCopyright=Copyright (C) 2024
VersionInfoProductName={#MyAppName}
VersionInfoProductVersion={#MyAppVersion}
DisableProgramGroupPage=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\dist\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName} CLI"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName} CLI"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

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
  MsgBox('Welcome to the Hix installer wizard!' + #13#10 +
         'This will install Hix version {#MyAppVersion} on your computer.',
         mbInformation, MB_OK);
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then begin
    MsgBox('✅ Hix CLI version {#MyAppVersion} was installed successfully!' + #13#10 +
           'ℹ️ You may need to restart your terminal for "hix" to work everywhere.' + #13#10 +
           '📚 Documentation is available at {#MyAppURL}',
           mbInformation, MB_OK);
  end;
end;

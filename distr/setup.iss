; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Ru.Board EasyWrite"
#define MyAppVerName "Ru.Board EasyWrite 0.3.3 alpha"
#define MyAppPublisher "GalaxyWorks"
#define MyAppURL "http://www.gworks.ru"
#define MyAppExeName "rb_EasyWrite.exe"
#define MyAppReadMe "ReadMe.rtf"

[ISSI]
#define ISSI_URL
#define ISSI_URLText
#define ISSI_IncludePath "C:\Program Files\~Dev\Inno Setup\ISSI"
#include ISSI_IncludePath+"\_issi.isi"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\EasyWrite
DefaultGroupName={#MyAppName}
InfoBeforeFile=
OutputDir=..\
;SetupIconFile=E:\Projects\rb_EasyWrite\misc\resources\notepad_contrast.ico
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
SourceDir=files
VersionInfoCompany={#MyAppPublisher}
VersionInfoTextVersion={#MyAppVerName}
VersionInfoCopyright={#MyAppPublisher}
AppCopyright={#MyAppPublisher}
ShowLanguageDialog=yes
LanguageDetectionMethod=locale
WizardImageFile=E:\Projects\gw_dev\templates\distr\wiz_imgs\Modern\SetupModern21.bmp
WizardSmallImageFile=E:\Projects\gw_dev\templates\distr\wiz_imgs\Modern\Small\SetupModernSmall16.bmp
VersionInfoVersion=0.3.3

[Languages]
Name: english; MessagesFile: compiler:Default.isl
Name: russian; MessagesFile: compiler:Languages\Russian.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: *; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; Tasks: desktopicon
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}; Filename: {app}\{#MyAppExeName}; Tasks: quicklaunchicon

[Run]
Filename: {app}\{#MyAppExeName}; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: nowait postinstall skipifsilent unchecked
Filename: {app}\{#MyAppReadMe}; Description: {cm:ViewReadMe}; Flags: nowait postinstall skipifsilent shellexec

[UninstallDelete]
Name: {app}\*; Type: filesandordirs; Languages: 

[CustomMessages]
issiUrl={#MyAppURL}
issiUrlText={#MyAppPublisher}
english.ViewReadMe=View ReadMe file
russian.ViewReadMe=������� ReadMe ����

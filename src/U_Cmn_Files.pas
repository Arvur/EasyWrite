unit U_Cmn_Files;

interface

uses
 Windows{TStartupInfo},
 ImageHlp{TLoadedImage}, SysUtils{Win32Check, TSearchRec}, Classes{TStringList}, Forms{TApplication};

 function HomePath : string;
 function AuxName(aExt : string) : string;

 function PETimeStamp(const FileName: string): TDateTime; // [From PEStamp]
 procedure KillApplication(Restart: boolean);

 procedure GetAllFiles(aPathPlusMask : string; aSubFolders : Boolean; OlderThen : TDateTime; var aResultList : TStringList);
 procedure GetSubFolders(aPath : string; var aResultList : TStringList);
 function GetFirstFile(aPathPlusMask : string) : string;

implementation

uses
  Dialogs;

function HomePath : string;
begin
 Result := ExtractFilePath(Application.ExeName);
end;

function AuxName(aExt : string) : string;
var
 tmpExt : string;
begin
 if (aExt[1] = '.') then tmpExt := aExt else tmpExt := '.' + aExt;
 Result := ChangeFileExt(Application.ExeName, tmpExt);
end;

function PETimeStamp(const FileName: string): TDateTime; // [From PEStamp]
var
  LI: TLoadedImage;
begin
  Win32Check(MapAndLoad(PChar(FileName), nil, @LI, False, True));
  Result := LI.FileHeader.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
  UnMapAndLoad(@LI);
end;

procedure KillApplication(Restart: Boolean); // [From EurekaLog]
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  if Restart then
  begin
    GetStartupInfo(StartInfo);
    FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
    CreateProcess(nil, GetCommandLine, nil, nil, False,
      CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS, nil,
      PChar(GetCurrentDir), StartInfo, ProcInfo);
  end;
  TerminateProcess(GetCurrentProcess, 1);
end;

procedure GetAllFiles(aPathPlusMask : string; aSubFolders : Boolean; OlderThen : TDateTime; var aResultList : TStringList);
var
 Search : TSearchRec;
 hDir, hMask : string;
begin
 hDir := ExtractFilePath(aPathPlusMask);
 if (Length(hDir) > 0)
  then hDir := IncludeTrailingPathDelimiter(hDir)
  else begin   // No Path -- Get the current and tack on the mask
   hDir := HomePath;
   aPathPlusMask := hDir + aPathPlusMask;
  end;

 hMask := ExtractFileName(aPathPlusMask); // hMask := copy(aPathPlusMask,length(hDir)+1,(length(aPathPlusMask) - length(hDir)));
 if (hMask = '') then hMask := '*.*';

 if DirectoryExists(hDir) then begin
  if (FindFirst(hDir + hMask, faAnyFile, Search)= 0) then begin
   repeat
    if (FileDateToDateTime(Search.Time) >= OlderThen) then
     aResultList.Add(hDir + Search.Name);
   until (FindNext(Search) <> 0);
   FindClose(Search);
  end;

  if aSubFolders and (FindFirst(hDir + '*.*', faDirectory, Search) = 0) then begin
   repeat
    if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name[1] <> '.') then
     GetAllFiles(hDir + Search.Name + '\' + hMask, aSubFolders, OlderThen, aResultList);
   until (FindNext(Search) <> 0);
   FindClose(Search);
  end;
 end;
end;

procedure GetSubFolders(aPath : string; var aResultList : TStringList);
var
 Search : TSearchRec;
 hDir : string;
begin
 hDir := IncludeTrailingPathDelimiter(aPath);
 if DirectoryExists(hDir) and (FindFirst(hDir + '*.*', faDirectory, Search) = 0) then begin
  repeat
   if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name[1] <> '.') then
    aResultList.Add(Search.Name);
  until (FindNext(Search) <> 0);
  FindClose(Search);
 end;
end;

function GetFirstFile(aPathPlusMask : string) : string;
var
 tmpSR: TSearchRec;
begin
 if (FindFirst(aPathPlusMask, faAnyFile, tmpSR) = 0)
  then Result := ExtractFilePath(aPathPlusMask) + tmpSR.Name
  else Result := '';
 FindClose(tmpSR);
end;

end.


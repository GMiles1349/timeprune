unit unitmain;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  gemutil, gemclock, Unix, crt,
  Classes, SysUtils, StrUtils;

type
  TTimeEntry = record
  	public
    	Index: Integer;
    	SnapName: String;
    	Date: TGEMDateStruct;
    	TotalDays: Integer;
    	AgeDays: Integer;
  end;

	procedure Main();
  procedure GetParams();
  procedure GetMaxes();
  procedure GetData();
  procedure PopEntries();
  procedure PopDays(var E: TTimeEntry);
  procedure TrimExcess();
  procedure DeleteOld();
  procedure ErrorHalt(const aErrStr: String);

  function TimeToString(const aSeconds: Double): String;

var
  EXEPath: String;
  MaxPath: String;
  FileOutPath: String;
  FileBuff: String;
  Lines: Array of String;
  EntryLine: Array of String;
  Entry: Array of TTimeEntry;

  AutoDelete: Boolean = False;
  ListSnaps: Boolean = False;

  Clock: TGEMClock;
  StartTime,EndTime,DelTime,TotalTime,EstimatedTime: Double;

  CurYear: Word;
  CurMonth: Word;
  CurDay: Word;

  MaxDays: Integer;
  MaxKeep: Integer;

const
  DayCount: Array [1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);


implementation

procedure Main();
	begin

    Clock := TGEMClock.Create(100);

    gemWriteLn('----------------------------------------', gem_tred_bold);
   	gemWriteLn('TimePrune v1.0', gem_tred_bold);
    gemWriteLn();

    if gemRequestSudo('sudo priveleges needed to access timeshift snapshots...') <> 0 then begin
      ErrorHalt('could not obtain sudo priveleges!');
    end;

    EXEPath := ExtractFilePath(ParamStr(0));
    FileOutPath := EXEPath + 'timeprune.in';
    MaxPath := EXEPath + 'timeprune.conf';

    GetMaxes();
    GetParams();

    DecodeDate(Date, CurYear, CurMonth, CurDay);
  	GetData();

    gemWrite('Max Days to keep snapshots: ');
    if MaxDays = 0 then begin
    	gemWriteLn('FOREVER', gem_twhite_bold);
    end else begin
    	gemWriteLn(MaxDays.ToString(), gem_twhite_bold);
    end;

    gemWrite('Max snapshots to keep: ');  gemWriteLn(MaxKeep.ToString(), gem_twhite_bold);
    gemWrite('Current number of snapshots: '); gemWriteLn(Length(Entry).ToString(), gem_twhite_bold);
    gemWriteLn();

    TrimExcess();
    DeleteOld();

    gemWriteLn('All Done!', gem_twhite_bold);
  end;


procedure GetParams();
var
Par: Array of String;
Args: Array of String;
Num: Integer;
I: Integer;
OutFile: TextFile;
	begin

    for I := 1 to 10 do begin
      if ParamStr(I) <> '' then begin
        SetLength(Par, Length(Par) + 1);
        Par[High(Par)] := ParamStr(I);
      end;
    end;

    if Length(Par) = 0 then Exit();

    for I := 0 to High(Par) do begin
      // check that arguments start with --
    	if Par[I][1..2] <> '--' then begin
        ErrorHalt('"' + Par[I] + '" is not a valid argument!');
      end;

      Delete(Par[I], 1, 2);
      Args := SplitString(Par[I],'=');
      if Length(Args) = 1 then begin
        SetLength(Args, 2);
        Args[1] := '0';
      end;

      // check for auto-delete
      if Args[0] = 'auto-delete' then begin
        if Args[1] = 'true' then begin
          AutoDelete := true;
    			gemWriteLn('Set auto-delete');
        end;
      end;

      // check for auto-delete
      if Args[0] = 'list' then begin
        if Args[1] = 'true' then begin
          ListSnaps := true;
    			gemWriteLn('Set list');
        end;
      end;

      // check for max days
      if Args[0] = 'max-days' then begin
        try
          Num := Args[1].ToInteger();
          if Num <> MaxDays then begin
            MaxDays := Num;
            AssignFile(OutFile, MaxPath);
            ReWrite(OutFile);
            WriteLn(OutFile,'Max Days=' + Num.ToString());
            WriteLn(OutFile,'Max Keep=' + MaxKeep.ToString());
            CloseFile(OutFile);
            gemWriteLn('Set max-days ' + Num.ToString());
          end;
        except
        	ErrorHalt('max-days must be a positive integer value!');
        end;
      end;

      // check for max keep
      if Args[0] = 'max-keep' then begin
        try
          Num := Args[1].ToInteger();
          if Num <> MaxKeep then begin
            MaxKeep := Num;
            AssignFile(OutFile, MaxPath);
            ReWrite(OutFile);
            WriteLn(OutFile,'Max Days=' + MaxDays.ToString());
            WriteLn(OutFile,'Max Keep=' + MaxKeep.ToString());
            CloseFile(OutFile);
            gemWriteLn('Set max-keep ' + Num.ToString());
          end;
        except
        	ErrorHalt('max-keep must be a positive integer value!');
        end;
      end;


    end;


  end;

procedure GetMaxes();
var
OutFile: TextFile;
DL: Array of String;
Data: Array of String;

	procedure MakeMaxes();
    begin
    	AssignFile(OutFile, MaxPath);
      ReWrite(OutFile);
      WriteLn(OutFile, 'Max Days=30');
      WriteLn(OutFile, 'Max Keep=100');
      CloseFile(OutFile);

      MaxDays := 30;
      MaxKeep := 100;
    end;

	begin

    if gemFileExists(MaxPath) = False then begin
      WriteLn('!Could not find timeprune.conf! Creating with default values at ' + MaxPath);
    	MakeMaxes();
      Exit();
    end;

    try
      gemReadFile(MaxPath, FileBuff);
      DL := SplitString(FileBuff, sLineBreak);

      Data := SplitString(DL[0], '=');
      MaxDays := Data[1].ToInteger();

      Data := SplitString(DL[1], '=');
      MaxKeep := Data[1].ToInteger();

    except
      WriteLn('!Error parsing timeprune.conf! Overwriting with default values at ' + MaxPath);
      MakeMaxes();
    end;

  end;

procedure GetData();
var
I,R: Integer;
Start: Boolean;
	begin

		fpSystem('touch ' + FileOutPath);
    fpSystem('sudo timeshift --list > ' + FileOutPath);

    gemReadFile(FileOutPath, FileBuff);
    Lines := SplitString(Filebuff, sLineBreak);

    Start := False;
    R := 0;
    for I := 0 to High(Lines) do begin

      if Start = False then begin
        if Length(Lines[I]) > 0 then begin
          if Lines[I][1] = '-' then begin
            Start := True;
            Continue;
          end;
        end;
      end;

      if Start = True then begin
        if Length(Lines[I]) = 0 then Continue;
        SetLength(EntryLine, R + 1);
      	EntryLine[R] := Lines[I];
        Inc(R);
      end;

    end;

    SetLength(Entry, Length(EntryLine));
    PopEntries();

    if ListSnaps then begin
      R := WhereY();
      GotoXY(1, R);
      gemWriteLn('[LISTING SNAPSHOTS]');

      for I := 0 to High(Entry) do begin

        gemWrite(I.ToString + ': ' + Entry[I].Date.Year.ToString() + '-' +
        	StrPadLeft(Entry[I].Date.Month.ToString(), '0', 2) + '-' +
          StrPadLeft(Entry[I].Date.Day.ToString(), '0', 2));

        if Entry[I].TotalDays > MaxDays then begin
          gemWrite(' - OLD' + sLineBreak);
        end else begin
          gemWriteLn();
        end;

      end;

			gemWriteLn();
    end;

  end;

procedure PopEntries();
var
ELen: Integer;
Data: Array of String;
I,R: Integer;
P: Integer;
	begin
  	ELen := Length(Entry);

    for I := 0 to High(Entry) do begin
    	Entry[I].Index := I;

      SetLength(Data, 1);

      P := Pos('>', EntryLine[I]);
      for R := P + 1 to Length(EntryLine[I]) do begin
        if EntryLine[I,R] <> ' ' then begin
          P := R;
          break;
        end;
      end;

      Data[0] := EntryLine[I, P..High(EntryLine[I])];
      P := Pos(' ', Data[0]);
      Data[0] := Data[0][1..P-1];
      Entry[I].SnapName := Data[0];
      Data := SplitString(Data[0], '-');

      SetLength(Data, 3);

      for R := 0 to High(Data) do begin
        P := Pos('_', Data[R]);
        if P <> 0 then begin
          Data[R] := Data[R][1..P - 1];
        end;
      end;

      Entry[I].Date.Year := Data[0].ToInteger();
      Entry[I].Date.Month := Data[1].ToInteger();
      Entry[I].Date.Day := Data[2].ToInteger();
      PopDays(Entry[I]);
    end;

  end;

procedure PopDays(var E: TTimeEntry);
var
Year, Month, Day: Cardinal;
CurDate: TGEMDateStruct;
	begin
    CurDate.SetCurrentDate();
    E.TotalDays := CurDate.Difference(E.Date);
  end;


procedure TrimExcess();
var
InString: String;
Old: Integer;
Deleted: Integer;
X,Y: Integer;
Per: Double;
	begin

    gemWrite('Number of snapshots: ');

  	if (Length(Entry) <= MaxKeep) and (MaxKeep <> 0) then begin
      gemWriteLn('Good', gem_twhite_bold);
    	Exit;
    end else begin
      gemWriteLn('Excessive!', gem_tred_bold);
    end;

    if not AutoDelete then begin
      repeat
        gemWrite('Delete excessive snapshots [y/n]?: ', gem_twhite_bold); ReadLn(InString);
        if InString = '' then InString := ' ';
      until (InString[1] = 'y') or (InString[1] = 'n');

      if InString[1] = 'n' then begin
      	gemWriteLn();
      	Exit();
    	end;
    end;


    Deleted := 0;
    Old := Length(Entry) - MaxKeep;

    gemWriteLn();
    gemWriteLn('Deleting ' + Old.ToString() + ' old snapshots...', gem_tred_bold);
   	while Length(Entry) >  MaxKeep do begin
      StartTime := Clock.GetTime();

      X := WhereX();
      gemWrite('Number Deleted: '); gemWriteLn(Deleted.ToString(), gem_twhite_bold);
      gemWrite('Deleting snapshot '); gemWriteLn(Entry[0].SnapName, gem_twhite_bold);
      fpSystem('sudo timeshift --delete --snapshot ''' + Entry[0].SnapName + ''' >> /dev/null');
      Delete(Entry, 0, 1);
      if Length(Entry) > 100 then begin
        Y := WhereY();
				gotoXY(X,Y);
      end;
      Inc(Deleted);

      EndTime := Clock.GetTime();
      DelTime := EndTime - StartTime;
      TotalTime := TotalTime + DelTime;
      Per := Deleted / Old;

      EstimatedTime := TotalTime * (1/Per);

      gemWriteLn('Time to delete: ' + TimeToString(DelTime));
      gemWriteLn('Elapsed Time: ' + TimeToString(TotalTime));
      gemWriteLn('Estimated Completion Time: ' + TimeToString(EstimatedTime));
      gemWriteLn();



    end;

    gemWriteLn();
  end;


procedure DeleteOld();
var
I: Integer;
Old: Integer;
Deleted: Integer;
InString: String;
	begin

    if MaxDays = 0 then Exit();

    Old := 0;
    for I := 0 to High(Entry) do begin
      if Entry[I].TotalDays > MaxDays then begin
     		Inc(Old);
      end else begin
        Break;
    	end;
    end;

    if Old = 0 then begin
      gemWriteLn('No snapshots older than ' + MaxDays.ToString() + ' days!');
      gemWriteLn();
      Exit();
    end;

		gemWriteLn(Old.ToString() + ' snapshots older than ' + MaxDays.ToString());

    if not AutoDelete then begin
      repeat
        gemWrite('Delete old snapshots [y/n]?: ', gem_twhite_bold); ReadLn(InString);
      until (InString[1] = 'y') or (InString[1] = 'n');

      if InString[1] = 'n' then begin
      	gemWriteLn();
      	Exit();
    	end;
    end;

    Deleted := 0;

    I := 0;
    while I <= High(Entry) do begin
      if Entry[I].TotalDays >= MaxDays then begin
      	gemWriteLn('Deleting snapshot ' + Entry[I].SnapName + '... ' + Entry[I].TotalDays.ToString() + ' days old');
        fpSystem('sudo timeshift --delete --snapshot ' + Entry[I].SnapName + ' | /dev/null');
        Delete(Entry,I,1);
        Inc(Deleted);
        Continue;
      end else begin
      	Inc(I);
      end;
    end;

    if Deleted = 0 then begin
      gemWriteLn('No old snapshots to delete!', gem_twhite_bold);
    end else begin
      gemWriteLn('Deleted ' + Deleted.ToString() + ' snapshots', gem_twhite_bold);
    end;

    gemWriteLn();

  end;


procedure ErrorHalt(const aErrStr: String);
	begin
    gemWriteLn();
    gemWriteLn('ERROR: ', gem_tred_bold);
    gemWriteLn('  ' + aErrStr, gem_twhite_bold);
    Halt();
  end;

function TimeToString(const aSeconds: Double): String;
var
Secs,Mins,Hours: Integer;
	begin
    Result := '';

    Secs := trunc(aSeconds);
    Mins := 0;
    Hours := 0;
    while Secs >= 60 do begin
      Mins := Mins + 1;
      Secs := Secs - 60;
    end;

    while Mins >= 60 do begin
      Hours := Hours + 1;
      Mins := Mins - 60;
    end;

    if Hours <> 0 then begin
      Result := Result + Hours.ToString() + ':';
    end;

    if Mins < 10 then begin
      Result := Result + '0';
    end;
    Result := Result + Mins.ToString() + ':';

    if Secs < 10 then begin
      Result := Result + '0';
    end;
    Result := Result + Secs.ToString();


  end;

end.


unit unitmain;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  gemutil, gemclock, gemprogram, Unix, BaseUnix, crt, Process,
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
  Prog: TGEMProgram;
  ConfPath: String;
  FileBuff: String;
  OList: TStringList;
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

    Prog := TGEMProgram.Create(True);

    Clock := TGEMClock.Create(100);

    WriteLn(#27'[1;31m' + '----------------------------------------' + #27'[0m');
   	WriteLn(#27'[1;31m' + 'TimePrune v1.0' + #27'[0m');
    WriteLn();

    if fpGetEUID <> 0 then begin
      ErrorHalt('could not obtain sudo priveleges!');
    end;

    ConfPath := '/home/' + Prog.UserName + '/.config/timeprune/';

    GetMaxes();
    GetParams();

    DecodeDate(Date, CurYear, CurMonth, CurDay);
  	GetData();

    Write('Max Days to keep snapshots: ');
    if MaxDays = 0 then begin
    	WriteLn(#27'[1;97m' + 'FOREVER' + #27'[0m');
    end else begin
    	WriteLn(#27'[1;97m' + MaxDays.ToString());
    end;

    Write(#27'[1;97;m' + 'Max snapshots to keep: ' + #27'[0m');  WriteLn(MaxKeep.ToString());
    Write(#27'[1;97;m' + 'Current number of snapshots: ' + #27'[0m'); WriteLn(Length(Entry).ToString());
    WriteLn();

    TrimExcess();
    DeleteOld();

    WriteLn(#27'[1;97;m' + 'All Done!' + #27'[0m');
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
    			WriteLn('Set auto-delete');
        end;
      end;

      // check for auto-delete
      if Args[0] = 'list' then begin
        if Args[1] = 'true' then begin
          ListSnaps := true;
    			WriteLn('Set list');
        end;
      end;

      // check for max days
      if Args[0] = 'max-days' then begin
        try
          Num := Args[1].ToInteger();
          if Num <> MaxDays then begin
            MaxDays := Num;
            AssignFile(OutFile, ConfPath + 'timeprune.conf');
            ReWrite(OutFile);
            WriteLn(OutFile,'Max Days=' + Num.ToString());
            WriteLn(OutFile,'Max Keep=' + MaxKeep.ToString());
            CloseFile(OutFile);
            WriteLn('Set max-days ' + Num.ToString());
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
            AssignFile(OutFile, ConfPath + 'timeprune.conf');
            ReWrite(OutFile);
            WriteLn(OutFile,'Max Days=' + MaxDays.ToString());
            WriteLn(OutFile,'Max Keep=' + MaxKeep.ToString());
            CloseFile(OutFile);
            WriteLn('Set max-keep ' + Num.ToString());
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
      MkDir(RawByteString(ConfPath));
    	AssignFile(OutFile, ConfPath + 'timeprune.conf');
      ReWrite(OutFile);
      WriteLn(OutFile, 'Max Days=30');
      WriteLn(OutFile, 'Max Keep=30');
      CloseFile(OutFile);

      MaxDays := 30;
      MaxKeep := 30;
    end;

	begin

    if (gemFileExists(ConfPath) = False) or (gemFileExists(ConfPath + 'timeprune.conf') = False) then begin
      WriteLn('!Could not find timeprune.conf! Creating with default values at ' + ConfPath);
    	MakeMaxes();
      Exit();
    end;

    try
      gemReadFile(ConfPath + 'timeprune.conf', FileBuff);
      DL := SplitString(FileBuff, sLineBreak);

      Data := SplitString(DL[0], '=');
      MaxDays := Data[1].ToInteger();

      Data := SplitString(DL[1], '=');
      MaxKeep := Data[1].ToInteger();

    except
      WriteLn('!Error parsing timeprune.conf! Overwriting with default values at ' + ConfPath);
      MakeMaxes();
    end;

  end;

procedure GetData();
var
I,R: Integer;
Start: Boolean;
Proc: TProcess;
	begin

    OList := TStringList.Create();

    Proc := TProcess.Create(nil);
    Proc.Options := Proc.Options + [poUsePipes, poWaitOnExit];
    Proc.Executable := 'timeshift';
    Proc.Parameters.Add('--list');
    Proc.Execute();

    OList.LoadFromStream(Proc.Output);

    Proc.Free();

    Start := False;
    R := 0;
    for I := 0 to OList.Count - 1 do begin

      if Start = False then begin
        if Length(OList[I]) > 0 then begin
          if OList[I][1] = '-' then begin
            Start := True;
            Continue;
          end;
        end;
      end;

      if Start = True then begin
        if Length(OList[I]) = 0 then Continue;
        SetLength(EntryLine, R + 1);
      	EntryLine[R] := OList[I];
        Inc(R);
      end;

    end;

    SetLength(Entry, Length(EntryLine));
    PopEntries();

    if ListSnaps then begin
      R := WhereY();
      GotoXY(1, R);
      WriteLn('[LISTING SNAPSHOTS]');

      for I := 0 to High(Entry) do begin

        Write(I.ToString + ': ' + Entry[I].Date.Year.ToString() + '-' +
        	StrPadLeft(Entry[I].Date.Month.ToString(), '0', 2) + '-' +
          StrPadLeft(Entry[I].Date.Day.ToString(), '0', 2));

        if Entry[I].TotalDays > MaxDays then begin
          Write(' - OLD' + sLineBreak);
        end else begin
          WriteLn();
        end;

      end;

			WriteLn();
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

    Write('Number of snapshots: ');

  	if (Length(Entry) <= MaxKeep) and (MaxKeep <> 0) then begin
      WriteLn(#27'[1;97;m' + 'Good' + #27'[0m');
    	Exit;
    end else begin
      WriteLn(#27'[1;97;m' + 'Excessive!' + #27'[0m');
    end;

    if AutoDelete = False then begin
      repeat
        Write(#27'[1;97;m' + 'Delete excessive snapshots [y/n]?: ' + #27'[0m'); ReadLn(InString);
        if InString = '' then InString := ' ';
      until (InString[1] = 'y') or (InString[1] = 'n');

      if InString[1] = 'n' then begin
      	WriteLn();
      	Exit();
    	end;
    end;


    Deleted := 0;
    Old := Length(Entry) - MaxKeep;

    WriteLn();
    WriteLn(#27'[1;97;m' + 'Deleting ' + Old.ToString() + ' old snapshots...' + #27'[0m');
   	while Length(Entry) >  MaxKeep do begin
      StartTime := Clock.GetTime();

      X := WhereX();
      Write(#27'[1;91;m' + 'Number Deleted: ' + #27'[0m'); WriteLn(Deleted.ToString());
      Write(#27'[1;91;m' + 'Deleting snapshot ' + #27'[0m'); WriteLn(Entry[0].SnapName);
      fpSystem('sudo timeshift --delete --snapshot ''' + Entry[0].SnapName + ''' >> /dev/null' + #27'[0m');
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

      WriteLn('Time to delete: ' + TimeToString(DelTime));
      WriteLn('Elapsed Time: ' + TimeToString(TotalTime));
      WriteLn('Estimated Completion Time: ' + TimeToString(EstimatedTime));
      WriteLn();



    end;

    WriteLn();
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
      WriteLn('No snapshots older than ' + MaxDays.ToString() + ' days!');
      WriteLn();
      Exit();
    end;

		WriteLn(Old.ToString() + ' snapshots older than ' + MaxDays.ToString());

    if AutoDelete = False then begin
      repeat
        Write(#27'[1;97;m' + 'Delete old snapshots [y/n]?: ' + #27'[0m'); ReadLn(InString);
      until (InString[1] = 'y') or (InString[1] = 'n');

      if InString[1] = 'n' then begin
      	WriteLn();
      	Exit();
    	end;
    end;

    Deleted := 0;

    I := 0;
    while I <= High(Entry) do begin
      if Entry[I].TotalDays >= MaxDays then begin
      	WriteLn('Deleting snapshot ' + Entry[I].SnapName + '... ' + Entry[I].TotalDays.ToString() + ' days old');
        fpSystem('sudo timeshift --delete --snapshot ' + Entry[I].SnapName + ' | /dev/null');
        Delete(Entry,I,1);
        Inc(Deleted);
        Continue;
      end else begin
      	Inc(I);
      end;
    end;

    if Deleted = 0 then begin
      WriteLn(#27'[1;97;m' + 'No old snapshots to delete!' + #27'[0m');
    end else begin
      WriteLn(#27'[1;97;m' + 'Deleted ' + Deleted.ToString() + ' snapshots' + #27'[0m');
    end;

    WriteLn();

  end;


procedure ErrorHalt(const aErrStr: String);
	begin
    WriteLn();
    WriteLn(#27'[1;91;m' + 'ERROR: ' + #27'[0m');
    WriteLn(#27'[1;97;m' + '  ' + aErrStr + #27'[0m');
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


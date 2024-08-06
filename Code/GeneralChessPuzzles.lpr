program GeneralChessPuzzles;

{$mode ObjFPC}{$H+}

uses SysUtils, UBaseChess, USolitaire, USolo, UMelee, UBinaryChess;

const
  AllPuzzleType: Array[1..4] of String = ('Solitaire', 'Solo', 'Melee', 'Binary');
  FileName: String = 'Puzzles.txt';

function GetNumber(Question: String): Integer;
var
  s: String;
begin
  s := '';
  WriteLn(Question);
  ReadLn(s);
  while not TryStrToInt(s, Result) do
  begin
    WriteLn('Give correct input!');
    WriteLn(Question);
    ReadLn(s);
  end;
end;

procedure ShowOptions(var ShowSteps: Boolean);
var
  Question: String;
  Answer: Integer;
begin
  Question := 'Turn ';
  if ShowSteps then Question += 'off '
  else Question += 'on ';
  Question += 'showing generation steps [1]' + LineEnding;
  Question += 'Back to menu [0]';
  Answer := GetNumber(Question);
  if Answer = 1 then ShowSteps := not ShowSteps;
end;

//Solitaire: 1, Solo: 2, Melee: 3, Binary: 4
function GetPuzzleType(): Integer;
const
  Question: String = 'Which puzzle type?' + LineEnding +
                     'Solitaire [1]' + LineEnding +
                     'Solo [2]' + LineEnding +
                     'Melee [3]' + LineEnding +
                     'Binary [4]' + LineEnding +
                     'Options [5]' + LineEnding +
                     'Quit Program [0]';
type
  TPuzzleType = Set of Byte;
var
  PuzzleTypes: TPuzzleType;
  Answer: Integer;
begin
  PuzzleTypes := [0..High(AllPuzzleType)+1];
  Answer := GetNumber(Question);
  while not (Answer in PuzzleTypes) do
  begin
    WriteLn('Please choose an option!');
    Answer := GetNumber(Question);
  end;
  Result := Answer;
end;

function SolveOrGenerate(): Integer;
const
  Question: String = 'Which mode?' + LineEnding +
                     'Solve Puzzle [1]' + LineEnding +
                     'Generate Puzzle [2]' + LineEnding +
                     'Quit Program [0]';
type
  TModi = Set of Byte;
var
  Modi: TModi;
  Answer: Integer;
begin
  Modi := [0..2];
  Answer := GetNumber(Question);
  while not (Answer in Modi) do
  begin
    WriteLn('Please choose an option!');
    Answer := GetNumber(Question);
  end;
  Result :=  Answer;
end;

procedure SolvePuzzle(PuzzleType: Integer);
  function SolveSolitaire(Fen: String): String;
  var
    Solitaire: TSolitaire;
  begin
    Result := '';
    try
      Solitaire := TSolitaire.Create;
      Result := Solitaire.SolvePuzzle(Fen);
    finally
      Solitaire.Free;
    end;
  end;

  function SolveSolo(Fen: String): String;
  var
    Solo: TSolo;
  begin
    Result := '';
    try
      Solo := TSolo.Create;
      Result := Solo.SolvePuzzle(Fen);
    finally
      Solo.Free;
    end;
  end;

  function SolveMelee(Fen: String): String;
  var
    Melee: TMelee;
  begin
    Result := '';
    try
      Melee := TMelee.Create;
      Result := Melee.SolvePuzzle(Fen);
    finally
      Melee.Free;
    end;
  end;

  function SolveBinary(Fen: String): String;
  var
    Binary: TBinaryChess;
  begin
    Result := '';
    try
      Binary := TBinaryChess.Create;
      Result := Binary.SolvePuzzle(Fen);
    finally
      Binary.Free;
    end;
  end;

var
  Time: TDateTime;
  Fen, Solution: String;
begin
  Solution := '';
  WriteLn('Solving ', AllPuzzleType[PuzzleType], ' Puzzle chosen.', LineEnding);
  WriteLn('Give Fen:');
  ReadLn(Fen);
  Time := Now;
  case PuzzleType of
  1:
    Solution := SolveSolitaire(Fen);
  2:
    Solution := SolveSolo(Fen);
  3:
    Solution := SolveMelee(Fen);
  4:
    Solution := SolveBinary(Fen);
  end;
  Time := Now - Time;
  WriteLn('Solution:');
  WriteLn(Solution, LineEnding);
  WriteLn('Time needed: ', FormatDateTime('n:ss:zzz', time));
end;

procedure GeneratePuzzles(PuzzleType: Integer; ShowSteps: Boolean);
var
  Time: TDateTime;
  Solitaire: TSolitaire;
  Solo: TSolo;
  Melee: TMelee;
  Binary: TBinaryChess;
  Size, Amount, i: Integer;
  Fen, AllFens: String;
  SaveInFile: Boolean;
  s: String;
  MyFile: TextFile;
  FileHandle: Integer;
begin
  WriteLn('Generating ', AllPuzzleType[PuzzleType], ' Puzzles chosen.', LineEnding);
  Size := GetNumber('Give Puzzle Size:');
  Amount := GetNumber('How many puzzles generate?');
  SaveInFile := False;
  WriteLn('Save in file? [y/n]');
  ReadLn(s);
  if (s = 'y') or (s = 'Y') then
  begin
    if not FileExists(FileName) then
    begin
      FileHandle := FileCreate(FileName);
      if FileHandle = -1 then
      begin
        WriteLn('Error: Couldn''t create ', FileName, '.');
        Exit;
      end;
      FileClose(FileHandle);
    end;
    s := LineEnding + 'Puzzle type: ' + AllPuzzleType[PuzzleType] + LineEnding;
    s += 'Size: ' + IntToStr(Size) + LineEnding;
    SaveInFile := True;
    AssignFile(MyFile, FileName);
    try
      Append(MyFile);
      WriteLn(MyFile, s);
    finally
      CloseFile(MyFile)
    end;
  end;
  try
    Solitaire := TSolitaire.Create;
    Solo := TSolo.Create;
    Melee := TMelee.Create;
    Binary := TBinaryChess.Create;
    AllFens := '';
    WriteLn('Generating puzzles...');
    Time := Now;
    for i := 1 to Amount do
    begin
      case PuzzleType of
      1:
        Fen := Solitaire.GeneratePuzzle(Size, ShowSteps);
      2:
        Fen := Solo.GeneratePuzzle(Size, ShowSteps);
      3:
        Fen := Melee.GeneratePuzzle(Size, ShowSteps);
      4:
        Fen := Binary.GeneratePuzzle(Size, ShowSteps);
      end;
      WriteLn('Puzzle ', Fen, ' generated');
      AllFens += Fen + LineEnding;

      if SaveInFile then
      begin
        AssignFile(MyFile, FileName);
        try
          Append(MyFile);
          WriteLn(MyFile, Fen);
        finally
          CloseFile(MyFile)
        end;
    end;
    end;
    Time := Now - Time;
    WriteLn('All Puzzles:');
    WriteLn(AllFens, LineEnding);
    WriteLn('Time needed: ', FormatDateTime('h:nn:ss:zzz', time), LineEnding);
  finally
    Solitaire.Free;
    Solo.Free;
    Melee.Free;
    Binary.Free;
  end;
end;

var
  PuzzleType: Integer;
  Mode: Integer;
  ShowSteps: Boolean;
begin
  PuzzleType := -1;
  ShowSteps := False;
  while PuzzleType <> 0 do
  begin
    PuzzleType := GetPuzzleType();
    if PuzzleType = 0 then Exit
    else if PuzzleType = 5 then
    begin
      ShowOptions(ShowSteps);
      Continue;
    end;
    Mode := SolveOrGenerate();
    case Mode of
    0: Exit;
    1:
      SolvePuzzle(PuzzleType);
    2:
      GeneratePuzzles(PuzzleType, ShowSteps);
    end;
  end;
end.


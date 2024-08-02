unit UMelee;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UBaseChess, Math;

type
  TMelee = class(TBaseChess)
  private
    type
      TUnsolvablePositions = Array of Array[0..8] of QWord;
    const
      MinPuzzleSize:      Integer = 4;
      MaxPuzzleSize:      Integer = 19;
      MaxSizeSmallBoard:  Integer = 8;  // Puzzles with this size are on a 4x4 board
      MaxSizeMiddleBoard: Integer = 13; // 6x6

    function GetPieceMoves(Piece: QWord; WhiteTurn: Boolean; var Boards: TBoards): TAllMoves;
    function GetAllMoves(Boards: TBoards; WhiteTurn: Boolean): TAllMoves;
    function GetNewBoard(Move: TMove; WhiteTurn: Boolean; var Boards: TBoards): TBoards;
    // Puzzle solving
    function RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer;
                             var CurrentMoves: TAllMoves): Boolean; overload;
    // Uniqueness search
    procedure RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                              var AllUnsolvables: TUnsolvablePositions;
                              MoveCount: Integer;
                              MaxMoves: Integer;
                              var CurrentMoves: TAllMoves;
                              var AllSolutions: TStringArray); overload;
    // Only for checking if solution exists
    function RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer): Boolean; overload;
    procedure ReverseMove(var Boards: TBoards; NewPieceType: EBoards;
                          NewPieceWhite: Boolean; OldSqr, NewSqr: QWord);
    function AttacksPieceSquare(PieceSquare, Square: QWord;
                                WhitePiece: Boolean; var Boards: TBoards): Boolean;
    // Assumes white first turn
    function CheckUniqueness(Boards: TBoards): Boolean; overload;
    function CheckUniqueness(Boards: TBoards; WhiteTurn: Boolean): Boolean; overload;

    function PositionInUnsolvables(Boards: TBoards; WhiteTurn: Boolean;
                                   var Unsolvables: TUnsolvablePositions): Boolean;
    procedure AddToUnsolvables(Boards: TBoards; WhiteTurn: Boolean;
                               var Unsolvables: TUnsolvablePositions);
    function IsSolvable(Boards: TBoards; PiecesLeft: Integer; WhiteTurn: Boolean): Boolean;
  public
    constructor Create;
    function CheckUniqueness(fen: String): Boolean; overload; // Debugging purpose
    function SolvePuzzle(fen: String): String;
    function GeneratePuzzle(size: Integer; ShowSteps: Boolean): String;
  end;

implementation

constructor TMelee.Create;
begin
  Inherited;
  Randomize;
end;

function TMelee.GetPieceMoves(Piece: QWord; WhiteTurn: Boolean; var Boards: TBoards): TAllMoves;
const
  MaxMoveArray: Array[EBoards.EPawns..EBoards.EKings] of Byte = (2, 8, 4, 4, 8, 8);
var
  i, FoundMoves: Byte;
  MaxMoves:      Byte;
  Captures:      QWord;
  PieceType:     EBoards;
begin
  Result := Nil;
  PieceType := GetPieceType(Piece, Boards);
  MaxMoves := MaxMoveArray[PieceType];
  SetLength(Result, MaxMoves);
  FoundMoves := 0;
  case PieceType of
  EBoards.EPawns:
    Captures := AllPawnMoves(Piece, Boards[EBoards.EFull], WhiteTurn);
  EBoards.EKnights:
    Captures := AllKnightMoves(Piece, Boards[EBoards.EFull]);
  EBoards.EBishops:
    Captures := AllBishopMoves(Piece, Boards[EBoards.EFull]);
  EBoards.ERooks:
    Captures := AllRookMoves(Piece, Boards[EBoards.EFull]);
  EBoards.EQueens:
    Captures := AllQueenMoves(Piece, Boards[EBoards.EFull]);
  EBoards.EKings:
    Captures := AllKingMoves(Piece, Boards[EBoards.EFull]);
  else
    Exit;
  end;
  // Removes same colour captures
  if WhiteTurn then
    Captures := Captures and Boards[EBoards.EBlack]
  else
    Captures := Captures and not Boards[EBoards.EBlack];

  for i := startSquare to endSquare do
  begin
    if ((QWord(1) shl i) and Captures) <> 0 then
    begin
      Result[FoundMoves].startSqr := Piece;
      Result[FoundMoves].endSqr   := QWord(1) shl i;
      Result[FoundMoves].PieceType := Ord(PieceType);
      FoundMoves += 1;
      if FoundMoves = MaxMoves then
        Exit;
    end;
  end;
  SetLength(Result, FoundMoves);
end;

function TMelee.GetAllMoves(Boards: TBoards; WhiteTurn: Boolean): TAllMoves;
var
  i, j: Integer;
  Lowest: Integer;
  PieceBoard: QWord;
  Moves: TAllMoves;
begin
  Result := Nil;
  PieceBoard := Boards[EBoards.EFull];
  if WhiteTurn then
    PieceBoard := Boards[EBoards.EFull] and not Boards[EBoards.EBlack]
  else
    PieceBoard := Boards[EBoards.EFull] and Boards[EBoards.EBlack];

  for i := startSquare to endSquare do
  begin
    if ((QWord(1) shl i) and PieceBoard) <> 0 then
    begin
      Moves := GetPieceMoves(QWord(1) shl i, WhiteTurn, Boards);
      if Moves <> Nil then
      begin
        Lowest := Length(Result);
        SetLength(Result, Length(Result) + Length(Moves));
        for j := Lowest to High(Result) do
        begin
          Result[j] := Moves[j-Lowest];
        end;
      end;
    end;
  end;
end;

function TMelee.GetNewBoard(Move: TMove; WhiteTurn: Boolean; var Boards: TBoards): TBoards;
var
  CapturedPiece: EBoards;
begin
  Result := Boards;
  CapturedPiece := GetPieceType(Move.endSqr, Boards);
  Result[EBoards.EFull] :=
      Result[EBoards.EFull] and not Move.startSqr; //fullboard remove start
  Result[CapturedPiece] :=
      Result[CapturedPiece] and not Move.endSqr; //board with captured piecetype remove end
  Result[EBoards(Move.pieceType)] :=
      Result[EBoards(Move.pieceType)] and not Move.startSqr; //board with capture piecetype remove start
  Result[EBoards(Move.pieceType)] :=
      Result[EBoards(Move.pieceType)] or Move.endSqr; //board with capture piecetype add end
  if WhiteTurn then
  begin
    //black piece is captured, remove
    Result[EBoards.EBlack] := Result[EBoards.EBlack] and not Move.endSqr;
  end
  else
  begin
    //black piece captures, remove from start and add to end
    Result[EBoards.EBlack] := Result[EBoards.EBlack] and not Move.startSqr;
    Result[EBoards.EBlack] := Result[EBoards.EBlack] or Move.endSqr;
  end;
end;

{%region Solving Puzzle]}

function TMelee.RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                                var AllUnsolvables: TUnsolvablePositions;
                                MoveCount: Integer;
                                MaxMoves: Integer;
                                var CurrentMoves: TAllMoves): Boolean;
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  TranspositionDepth: Byte;
begin
  Result := False;
  if MoveCount = MaxMoves then
  begin
    Result := True;
    Exit;
  end;
  TranspositionDepth := MaxMoves div 2;
  AllMoves := GetAllMoves(Boards, WhiteTurn);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    NewBoard := GetNewBoard(Move, WhiteTurn, Boards);
    // If new boards are in transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, WhiteTurn, AllUnsolvables);
    end;
  end;
end;

function TMelee.SolvePuzzle(fen: String): String;
var
  MaxMoves: Integer;
  Move: TMove;
  AllMoves: TAllMoves;
  NewBoards: TBoards;
  Solution: TAllMoves;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := '';
  if not SetBoardFromFen(fen) then
  begin
    Result := 'Invalid FEN!';
    Exit;
  end;
  if FBoards[EBoards.EFull] = 0 then
  begin
    Result := 'No pieces on board!';
  end;
  WriteLn(LineEnding, 'Position:', LineEnding);
  PrintBoard(FBoards);
  MaxMoves := FPieceAmounts - 1;
  Solution := Nil;
  AllUnsolvables := Nil;
  SetLength(Solution, MaxMoves);
  AllMoves := GetAllMoves(FBoards, True);
  for Move in AllMoves do
  begin
    NewBoards := GetNewBoard(Move, True, FBoards);
    Solution[0] := Move;
    if RecursiveSearch(NewBoards, False, AllUnsolvables, 1, MaxMoves, Solution) then
      Break;
  end;
  AllUnsolvables := Nil;
  if Solution[High(Solution)].pieceType = 0 then
    Result := 'Solution not found'
  else
    Result := DecodeSolution(Solution);
end;

{%endregion}

{%region Generate Puzzle}

procedure TMelee.ReverseMove(var Boards: TBoards; NewPieceType: EBoards;
                             NewPieceWhite: Boolean; OldSqr, NewSqr: QWord);
var
  OldPieceType: EBoards;
begin
  OldPieceType := GetPieceType(OldSqr, Boards);
  Boards[EBoards.EFull] := Boards[EBoards.EFull] or NewSqr; //add to fullboard
  Boards[OldPieceType] := Boards[OldPieceType] and not OldSqr; //remove capturing piece from old square
  Boards[OldPieceType] := Boards[OldPieceType] or NewSqr; //add capturing piece to new square
  Boards[NewPieceType] := Boards[NewPieceType] or OldSqr; //add captured piece to old square

  if NewPieceWhite then
  begin
    // Black piece is capturing piece
    Boards[EBoards.EBlack] := Boards[EBoards.EBlack] and not OldSqr;
    Boards[EBoards.EBlack] := Boards[EBoards.EBlack] or NewSqr;
  end
  else
  begin
    // Black is captured piece
    Boards[EBoards.EBlack] := Boards[EBoards.EBlack] or OldSqr;
  end;
end;

procedure TMelee.RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                                 var AllUnsolvables: TUnsolvablePositions;
                                 MoveCount: Integer;
                                 MaxMoves: Integer;
                                 var CurrentMoves: TAllMoves;
                                 var AllSolutions: TStringArray);
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  Solution, temp: String;
  SolutionFoundBefore: Boolean;
  TranspositionDepth: Byte;
begin
  if MoveCount = MaxMoves then
  begin
    SolutionFoundBefore := False;
    Solution := DecodeMoves(CurrentMoves);
    for temp in AllSolutions do
    begin
      if temp = Solution then
      begin
        SolutionFoundBefore := True;
      end;
    end;
    if not SolutionFoundBefore then
    begin
      SetLength(AllSolutions, Length(AllSolutions)+1);
      AllSolutions[High(AllSolutions)] := Solution;
    end;
    Exit;
  end;
  TranspositionDepth := MaxMoves div 2;
  AllMoves := GetAllMoves(Boards, WhiteTurn);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    NewBoard := GetNewBoard(Move, WhiteTurn, Boards);
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end
      else if not IsSolvable(NewBoard, MaxMoves-MoveCount, not WhiteTurn) then
      begin
        AddToUnsolvables(NewBoard, WhiteTurn, AllUnsolvables);
        Continue;
      end;
    end;
    RecursiveSearch(NewBoard, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
end;

function TMelee.CheckUniqueness(Boards: TBoards): Boolean;
begin
  Result := CheckUniqueness(Boards, True);
end;

function TMelee.CheckUniqueness(fen: String): Boolean;
begin
  Result := False;
  if SetBoardFromFen(fen) then
  begin
    Result := CheckUniqueness(FBoards);
  end;
end;

function TMelee.CheckUniqueness(Boards: TBoards; WhiteTurn: Boolean): Boolean; overload;
var
  MaxMoves: Integer;
  Move: TMove;
  AllFirstMoves: TAllMoves;
  NewBoards: TBoards;
  AllMoves: TAllMoves;
  AllSolutions: TStringArray;
  PieceAmount, i: Integer;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := False;
  if Boards[EBoards.EFull] = 0 then
  begin
    Exit;
  end;
  PieceAmount := 0;
  for i := startSquare to endSquare do
  begin
    if (Boards[EBoards.EFull] and (QWord(1) shl i)) <> 0 then
    begin
      PieceAmount += 1;
    end;
  end;
  MaxMoves := PieceAmount - 1;
  AllMoves := Nil;
  SetLength(AllMoves, MaxMoves);
  AllSolutions := Nil;
  AllUnsolvables := Nil;
  AllFirstMoves := GetAllMoves(Boards, WhiteTurn);
  for Move in AllFirstMoves do
  begin
    NewBoards := GetNewBoard(Move, WhiteTurn, Boards);
    AllMoves[0] := Move;
    RecursiveSearch(NewBoards, not WhiteTurn, AllUnsolvables, 1, MaxMoves, AllMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
  AllUnsolvables := Nil;
  if Length(AllSolutions) = 1 then
    Result := True;
end;

function TMelee.AttacksPieceSquare(PieceSquare, Square: QWord;
                                   WhitePiece: Boolean; var Boards: TBoards): Boolean;
begin
  Result := False;
  case GetPieceType(PieceSquare, Boards) of
  EBoards.EPawns:
    // Moves are reversed, so White Pawns attack like black pawns
    Result := AttacksPawnSquare(PieceSquare, Square, not WhitePiece);
  EBoards.EKnights:
    Result := AttacksKnightSquare(PieceSquare, Square);
  EBoards.EBishops:
    Result := AttacksBishopSquare(PieceSquare, Square, Boards[EBoards.EFull]);
  EBoards.ERooks:
    Result := AttacksRookSquare(PieceSquare, Square, Boards[EBoards.EFull]);
  EBoards.EQueens:
    Result := AttacksQueenSquare(PieceSquare, Square, Boards[EBoards.EFull]);
  EBoards.EKings:
    Result := AttacksKingSquare(PieceSquare, Square);
  end;
end;

function TMelee.GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
const
  SmallSize:  Byte = 16;
  MiddleSize: Byte = 36;
  FullSize:   Byte = 64;
  SmallBoard:  QWord = QWord(66229406269440);
  MiddleBoard: QWord = QWord(35604928818740736);
  FullBoard:   Qword = Qword(18446744073709551615);
  Pawn: Integer = Ord(EBoards.EPawns);
  King: Integer = Ord(EBoards.EKings);
var
  ResetBoard: Boolean; // Needed if no possible pieces can be added to prevent infinite loop
  EmptySquares: Array of QWord;
  AllPieceSquares: Array of QWord;
  Square: QWord;
  PieceSquare: QWord;
  BitBoard: QWord;
  i, j: Integer;
  Boards: TBoards;
  RandomPieceType: EBoards;
  PieceAdded: Boolean; // Checks if in a circle a piece has been added
  AddWhitePiece: Boolean; // If added Piece is white
begin
  Result := '';
  if Size < MinPuzzleSize then
  begin
    Result := 'Size too small!';
    Exit;
  end;
  if Size > MaxPuzzleSize then
  begin
    Result := 'Size too big!';
    Exit;
  end;
  ResetBoard := True;
  // loop until unique puzzle
  while ResetBoard do
  begin
    // preparing and randomising empty squares
    ResetBoard := False;
    AddWhitePiece := (Size mod 2) = 0;
    EmptyBoards(Boards);
    AllPieceSquares := Nil;
    EmptySquares := Nil;
    if Size <= MaxSizeSmallBoard then
    begin
      SetLength(EmptySquares, SmallSize);
      BitBoard := SmallBoard;
    end
    else if Size <= MaxSizeMiddleBoard then
    begin
      SetLength(EmptySquares, MiddleSize);
      BitBoard := MiddleBoard;
    end
    else
    begin
      SetLength(EmptySquares, FullSize);
      BitBoard := FullBoard;
    end;
    j := 0;
    for i := startSquare to endSquare do
    begin
      if ((QWord(1) shl i) and BitBoard) <> 0 then
      begin
        EmptySquares[j] := QWord(1) shl i;
        j += 1;
      end;
    end;
    ShuffleSquares(EmptySquares);
    // Add Pieces until Size, break loop if no pieces could be added
    for i := 1 to Size do
    begin
      PieceAdded := False;
      RandomPieceType := EBoards(RandomRange(Pawn, King));
      if i = 1 then
      begin
        Square := EmptySquares[High(EmptySquares)];
        SetLength(EmptySquares, Length(EmptySquares)-1);
        Boards[EFull] := Square;
        if not AddWhitePiece then
        begin
          Boards[EBlack] := Square;
        end;
        Boards[RandomPieceType] := Square;
        SetLength(AllPieceSquares, 1);
        AllPieceSquares[0] := Square;
        PieceAdded := True;
      end
      else
      begin
        ShuffleSquares(AllPieceSquares);
        for Square in EmptySquares do
        begin
          for PieceSquare in AllPieceSquares do
          begin
            // at adding white piece only black pieces should attack new square
            if ((PieceSquare and Boards[EBoards.EBlack]) <> 0) = AddWhitePiece then
              if AttacksPieceSquare(PieceSquare, Square, not AddWhitePiece, Boards) then
              begin
                ReverseMove(Boards, RandomPieceType, AddWhitePiece, PieceSquare, Square);
                RemoveSquareInEmptySquares(Square, EmptySquares);
                SetLength(AllPieceSquares, Length(AllPieceSquares)+1);
                AllPieceSquares[High(AllPieceSquares)] := Square;
                PieceAdded := True;
                Break;
              end;
          end;
          //Break from second loop
          if PieceAdded then Break;
        end;
      end;
      if not PieceAdded then
      begin
        ResetBoard := True;
        Break;
      end
      // Catches not unique puzzles before full size is generated
      else if (i >= size div 2) and ((i mod 3) = 0) and (i <> size) then
        begin
          if not CheckUniqueness(Boards, not AddWhitePiece) then
          begin
            ResetBoard := True;
            Break;
          end;
        end;
      AddWhitePiece := not AddWhitePiece;
    end;
    // Melee generation is already fast
    if not ResetBoard then
    begin
      if ShowSteps then WriteLn('Checking uniqueness of ', MakeFen(Boards));
      if not CheckUniqueness(Boards) then
      begin
        ResetBoard := True;
        if ShowSteps then WriteLn('Puzzle wasn''t unique.');
      end;
    end;
  end;
  Result := MakeFen(Boards);
end;

function TMelee.IsSolvable(Boards: TBoards; PiecesLeft: Integer; WhiteTurn: Boolean): Boolean;
var
  MaxMoves: Integer;
  Move: TMove;
  AllMoves: TAllMoves;
  NewBoards: TBoards;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := False;
  MaxMoves := PiecesLeft - 1;
  AllUnsolvables := Nil;
  AllMoves := GetAllMoves(Boards, WhiteTurn);
  for Move in AllMoves do
  begin
    NewBoards := GetNewBoard(Move, WhiteTurn, Boards);
    if RecursiveSearch(NewBoards, not WhiteTurn, AllUnsolvables, 1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TMelee.RecursiveSearch(var Boards: TBoards; WhiteTurn: Boolean;
                                var AllUnsolvables: TUnsolvablePositions;
                                MoveCount: Integer;
                                MaxMoves: Integer): Boolean; overload;
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  TranspositionDepth: Byte;
begin
  Result := False;
  if MoveCount = MaxMoves then
  begin
    Result := True;
    Exit;
  end;
  TranspositionDepth := MaxMoves div 2;
  AllMoves := GetAllMoves(Boards, WhiteTurn);
  for Move in AllMoves do
  begin
    NewBoard := GetNewBoard(Move, WhiteTurn, Boards);
    // If new boards are in transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, WhiteTurn, AllUnsolvables);
    end;
  end;
end;

{%endregion}

{%region Transposition tables}

function TMelee.PositionInUnsolvables(Boards: TBoards; WhiteTurn: Boolean;
                                      var Unsolvables: TUnsolvablePositions): Boolean;
var
  i, WhiteValue: Integer;
begin
  Result := False;
  if WhiteTurn then WhiteValue := 1
  else WhiteValue := 0;
  for i := High(Unsolvables) downto 0 do // Current positions are more similar at the end
  begin
    if  (Boards[EBoards.EFull] = Unsolvables[i, 0])
    and (Boards[EBoards.EBlack] = Unsolvables[i, 1])
    and (Boards[EBoards.EPawns] = Unsolvables[i, 2])
    and (Boards[EBoards.EKnights] = Unsolvables[i, 3])
    and (Boards[EBoards.EBishops] = Unsolvables[i, 4])
    and (Boards[EBoards.ERooks] = Unsolvables[i, 5])
    and (Boards[EBoards.EQueens] = Unsolvables[i, 6])
    and (Boards[EBoards.EKings] = Unsolvables[i, 7])
    and (WhiteValue = Unsolvables[i, 8]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TMelee.AddToUnsolvables(Boards: TBoards; WhiteTurn: Boolean;
                                  var Unsolvables: TUnsolvablePositions);
var
  i: Byte;
  Last: Integer;
begin
  SetLength(Unsolvables, Length(Unsolvables)+1);
  Last := High(Unsolvables);
  for i := 0 to 7 do
  begin
    Unsolvables[Last, i] := Boards[EBoards(i)];
  end;
  if WhiteTurn then
    Unsolvables[Last, 8] := 1
  else
    Unsolvables[Last, 8] := 0;
end;

{%endregion}

end.


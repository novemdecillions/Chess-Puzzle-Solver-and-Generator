unit USolitaire;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UBaseChess, Math;

type
  TSolitaire = class(TBaseChess)
  private
    type
      TUnsolvablePositions = Array of Array[0..7] of QWord;
    const
      MinPuzzleSize:      Integer = 4;
      MaxPuzzleSize:      Integer = 17;
      MaxSizeSmallBoard:  Integer = 8;  // Puzzles with this size are on a 4x4 board
      MaxSizeMiddleBoard: Integer = 11; // 6x6
      // Max Puzzle Size at which puzzle generation stops using random boards
      // and switches to brute force
      MaxRandomPuzzleSize:  Integer = 12;
    function GetPieceMoves(Piece: QWord; var Boards: TBoards): TAllMoves;
    function GetAllMoves(Boards: TBoards): TAllMoves;
    function GetNewBoard(Move: TMove; var Boards: TBoards): TBoards;
    // Puzzle solving
    function RecursiveSearch(var Boards: TBoards;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer;
                             var CurrentMoves: TAllMoves): Boolean; overload;
    // Uniqueness search
    procedure RecursiveSearch(var Boards: TBoards;
                              var AllUnsolvables: TUnsolvablePositions;
                              MoveCount: Integer;
                              MaxMoves: Integer;
                              var CurrentMoves: TAllMoves;
                              var AllSolutions: TStringArray); overload;
    // Only for checking if solution exists
    function RecursiveSearch(var Boards: TBoards;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer): Boolean; overload;
    procedure ReverseMove(var Boards: TBoards; NewPieceType: EBoards;
                              OldSqr, NewSqr: QWord);
    function AttacksPieceSquare(PieceSquare, Square: QWord; var Boards: TBoards): Boolean;
    function CheckUniqueness(Boards: TBoards): Boolean; overload;
    function PositionInUnsolvables(Boards: TBoards;
                                   var Unsolvables: TUnsolvablePositions): Boolean;
    procedure AddToUnsolvables(Boards: TBoards;
                               var Unsolvables: TUnsolvablePositions);
    function AddPieces(Boards: TBoards; CurrentSize, PuzzleSize: Integer;
                       EmptySquares, PieceSquares: TArrayQWord;
                       WasKingPlaced, ShowSteps: Boolean; out Fen: String): Boolean;
    function IsSolvable(Boards: TBoards; PiecesLeft: Integer): Boolean;
  public
    constructor Create;
    function CheckUniqueness(fen: String): Boolean; overload; // Debugging purpose
    function SolvePuzzle(fen: String): String;
    function GeneratePuzzle(size: Integer; ShowSteps: Boolean): String;
  end;

implementation

constructor TSolitaire.Create;
begin
  Inherited;
  Randomize;
end;

function TSolitaire.GetPieceMoves(Piece: QWord; var Boards: TBoards): TAllMoves;
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
    Captures := AllPawnMoves(Piece, Boards[EBoards.EFull]);
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

function TSolitaire.GetAllMoves(Boards: TBoards): TAllMoves;
var
  i, j: Integer;
  Lowest: Integer;
  PieceBoard: QWord;
  Moves: TAllMoves;
begin
  Result := Nil;
  PieceBoard := Boards[EBoards.EFull];
  for i := startSquare to endSquare do
  begin
    if ((QWord(1) shl i) and PieceBoard) <> 0 then
    begin
      Moves := GetPieceMoves(QWord(1) shl i, Boards);
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

function TSolitaire.GetNewBoard(Move: TMove; var Boards: TBoards): TBoards;
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
end;

{%region Solving Puzzle]}

function TSolitaire.RecursiveSearch(var Boards: TBoards;
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
  AllMoves := GetAllMoves(Boards);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    NewBoard := GetNewBoard(Move, Boards);
    // If new boards are in transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, AllUnsolvables);
    end;
  end;

end;

function TSolitaire.SolvePuzzle(fen: String): String;
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
  AllMoves := GetAllMoves(FBoards);
  for Move in AllMoves do
  begin
    NewBoards := GetNewBoard(Move, FBoards);
    Solution[0] := Move;
    if RecursiveSearch(NewBoards, AllUnsolvables, 1, MaxMoves, Solution) then
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

procedure TSolitaire.ReverseMove(var Boards: TBoards; NewPieceType: EBoards; OldSqr, NewSqr: QWord);
var
  OldPieceType: EBoards;
begin
  OldPieceType := GetPieceType(OldSqr, Boards);
  Boards[EBoards.EFull] := Boards[EBoards.EFull] or NewSqr; //add to fullboard
  Boards[OldPieceType] := Boards[OldPieceType] and not OldSqr; //remove capturing piece from old square
  Boards[OldPieceType] := Boards[OldPieceType] or NewSqr; //add capturing piece to new square
  Boards[NewPieceType] := Boards[NewPieceType] or OldSqr; //add captured piece to old square
end;

procedure TSolitaire.RecursiveSearch(var Boards: TBoards;
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
  AllMoves := GetAllMoves(Boards);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    NewBoard := GetNewBoard(Move, Boards);
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, AllUnsolvables) then
      begin
        Continue;
      end
      else if not IsSolvable(NewBoard, MaxMoves-MoveCount) then
      begin
        AddToUnsolvables(NewBoard, AllUnsolvables);
        Continue;
      end;
    end;
    RecursiveSearch(NewBoard, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
end;

function TSolitaire.CheckUniqueness(Boards: TBoards): Boolean;
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
  AllFirstMoves := GetAllMoves(Boards);
  for Move in AllFirstMoves do
  begin
    NewBoards := GetNewBoard(Move, Boards);
    AllMoves[0] := Move;
    RecursiveSearch(NewBoards, AllUnsolvables, 1, MaxMoves, AllMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
  AllUnsolvables := Nil;
  if Length(AllSolutions) = 1 then
    Result := True;
end;

function TSolitaire.CheckUniqueness(fen: String): Boolean;
begin
  Result := False;
  if SetBoardFromFen(fen) then
  begin
    Result := CheckUniqueness(FBoards);
  end;
end;

function TSolitaire.AttacksPieceSquare(PieceSquare, Square: QWord; var Boards: TBoards): Boolean;
begin
  Result := False;
  case GetPieceType(PieceSquare, Boards) of
  EBoards.EPawns:
    Result := AttacksPawnSquare(PieceSquare, Square);
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

function TSolitaire.AddPieces(Boards: TBoards; CurrentSize, PuzzleSize: Integer;
                              EmptySquares, PieceSquares: TArrayQWord;
                              WasKingPlaced, ShowSteps: Boolean; out Fen: String): Boolean;
var
  Square, Piece: QWord;
  AllPieceTypes: TPieceTypeArray;
  NewEmptySquares: TArrayQWord;
  NewPieceSquares: TArrayQWord;
  NewBoards: TBoards;
  NewWasKingPlaced: Boolean;
  PieceType: EBoards;
begin
  Result := False;

  if CurrentSize = PuzzleSize then
  begin
    Result := True;
    Fen := MakeFen(Boards);
    Exit;
  end;

  AllPieceTypes := GetRandomPieceArray(not WasKingPlaced);

  ShuffleSquares(PieceSquares);

  for PieceType in AllPieceTypes do
    for Square in EmptySquares do
      for Piece in PieceSquares do
        if AttacksPieceSquare(Piece, Square, Boards) then
        begin
          NewBoards := Boards;
          ReverseMove(NewBoards, PieceType, Piece, Square);
          if CheckUniqueness(NewBoards) then
          begin
            if ShowSteps then
            begin
              if PuzzleSize <> CurrentSize+1 then
              begin
                WriteLn('Could add 1 piece. ', PuzzleSize-CurrentSize-1, ' piece(s) left.');
                WriteLn('New position: ', MakeFen(NewBoards));
              end;
            end;
            NewWasKingPlaced := WasKingPlaced;
            if PieceType = EBoards.EKings then
            begin
              NewWasKingPlaced := True;
            end;
            NewEmptySquares := EmptySquares;
            RemoveSquareInEmptySquares(Square, NewEmptySquares);
            NewPieceSquares := PieceSquares;
            SetLength(NewPieceSquares, Length(NewPieceSquares)+1);
            NewPieceSquares[High(NewPieceSquares)] := Square;
            if AddPieces(NewBoards, CurrentSize+1,
                         PuzzleSize, NewEmptySquares,
                         NewPieceSquares, NewWasKingPlaced, ShowSteps, Fen) then
            begin
              Result := True;
              Exit;
            end
            else if ShowSteps then
            begin
              WriteLn('Couldn''t add piece(s) to ', MakeFen(NewBoards));
              WriteLn('Back to position ', MakeFen(Boards));
            end;
          end;
        end;
end;

function TSolitaire.GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
const
  Pawn: Integer = Ord(EBoards.EPawns);
  King: Integer = Ord(EBoards.EKings);
var
  ResetBoard: Boolean; // Needed if no possible pieces can be added to prevent infinite loop
  EmptySquares: Array of QWord;
  AllPieceSquares: Array of QWord;
  Square: QWord;
  PieceSquare: QWord;
  BitBoard: QWord;
  i, j, MaxRandomSize: Integer;
  Boards: TBoards;
  RandomPieceType: EBoards;
  PieceAdded: Boolean; // Checks if in a circle a piece has been added
  WasKingPlaced: Boolean;
  Fen: String;
begin
  Result := '';
  Fen    := '';
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
  if Size <= MaxRandomPuzzleSize then
  begin
    MaxRandomSize := Size - 1;
  end
  else
  begin
    MaxRandomSize := MaxRandomPuzzleSize;
  end;
  ResetBoard := True;
  // loop until unique puzzle
  while ResetBoard do
  begin
    // preparing and randomising empty squares
    ResetBoard := False;
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
    WasKingPlaced := False;

    // Add Pieces until Size, break loop if no pieces could be added
    for i := 1 to MaxRandomSize do
    begin
      PieceAdded := False;
      if WasKingPlaced then
        RandomPieceType := EBoards(RandomRange(Pawn, King)) //Only one king per puzzle
      else
        RandomPieceType := EBoards(RandomRange(Pawn, King + 1));
      if i = 1 then
      begin
        Square := EmptySquares[High(EmptySquares)];
        SetLength(EmptySquares, Length(EmptySquares)-1);
        Boards[EFull] := Square;
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
            if AttacksPieceSquare(PieceSquare, Square, Boards) then
            begin
              ReverseMove(Boards, RandomPieceType, PieceSquare, Square);
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
      else if (i >= size div 2) and ((i mod 3) = 0) and (i < MaxRandomSize)  then
      begin
        if not CheckUniqueness(Boards) then
        begin
          ResetBoard := True;
          Break;
        end;
      end;
      if RandomPieceType = EBoards.EKings then
      begin
        WasKingPlaced := True;
      end;
    end;
    // Adding piece
    if not ResetBoard then
    begin
      if not CheckUniqueness(Boards) then
      begin
        ResetBoard := True;
      end
      else
      begin
        if ShowSteps then
          WriteLn('Adding ', IntToStr(Size-MaxRandomSize), ' piece(s) to ', MakeFen(Boards));
        if not AddPieces(Boards, MaxRandomSize, Size, EmptySquares,
                         AllPieceSquares, WasKingPlaced, ShowSteps, Fen) then
        begin
          ResetBoard := True;
          if ShowSteps then WriteLn('Couldn''t increase size.');
        end;
      end;
    end;
  end;
  Result := Fen;
end;

function TSolitaire.RecursiveSearch(var Boards: TBoards;
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
  AllMoves := GetAllMoves(Boards);
  for Move in AllMoves do
  begin
    NewBoard := GetNewBoard(Move, Boards);
    // If new boards are in transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, AllUnsolvables, MoveCount+1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
      begin
        AddToUnsolvables(NewBoard, AllUnsolvables);
      end;
  end;
end;

function TSolitaire.IsSolvable(Boards: TBoards; PiecesLeft: Integer): Boolean;
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
  AllMoves := GetAllMoves(Boards);
  for Move in AllMoves do
  begin
    NewBoards := GetNewBoard(Move, Boards);
    if RecursiveSearch(NewBoards, AllUnsolvables, 1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{%endregion}

{%region Transposition tables}

function TSolitaire.PositionInUnsolvables(Boards: TBoards;
                                          var Unsolvables: TUnsolvablePositions): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := High(Unsolvables) downto 0 do // Current positions are more similar at the end
  begin
    if  (Boards[EBoards.EFull] = Unsolvables[i, 0])
    and (Boards[EBoards.EPawns] = Unsolvables[i, 2])
    and (Boards[EBoards.EKnights] = Unsolvables[i, 3])
    and (Boards[EBoards.EBishops] = Unsolvables[i, 4])
    and (Boards[EBoards.ERooks] = Unsolvables[i, 5])
    and (Boards[EBoards.EQueens] = Unsolvables[i, 6])
    and (Boards[EBoards.EKings] = Unsolvables[i, 7]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSolitaire.AddToUnsolvables(Boards: TBoards;
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
end;

{%endregion}

end.


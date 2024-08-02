unit USolo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UBaseChess, Math;

type
  // Rules: one King, uncapturable, each piece moves max twice
  // Added: Array of Bitboards,
  // Bitboard[0] = Zero Moves left, Bitboard[1] = One Move left etc.
  TSolo = class(TBaseChess)
  private
    type
      TBoardsMovesLeft = Array[0..2] of QWord;
      TUnsolvablePositions = Array of Array[0..10] of QWord; //Transposition list of whole Boards
      // 0..7: EBoards, 8..10: BoardMoves
    const
      MinPuzzleSize:      Integer = 4;
      MaxPuzzleSize:      Integer = 16;
      MaxSizeSmallBoard:  Integer = 8; //Puzzles with this Size are on a 4x4 board
      MaxSizeMiddleBoard: Integer = 11; // 6x6
      // Max Puzzle Size at which puzzle generation stops using random boards
      // and switches to brute force
      MaxRandomPuzzleSize:  Integer = 11;

    // Removes King captures, doesn't check if Piece can move
    function GetPieceMoves(Piece: QWord; var Boards: TBoards): TAllMoves;
    // Checks here if piece can move
    function GetAllMoves(Boards: TBoards; BoardMoves: TBoardsMovesLeft): TAllMoves;
    procedure GetNewBoard(Move: TMove; var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                          out NewBoards: TBoards; out NewBoardsMoves: TBoardsMovesLeft);
    procedure ReverseMove(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                          NewPieceType: EBoards; OldSqr, NewSqr: QWord);
    // Solving puzzle
    function RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer;
                             var CurrentMoves: TAllMoves): Boolean; overload;
    // Checking uniqueness
    procedure RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                              var AllUnsolvables: TUnsolvablePositions;
                              MoveCount: Integer;
                              MaxMoves: Integer;
                              var CurrentMoves: TAllMoves;
                              var AllSolutions: TStringArray); overload;
    // Only for checking if solution exists
    function RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer): Boolean; overload;
    // Function helping to skip searching positions when king can't move anymore
    function HasKingZeroMoves(King, ZeroMovesBoard: QWord): Boolean;
    function CheckUniqueness(Boards: TBoards): Boolean; overload;
    // Also checks if piece can move
    function AttacksPieceSquare(PieceSquare, Square, ZeroBoard: QWord;
                                var Boards: TBoards): Boolean;
    function PositionInUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                                   var Unsolvables: TUnsolvablePositions): Boolean;
    procedure AddToUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                               var Unsolvables: TUnsolvablePositions);
    function IsSolvable(Boards: TBoards; BoardsMoves: TBoardsMovesLeft; PiecesLeft: Integer): Boolean;
    function AddPieces(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                       CurrentSize, PuzzleSize: Integer;
                       EmptySquares, PieceSquares: TArrayQWord;
                       ShowSteps: Boolean; out Fen: String): Boolean;
  public
    constructor Create;
    function SolvePuzzle(Fen: String): String;
    function CheckUniqueness(Fen: String): Boolean; overload; // Debugging purpose
    function GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
  end;

implementation

constructor TSolo.Create;
begin
  Inherited;
  Randomize;
end;

function TSolo.GetPieceMoves(Piece: QWord; var Boards: TBoards): TAllMoves;
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
  //Removes King captures
  Captures := Captures and not Boards[EBoards.EKings];

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

function TSolo.GetAllMoves(Boards: TBoards; BoardMoves: TBoardsMovesLeft): TAllMoves;
var
  i, j: Integer;
  Lowest: Integer;
  PieceBoard: QWord;
  Moves: TAllMoves;
begin
  Result := Nil;
  //Removes Pieces without moves left
  PieceBoard := Boards[EBoards.EFull] and not BoardMoves[0];
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

procedure TSolo.GetNewBoard(Move: TMove; var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                            out NewBoards: TBoards; out NewBoardsMoves: TBoardsMovesLeft);
var
  CapturedPiece: EBoards;
  MovesLeft: Byte;
  i: Byte;
begin
  NewBoards := Boards;
  CapturedPiece := GetPieceType(Move.endSqr, Boards);
  NewBoards[EBoards.EFull] :=
      NewBoards[EBoards.EFull] and not Move.startSqr; //fullboard remove start
  NewBoards[CapturedPiece] :=
      NewBoards[CapturedPiece] and not Move.endSqr; //board with captured piecetype remove end
  NewBoards[EBoards(Move.pieceType)] :=
      NewBoards[EBoards(Move.pieceType)] and not Move.startSqr; //board with capture piecetype remove start
  NewBoards[EBoards(Move.pieceType)] :=
      NewBoards[EBoards(Move.pieceType)] or Move.endSqr; //board with capture piecetype add end
  //Boards with moves
  NewBoardsMoves := BoardsMoves;
  if (BoardsMoves[2] and Move.startSqr) <> 0 then
  begin
    MovesLeft := 2;
  end
  else if (BoardsMoves[1] and Move.startSqr) <> 0 then
  begin
    MovesLeft := 1;
  end
  else
  begin
    WriteLn('Warning: A piece moved despite unable to!');
    MovesLeft := 2;
  end;
  for i := Low(NewBoardsMoves) to High(NewBoardsMoves) do
  begin
    NewBoardsMoves[i] := NewBoardsMoves[i] and not Move.startSqr; //Removes start square from every move board
    NewBoardsMoves[i] := NewBoardsMoves[i] and not Move.endSqr; // End square removed as it's unclear how many moves captured piece still had
  end;
  NewBoardsMoves[MovesLeft-1] := NewBoardsMoves[MovesLeft-1] or Move.endSqr; // Adds piece to one move lower bitboard
end;

function TSolo.HasKingZeroMoves(King, ZeroMovesBoard: QWord): Boolean;
begin
  Result := (King and ZeroMovesBoard) <> 0;
end;

{%region Puzzle Solving}

function TSolo.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                               var AllUnsolvables: TUnsolvablePositions;
                                   MoveCount: Integer;
                                   MaxMoves: Integer;
                               var CurrentMoves: TAllMoves): Boolean;
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  NewBoardMoves: TBoardsMovesLeft;
  TransPositionDepth: Byte;
begin
  Result := False;
  if MoveCount = MaxMoves then
  begin
    Result := True;
    Exit;
  end;
  AllMoves := GetAllMoves(Boards, BoardsMoves);
  TransPositionDepth := MaxMoves div 2;
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    GetNewBoard(Move, Boards, BoardsMoves, NewBoard, NewBoardMoves);
    if HasKingZeroMoves(NewBoard[EBoards.EKings], NewBoardMoves[0]) and
       (MoveCount+1 <> MaxMoves) then
    begin
      Continue;
    end;
    // If new Positions are in Transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, NewBoardMoves, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables);
    end;
  end;
end;

function TSolo.SolvePuzzle(Fen: String): String;
var
  MaxMoves: Integer;
  Move: TMove;
  AllMoves: TAllMoves;
  BoardsMoves: TBoardsMovesLeft;
  NewBoards: TBoards;
  NewBoardsMoves: TBoardsMovesLeft;
  Solution: TAllMoves;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := '';
  if not SetBoardFromFen(Fen) then
  begin
    Result := 'Invalid FEN!';
    Exit;
  end;
  if FBoards[EBoards.EFull] = 0 then
  begin
    Result := 'No pieces on board!';
  end;
  BoardsMoves[0] := QWord(0);
  BoardsMoves[1] := QWord(0);
  BoardsMoves[2] := FBoards[EBoards.EFull];
  WriteLn(LineEnding, 'Position:', LineEnding);
  PrintBoard(FBoards);
  MaxMoves := FPieceAmounts - 1;
  Solution := Nil;
  AllUnsolvables := Nil;
  SetLength(Solution, MaxMoves);
  AllMoves := GetAllMoves(FBoards, BoardsMoves);
  for Move in AllMoves do
  begin
    GetNewBoard(Move, FBoards, BoardsMoves, NewBoards, NewBoardsMoves);
    Solution[0] := Move;
    if RecursiveSearch(NewBoards, NewBoardsMoves, AllUnsolvables, 1, MaxMoves, Solution) then
      Break;
  end;
  AllUnsolvables := Nil;
  if Solution[High(Solution)].pieceType = 0 then
    Result := 'Solution not found'
  else
    Result := DecodeSolution(Solution);
end;

{%endregion}

{region generating puzzle}

function TSolo.AttacksPieceSquare(PieceSquare, Square, ZeroBoard: QWord;
                                  var Boards: TBoards): Boolean;
begin
  Result := False;
  if (Square and ZeroBoard) <> 0 then Exit;
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

procedure TSolo.ReverseMove(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                            NewPieceType: EBoards; OldSqr, NewSqr: QWord);
var
  OldPieceType: EBoards;
  MovesLeft: Byte;
begin
  OldPieceType := GetPieceType(OldSqr, Boards);
  Boards[EBoards.EFull] := Boards[EBoards.EFull] or NewSqr; //add to fullboard
  Boards[OldPieceType] := Boards[OldPieceType] and not OldSqr; //remove capturing piece from old square
  Boards[OldPieceType] := Boards[OldPieceType] or NewSqr; //add capturing piece to new square
  Boards[NewPieceType] := Boards[NewPieceType] or OldSqr; //add captured piece to old square
  // Move Bitboards
  if (BoardsMoves[2] and OldSqr) <> 0 then
  begin
    MovesLeft := 2;
  end
  else if (BoardsMoves[1] and OldSqr) <> 0 then
  begin
    MovesLeft := 1;
  end
  else
  begin
    WriteLn('Warning: A piece moved despite unable to!');
    MovesLeft := 2;
  end;
  BoardsMoves[MovesLeft] := BoardsMoves[MovesLeft] and not OldSqr; // removes capturing piece from oldSqr/MoveBoard
  BoardsMoves[2] := BoardsMoves[2] or OldSqr; // adds captured/new piece to Boards with two moves
  BoardsMoves[MovesLeft-1] := BoardsMoves[MovesLeft-1] or NewSqr; // adds capturing piece to new square/move less board
end;

procedure TSolo.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                                var AllUnsolvables: TUnsolvablePositions;
                                MoveCount: Integer;
                                MaxMoves: Integer;
                                var CurrentMoves: TAllMoves;
                                var AllSolutions: TStringArray);
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  NewBoardMoves: TBoardsMovesLeft;
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
  TransPositionDepth := MaxMoves div 2;
  AllMoves := GetAllMoves(Boards, BoardsMoves);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    GetNewBoard(Move, Boards, BoardsMoves, NewBoard, NewBoardMoves);
    if HasKingZeroMoves(NewBoard[EBoards.EKings], NewBoardMoves[0]) and
       (MoveCount+1 <> MaxMoves) then
    begin
      Continue;
    end;
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables) then
      begin
        Continue;
      end
      else if not IsSolvable(NewBoard, NewBoardMoves, MaxMoves-MoveCount) then
      begin
        AddToUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables);
        Continue;
      end;
    end;
    RecursiveSearch(NewBoard, NewBoardMoves, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
end;

function TSolo.CheckUniqueness(Fen: String): Boolean;
begin
  Result := False;
  if SetBoardFromFen(Fen) then
  begin
    Result := CheckUniqueness(FBoards);
  end;
end;

function TSolo.CheckUniqueness(Boards: TBoards): Boolean;
var
  MaxMoves: Integer;
  Move: TMove;
  AllFirstMoves: TAllMoves;
  BoardMoves: TBoardsMovesLeft;
  NewBoards: TBoards;
  NewBoardMoves: TBoardsMovesLeft;
  AllMoves: TAllMoves;
  AllSolutions: TStringArray;
  AllUnsolvables: TUnsolvablePositions;
  PieceAmount, i: Integer;
begin
  Result := False;
  if Boards[EBoards.EFull] = 0 then
  begin
    Exit;
  end;
  BoardMoves[0] := QWord(0);
  BoardMoves[1] := QWord(0);
  BoardMoves[2] := Boards[EBoards.EFull];
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
  AllFirstMoves := GetAllMoves(Boards, BoardMoves);
  for Move in AllFirstMoves do
  begin
    GetNewBoard(Move, Boards, BoardMoves, NewBoards, NewBoardMoves);
    AllMoves[0] := Move;
    RecursiveSearch(NewBoards, NewBoardMoves, AllUnsolvables, 1, MaxMoves, AllMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
  if Length(AllSolutions) = 1 then
    Result := True;
end;

function TSolo.GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
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
  i, j, MaxRandomSize: Integer;
  Boards: TBoards;
  BoardsMoves: TBoardsMovesLeft;
  RandomPieceType: EBoards;
  PieceAdded: Boolean; // Checks if in a circle a piece has been added
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
    BoardsMoves[0] := QWord(0);
    BoardsMoves[1] := QWord(0);
    BoardsMoves[2] := FullBoard;
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
    for i := 1 to MaxRandomSize do
    begin
      PieceAdded := False;
      if i = 1 then
      begin
        Square := EmptySquares[High(EmptySquares)];
        SetLength(EmptySquares, Length(EmptySquares)-1);
        Boards[EFull] := Square;
        Boards[EBoards.EKings] := Square;
        SetLength(AllPieceSquares, 1);
        AllPieceSquares[0] := Square;
        PieceAdded := True;
      end
      else
      begin
        RandomPieceType := EBoards(RandomRange(Pawn, King));
        ShuffleSquares(AllPieceSquares);
        for Square in EmptySquares do
        begin
          for PieceSquare in AllPieceSquares do
          begin
            if AttacksPieceSquare(PieceSquare, Square, BoardsMoves[0], Boards) then
            begin
              ReverseMove(Boards, BoardsMoves, RandomPieceType, PieceSquare, Square);
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
      else if (i >= Size div 2) and ((i mod 3) = 0) and (i <> MaxRandomSize) then
      begin
        if not CheckUniqueness(Boards) then
        begin
          ResetBoard := True;
          Break;
        end;
      end;
    end;

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
        BoardsMoves[0] := 0;
        BoardsMoves[1] := 0;
        BoardsMoves[2] := Boards[EBoards.EFull];
        if not AddPieces(Boards, BoardsMoves, MaxRandomSize, Size,
                         EmptySquares, AllPieceSquares, ShowSteps, Fen) then
        begin
          ResetBoard := True;
          if ShowSteps then WriteLn('Couldn''t increase size.');
        end;
      end;
    end;
  end;
  Result := Fen;
end;

function TSolo.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                               var AllUnsolvables: TUnsolvablePositions;
                               MoveCount: Integer;
                               MaxMoves: Integer): Boolean; overload;
var
  AllMoves: TAllMoves;
  Move: TMove;
  NewBoard: TBoards;
  NewBoardMoves: TBoardsMovesLeft;
  TransPositionDepth: Byte;
begin
  Result := False;
  if MoveCount = MaxMoves then
  begin
    Result := True;
    Exit;
  end;
  AllMoves := GetAllMoves(Boards, BoardsMoves);
  TransPositionDepth := MaxMoves div 2;
  for Move in AllMoves do
  begin
    GetNewBoard(Move, Boards, BoardsMoves, NewBoard, NewBoardMoves);
    if HasKingZeroMoves(NewBoard[EBoards.EKings], NewBoardMoves[0]) and
       (MoveCount+1 <> MaxMoves) then
    begin
      Continue;
    end;
    // If new Positions are in Transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, NewBoardMoves, AllUnsolvables, MoveCount+1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, NewBoardMoves, AllUnsolvables);
    end;
  end;
end;

function TSolo.IsSolvable(Boards: TBoards; BoardsMoves: TBoardsMovesLeft; PiecesLeft: Integer): Boolean;
var
  MaxMoves: Integer;
  Move: TMove;
  AllMoves: TAllMoves;
  NewBoards: TBoards;
  NewBoardsMoves: TBoardsMovesLeft;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := False;
  MaxMoves := PiecesLeft - 1;
  AllUnsolvables := Nil;
  AllMoves := GetAllMoves(Boards, BoardsMoves);
  for Move in AllMoves do
  begin
    GetNewBoard(Move, Boards, BoardsMoves, NewBoards, NewBoardsMoves);
    if RecursiveSearch(NewBoards, BoardsMoves, AllUnsolvables, 1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSolo.AddPieces(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                         CurrentSize, PuzzleSize: Integer;
                         EmptySquares, PieceSquares: TArrayQWord;
                         ShowSteps: Boolean; out Fen: String): Boolean;
var
  Square, Piece: QWord;
  AllPieceTypes: TPieceTypeArray;
  NewEmptySquares: TArrayQWord;
  NewPieceSquares: TArrayQWord;
  NewBoards: TBoards;
  NewBoardsMoves: TBoardsMovesLeft;
  PieceType: EBoards;
begin
  Result := False;

  if CurrentSize = PuzzleSize then
  begin
    Result := True;
    Fen := MakeFen(Boards);
    Exit;
  end;

  AllPieceTypes := GetRandomPieceArray(False);

  ShuffleSquares(PieceSquares);

  for PieceType in AllPieceTypes do
  begin
    if PieceType = EBoards.EKings then
    begin
      Continue;
    end;
    for Square in EmptySquares do
      for Piece in PieceSquares do
        if AttacksPieceSquare(Piece, Square, BoardsMoves[0], Boards) then
        begin
          NewBoards := Boards;
          NewBoardsMoves := BoardsMoves;
          ReverseMove(NewBoards, NewBoardsMoves, PieceType, Piece, Square);
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
            NewEmptySquares := EmptySquares;
            RemoveSquareInEmptySquares(Square, NewEmptySquares);
            NewPieceSquares := PieceSquares;
            SetLength(NewPieceSquares, Length(NewPieceSquares)+1);
            NewPieceSquares[High(NewPieceSquares)] := Square;
            if AddPieces(NewBoards, NewBoardsMoves, CurrentSize+1,
                         PuzzleSize, NewEmptySquares,
                         NewPieceSquares, ShowSteps, Fen) then
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
end;

{%endregion}

{%region Transposition tables}

function TSolo.PositionInUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
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
    and (Boards[EBoards.EKings] = Unsolvables[i, 7])
    and (BoardsMoves[0] = Unsolvables[i, 8])
    and (BoardsMoves[1] = Unsolvables[i, 9])
    and (BoardsMoves[2] = Unsolvables[i, 10]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSolo.AddToUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
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
  for i := 0 to 2 do
  begin
    Unsolvables[Last, i+8] := BoardsMoves[i];
  end;
end;

{%endregion}

end.


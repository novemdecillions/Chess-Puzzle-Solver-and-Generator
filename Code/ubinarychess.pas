unit UBinaryChess;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UBaseChess, Math;

type
  // Rules: Two (uncapturable) Kings, two moves per turn, two colours,
  // king have to stand next to each other at the end
  TBinaryChess = class(TBaseChess)
  private
    type
      TBoardsMovesLeft = Array[0..2] of QWord;
      TUnsolvablePositions = Array of Array[0..11] of QWord; //Transposition list of whole Boards
      // 0..7: EBoards, 8..10: BoardMoves, 11: Turn
    const
      MinPuzzleSize:      Integer = 5;
      MaxPuzzleSize:      Integer = 18;
      MaxSizeSmallBoard:  Integer = 9; //Puzzles with this Size are on a 4x4 board
      MaxSizeMiddleBoard: Integer = 14; // 6x6

    // Removes King captures, doesn't check if Piece can move
    function GetPieceMoves(Piece: QWord; WhiteTurn: Boolean; var Boards: TBoards): TAllMoves;
    // Checks here if piece can move
    function  GetAllMoves(Boards: TBoards; BoardMoves: TBoardsMovesLeft; WhiteTurn: Boolean): TAllMoves;
    procedure GetNewBoard(Move: TMove; var Boards: TBoards;
                          var BoardsMoves: TBoardsMovesLeft; WhiteTurn: Boolean;
                          out NewBoards: TBoards; out NewBoardsMoves: TBoardsMovesLeft);
    procedure ReverseMove(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                          NewPieceWhite: Boolean; NewPieceType: EBoards;
                          OldSqr, NewSqr: QWord);
    // Solving puzzle
    function RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                             WhiteTurn: Boolean;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer;
                             var CurrentMoves: TAllMoves): Boolean; overload;
    // Checking uniqueness
    procedure RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                              WhiteTurn: Boolean;
                              var AllUnsolvables: TUnsolvablePositions;
                              MoveCount: Integer;
                              MaxMoves: Integer;
                              var CurrentMoves: TAllMoves;
                              var AllSolutions: TStringArray); overload;
    // Only for checking if solution exists
    function RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                             WhiteTurn: Boolean;
                             var AllUnsolvables: TUnsolvablePositions;
                             MoveCount: Integer;
                             MaxMoves: Integer): Boolean; overload;
    function AreKingsTogether(Board: QWord): Boolean;
    function CheckUniqueness(Boards: TBoards): Boolean; overload;
    function CheckUniqueness(Boards: TBoards; WhiteTurn: Boolean): Boolean; overload;
    // Also checks if piece can move
    function AttacksPieceSquare(PieceSquare, Square, ZeroBoard: QWord;
                                WhitePiece: Boolean; var Boards: TBoards): Boolean;
    function PositionInUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                                   WhiteTurn: Boolean;
                                   var Unsolvables: TUnsolvablePositions): Boolean;
    procedure AddToUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                               WhiteTurn: Boolean;
                               var Unsolvables: TUnsolvablePositions);
    function IsSolvable(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                        PiecesLeft: Integer; WhiteTurn: Boolean): Boolean;
  public
    constructor Create;
    function SolvePuzzle(Fen: String): String;
    function CheckUniqueness(Fen: String): Boolean; overload; // Debugging purpose
    function GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
  end;

implementation

constructor TBinaryChess.Create;
begin
  Inherited;
  Randomize;
end;

function TBinaryChess.AreKingsTogether(Board: QWord): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := startSquare to endSquare do
  begin
    if ((QWord(1) shl i) and Board) <> 0 then
    begin
      if (AllKingMoves(QWord(1) shl i, Board) and Board) <> 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TBinaryChess.GetPieceMoves(Piece: QWord; WhiteTurn: Boolean; var Boards: TBoards): TAllMoves;
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
  //Removes King captures
  Captures := Captures and not Boards[EBoards.EKings];
  //Removes same colour captures
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

function TBinaryChess.GetAllMoves(Boards: TBoards; BoardMoves: TBoardsMovesLeft;
                                WhiteTurn: Boolean): TAllMoves;
var
  i, j: Integer;
  Lowest: Integer;
  PieceBoard: QWord;
  Moves: TAllMoves;
begin
  Result := Nil;

  PieceBoard := Boards[EBoards.EFull] and not BoardMoves[0];
  if WhiteTurn then
    PieceBoard := PieceBoard and not Boards[EBoards.EBlack]
  else
    PieceBoard := PieceBoard and Boards[EBoards.EBlack];

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

procedure TBinaryChess.GetNewBoard(Move: TMove; var Boards: TBoards;
                                 var BoardsMoves: TBoardsMovesLeft; WhiteTurn: Boolean;
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
  //Black piece board
  if WhiteTurn then
  begin
    //Black piece is captured, remove
    NewBoards[EBoards.EBlack] := NewBoards[EBoards.EBlack] and not Move.endSqr;
  end
  else
  begin
    //Black piece captures, remove from start and add to end
    NewBoards[EBoards.EBlack] := NewBoards[EBoards.EBlack] and not Move.startSqr;
    NewBoards[EBoards.EBlack] := NewBoards[EBoards.EBlack] or Move.endSqr;
  end;
end;

{%region Puzzle Solving}

function TBinaryChess.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                                    WhiteTurn: Boolean;
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
    if AreKingsTogether(Boards[EBoards.EFull]) then
      Result := True;
    Exit;
  end;
  AllMoves := GetAllMoves(Boards, BoardsMoves, WhiteTurn);
  TransPositionDepth := MaxMoves div 2;
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    GetNewBoard(Move, Boards, BoardsMoves, WhiteTurn, NewBoard, NewBoardMoves);
    // If new Positions are in Transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, NewBoardMoves, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables);
    end;
  end;
end;

function TBinaryChess.SolvePuzzle(Fen: String): String;
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
  MaxMoves := FPieceAmounts - 2;
  Solution := Nil;
  AllUnsolvables := Nil;
  SetLength(Solution, MaxMoves);
  AllMoves := GetAllMoves(FBoards, BoardsMoves, True);
  for Move in AllMoves do
  begin
    GetNewBoard(Move, FBoards, BoardsMoves, True, NewBoards, NewBoardsMoves);
    Solution[0] := Move;
    if RecursiveSearch(NewBoards, NewBoardsMoves, False, AllUnsolvables, 1, MaxMoves, Solution) then
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

function TBinaryChess.AttacksPieceSquare(PieceSquare, Square, ZeroBoard: QWord;
                                       WhitePiece: Boolean; var Boards: TBoards): Boolean;
begin
  Result := False;
  if (Square and ZeroBoard) <> 0 then Exit;
  case GetPieceType(PieceSquare, Boards) of
  EBoards.EPawns:
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

procedure TBinaryChess.ReverseMove(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                                 NewPieceWhite: Boolean; NewPieceType: EBoards;
                                 OldSqr, NewSqr: QWord);
var
  OldPieceType: EBoards;
  MovesLeft: Byte;
begin
  OldPieceType := GetPieceType(OldSqr, Boards);
  Boards[EBoards.EFull] := Boards[EBoards.EFull] or NewSqr; //add to fullboard
  Boards[OldPieceType] := Boards[OldPieceType] and not OldSqr; //remove capturing piece from old square
  Boards[OldPieceType] := Boards[OldPieceType] or NewSqr; //add capturing piece to new square
  Boards[NewPieceType] := Boards[NewPieceType] or OldSqr; //add captured piece to old square
  // Black Board
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

procedure TBinaryChess.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                                     WhiteTurn: Boolean;
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
    if AreKingsTogether(Boards[EBoards.EFull]) then
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
    end;
    Exit;
  end;
  TransPositionDepth := MaxMoves div 2;
  AllMoves := GetAllMoves(Boards, BoardsMoves, WhiteTurn);
  for Move in AllMoves do
  begin
    CurrentMoves[MoveCount] := Move;
    GetNewBoard(Move, Boards, BoardsMoves, WhiteTurn, NewBoard, NewBoardMoves);
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end
      else if not IsSolvable(NewBoard, NewBoardMoves, MaxMoves-MoveCount, not WhiteTurn) then
      begin
        AddToUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables);
        Continue;
      end;
    end;
    RecursiveSearch(NewBoard, NewBoardMoves, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves, CurrentMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
end;

function TBinaryChess.CheckUniqueness(Fen: String): Boolean;
begin
  Result := False;
  if SetBoardFromFen(Fen) then
  begin
    Result := CheckUniqueness(FBoards, True);
  end;
end;

function TBinaryChess.CheckUniqueness(Boards: TBoards): Boolean;
begin
  Result := CheckUniqueness(Boards, True);
end;

function TBinaryChess.CheckUniqueness(Boards: TBoards; WhiteTurn: Boolean): Boolean;
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
  MaxMoves := PieceAmount - 2;
  AllMoves := Nil;
  SetLength(AllMoves, MaxMoves);
  AllSolutions := Nil;
  AllUnsolvables := Nil;
  AllFirstMoves := GetAllMoves(Boards, BoardMoves, WhiteTurn);
  for Move in AllFirstMoves do
  begin
    GetNewBoard(Move, Boards, BoardMoves, WhiteTurn, NewBoards, NewBoardMoves);
    AllMoves[0] := Move;
    RecursiveSearch(NewBoards, NewBoardMoves, not WhiteTurn, AllUnsolvables, 1, MaxMoves, AllMoves, AllSolutions);
    if Length(AllSolutions) > 1 then
      Exit;
  end;
  if Length(AllSolutions) = 1 then
    Result := True;
end;

function TBinaryChess.GeneratePuzzle(Size: Integer; ShowSteps: Boolean): String;
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
  i, j: Integer;
  Boards: TBoards;
  BoardsMoves: TBoardsMovesLeft;
  RandomPieceType: EBoards;
  PieceAdded: Boolean; // Checks if in a circle a piece has been added
  AddWhitePiece: Boolean;
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
    for i := 1 to Size do
    begin
      PieceAdded := False;
      if i = 1 then
      begin
        Square := EmptySquares[High(EmptySquares)];
        SetLength(EmptySquares, Length(EmptySquares)-1);
        Boards[EFull] := Square;
        if not AddWhitePiece then
        begin
          Boards[EBlack] := Square;
        end;
        Boards[EBoards.EKings] := Square;
        SetLength(AllPieceSquares, 1);
        AllPieceSquares[0] := Square;
        PieceAdded := True;
      end
      else
      begin
        if i <> 2 then
          RandomPieceType := EBoards(RandomRange(Pawn, King))
        else
          RandomPieceType := EBoards.EKings;
        ShuffleSquares(AllPieceSquares);
        for Square in EmptySquares do
        begin
          for PieceSquare in AllPieceSquares do
          begin
            if ((PieceSquare and Boards[EBoards.EBlack]) <> 0) = AddWhitePiece then
              if AttacksPieceSquare(PieceSquare, Square, BoardsMoves[0], not AddWhitePiece, Boards) then
              begin
                ReverseMove(Boards, BoardsMoves, AddWhitePiece, RandomPieceType, PieceSquare, Square);
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

function TBinaryChess.RecursiveSearch(var Boards: TBoards; var BoardsMoves: TBoardsMovesLeft;
                                    WhiteTurn: Boolean;
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
    if AreKingsTogether(Boards[EBoards.EFull]) then
      Result := True;
    Exit;
  end;
  AllMoves := GetAllMoves(Boards, BoardsMoves, WhiteTurn);
  TransPositionDepth := MaxMoves div 2;
  for Move in AllMoves do
  begin
    GetNewBoard(Move, Boards, BoardsMoves, WhiteTurn, NewBoard, NewBoardMoves);
    // If new Positions are in Transposition table
    if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      if PositionInUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables) then
      begin
        Continue;
      end;
    end;
    if RecursiveSearch(NewBoard, NewBoardMoves, not WhiteTurn, AllUnsolvables, MoveCount+1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end
    else if (MoveCount <= TranspositionDepth) and (MoveCount > 2) then
    begin
      AddToUnsolvables(NewBoard, NewBoardMoves, WhiteTurn, AllUnsolvables);
    end;
  end;
end;

function TBinaryChess.IsSolvable(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                               PiecesLeft: Integer; WhiteTurn: Boolean): Boolean;
var
  MaxMoves: Integer;
  Move: TMove;
  AllMoves: TAllMoves;
  NewBoards: TBoards;
  NewBoardsMoves: TBoardsMovesLeft;
  AllUnsolvables: TUnsolvablePositions;
begin
  Result := False;
  MaxMoves := PiecesLeft - 2;
  AllUnsolvables := Nil;
  AllMoves := GetAllMoves(Boards, BoardsMoves, WhiteTurn);
  for Move in AllMoves do
  begin
    GetNewBoard(Move, Boards, BoardsMoves, WhiteTurn, NewBoards, NewBoardsMoves);
    if RecursiveSearch(NewBoards, BoardsMoves, not WhiteTurn, AllUnsolvables, 1, MaxMoves) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{%endregion}

{%region Transposition tables}

function TBinaryChess.PositionInUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                                          WhiteTurn: Boolean;
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
    and (BoardsMoves[0] = Unsolvables[i, 8])
    and (BoardsMoves[1] = Unsolvables[i, 9])
    and (BoardsMoves[2] = Unsolvables[i, 10])
    and (WhiteValue = Unsolvables[i, 11])then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TBinaryChess.AddToUnsolvables(Boards: TBoards; BoardsMoves: TBoardsMovesLeft;
                                           WhiteTurn: Boolean;
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
  if WhiteTurn then
    Unsolvables[Last, 11] := 1
  else
    Unsolvables[Last, 11] := 0;
end;

{%endregion}

end.


unit UBaseChess;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TBaseChess = class
  // Format: Bitboards as UInt64/QWord, h1 = max uint64, a8 = 1
  // functions: Set board from fen, return fen, including black pieces
  // give all possible capture moves
  // no colour distinction, white captures also white and black captures also black.
  // Has also solution decoding and generation methods used by every child
    protected
      type
        EBoards = (EFull,
                   EBlack,
                   EPawns,
                   EKnights,
                   EBishops,
                   ERooks,
                   EQueens,
                   EKings);

        TBoards = Array[EBoards] of QWord;
        TMove = record
          startSqr:  QWord;
          endSqr:    QWord;
          pieceType: Integer;
        end;
        TAllMoves = Array of TMove;
        TArrayQWord = Array of QWord;
        TPieceTypeArray = Array of EBoards;
      const
        startSquare: Byte = Byte(0); // square a8
        endSquare: Byte = Byte(63);  // square h1, for for-loops throughout whole Board
        AllPieces: Array of EBoards = (EBoards.EPawns,
                                       EBoards.EKnights,
                                       EBoards.EBishops,
                                       EBoards.ERooks,
                                       EBoards.EQueens,
                                       EBoards.EKings);
        Letters: Array[EBoards.EPawns..EBoards.EKings] of Char =
                                  ('P', 'N', 'B', 'R', 'Q', 'K');
        NotACol: QWord = QWord(18374403900871474942);
        NotHCol: QWord = QWord(9187201950435737471);
        Not8thRow: QWord = QWord(18446744073709551360); //not a8, b8...
        Not1stRow:  QWord = QWord(72057594037927935);
        // Puzzle generation
        SmallSize:  Byte = 16;
        MiddleSize: Byte = 36;
        FullSize:   Byte = 64;
        SmallBoard:  QWord = QWord(66229406269440);
        MiddleBoard: QWord = QWord(35604928818740736);
        FullBoard:   Qword = Qword(18446744073709551615);
      var
        FBoards:            TBoards;
        FPieceAmounts:      Integer;
      procedure PrintBoard(Board: TBoards);
      function  MakeFen(Board: TBoards): String;
      procedure EmptyBoards(var Boards: TBoards);
      function  GetPieceType(Square: QWord; var Boards: TBoards): EBoards;
      function  SetBoardFromFen(Fen: String): Boolean;

      // Move Generation:

      //Returns next compass direction square. returns zero if over board/not possible
      function NSquare(Square: QWord): QWord;
      function NESquare(Square: QWord): QWord;
      function NWSquare(Square: QWord): QWord;
      function ESquare(Square: QWord): QWord;
      function WSquare(Square: QWord): QWord;
      function SSquare(Square: QWord): QWord;
      function SESquare(Square: QWord): QWord;
      function SWSquare(Square: QWord): QWord;
      function SlideToEndSquare(Square: QWord;
                                CaptureBoard: QWord;
                                Direction: Byte): QWord;

      function AllPawnMoves(Pawn, Board: QWord; isWhite: Boolean): QWord; overload;
      function AllPawnMoves  (Pawn, Board: QWord):   QWord; overload; //Assumes white Pawn
      function AllKnightMoves(Knight, Board: QWord): QWord;
      function AllBishopMoves(Bishop, Board: QWord): QWord;
      function AllRookMoves  (Rook, Board: QWord):   QWord;
      function AllQueenMoves (Queen, Board: QWord):  QWord;
      function AllKingMoves  (King, Board: QWord):   QWord;

      //Puzzle generation, attack checks

      function AttacksPawnSquare(Pawn, Square: Qword;
                                 isWhite: Boolean): Boolean; overload;
      function AttacksPawnSquare  (Pawn, Square: Qword):   Boolean; overload;
      function AttacksKnightSquare(Knight, Square: Qword): Boolean;
      // Board needed for ending slide if piece inbetween
      function AttacksBishopSquare(Bishop, Square, Board: Qword): Boolean;
      function AttacksRookSquare  (Rook, Square, Board: Qword):   Boolean;
      function AttacksQueenSquare (Queen, Square, Board: Qword):  Boolean;
      function AttacksKingSquare  (King, Square: Qword):   Boolean;

      // Decoding for solving puzzles, no sorting
      function  DecodeSolution(Solution: TAllMoves): String;
      // Decoding for uniqueness check, sorts solution
      function  DecodeMoves(AllMoves: TAllMoves): String;
      // Used for Puzzle Generation
      procedure ShuffleSquares(var Squares: Array of QWord);
      procedure RemoveSquareInEmptySquares(Square: QWord; var EmptySquares: TArrayQWord);
      function GetRandomPieceArray(AddKing: Boolean): TPieceTypeArray;
  end;


implementation

{%region Setup Board}

procedure TBaseChess.EmptyBoards(var Boards: TBoards);
var
  i: EBoards;
begin
  for i := Low(Boards) to High(Boards) do
  begin
    Boards[i] := 0;
  end;
end;

procedure TBaseChess.PrintBoard(Board: TBoards);
var
  i:       Byte;
  Square:   QWord;
  Piece:   EBoards;
  s, temp: String;
begin
  s := '';
  for i := startSquare to endSquare do
  begin
    Square := (QWord(1) shl i);
    temp := '*';
    if (Board[EBoards.EFull] and Square) <> 0 then
    begin
      for Piece in AllPieces do
      begin
        if (Board[Piece] and Square) <> 0 then
        begin
          temp := Letters[Piece];
          if (Board[EBoards.EBlack] and Square) <> 0 then
          begin
            temp := LowerCase(temp);
          end;
          Break;
        end;
      end;
    end;
    s += temp;
    //i+1 as 0 mod 8 = 0
    if ((i+1) mod 8) = 0 then
    begin
      s += LineEnding;
    end;
  end;
  WriteLn(s);
end;

function TBaseChess.MakeFen(Board: TBoards): String;
var
  i:        Byte;
  Square:    QWord;
  Piece:    EBoards;
  s, temp:  String;
  Empty:    Integer;
begin
  Result := '';
  s := '';
  Empty := 0;
  for i := startSquare to endSquare do
  begin
    Square := (QWord(1) shl i);
    temp := '';
    if (Board[EBoards.EFull] and Square) <> 0 then
    begin
      if Empty <> 0 then
      begin
        s += IntToStr(Empty);
        Empty := 0;
      end;
      for Piece in AllPieces do
      begin
        if (Board[Piece] and Square) <> 0 then
        begin
          temp := Letters[Piece];
          if (Board[EBoards.EBlack] and Square) <> 0 then
          begin
            temp := LowerCase(temp);
          end;
          Break;
        end;
      end;
    end;
    if temp = '' then
    begin
      Empty += 1;
    end
    else
    begin
      s += temp;
    end;
    if ((i+1) mod 8) = 0 then
    begin
      if Empty <> 0 then
      begin
        s += IntToStr(Empty);
        Empty := 0;
      end;
      s += '/';
    end;
  end;
  SetLength(s, Length(s)-1);
  Result := s;
end;

function TBaseChess.SetBoardFromFen(Fen: String): Boolean;
const
  AllowedLetters: Set of Char = ['P', 'N', 'B', 'R', 'Q', 'K'];
  LowerCasePiece: Set of Char = ['p', 'n', 'b', 'r', 'q', 'k'];
var
  c:         Char;
  UpperChar: Char;
  temp:      Integer;
  shlVar:    Integer;
  CountedPieces:  Integer;
  Square:    QWord;
  Boards:    TBoards; //Temp Board before FieldBoard is set
  Pieces:    Array['B'..'R'] of EBoards;
  Piece:     EBoards;
begin
  Result := False;
  temp := 0;
  shlVar := 0;
  CountedPieces := 0;
  Pieces['P'] := EBoards.EPawns;
  Pieces['N'] := EBoards.EKnights;
  Pieces['B'] := EBoards.EBishops;
  Pieces['R'] := EBoards.ERooks;
  Pieces['Q'] := EBoards.EQueens;
  Pieces['K'] := EBoards.EKings;
  EmptyBoards(Boards);
  try
    for c in Fen do
    begin
      if TryStrToInt(c, temp) then
      begin
        if temp < 9 then
          shlVar += temp
        else
          exit; //Invalid Fen
      end
      else
      begin
        UpperChar := UpperCase(c)[1];
        if UpperChar in AllowedLetters then
        begin
          CountedPieces += 1;
          Square := QWord(1) shl shlVar;
          Boards[EBoards.EFull] += Square;
          if c in LowerCasePiece then
          begin
            Boards[EBoards.EBlack] += Square;
          end;
          Piece := Pieces[UpperChar];
          Boards[Piece] += Square;
          shlVar += 1;
        end
        else if c = '/' then
        else if c = ' ' then
        begin
          if shlVar = 64 then
          begin
            Break; //longer Fen, more information not needed
          end
          else
            Exit; //Invalid Fen
        end
        else
          Exit; //Invalid
      end;
    end;
    //Position has been set correctly
    if shlVar = 64 then
    begin
      FBoards := Boards;
      FPieceAmounts := CountedPieces;
      Result := True;
    end;
  finally
  end;
end;

{%endregion}

function TBaseChess.GetPieceType(Square: QWord; var Boards: TBoards): EBoards;
var
  Piece: EBoards;
begin
  Result := EBoards.EFull;
  for Piece in AllPieces do
  begin
    if (Boards[Piece] and Square) <> 0 then
    begin
      Result := Piece;
      Exit;
    end;
  end;
end;

{%region Move Generation}

function TBaseChess.NSquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not8thRow) <> 0 then
    Result := (Square shr 8);
end;

function TBaseChess.NESquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not8thRow and NotHCol) <> 0 then
    Result := (Square shr 7);
end;

function TBaseChess.NWSquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not8thRow and NotACol) <> 0 then
    Result := (Square shr 9);
end;

function TBaseChess.ESquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and NotHCol) <> 0 then
    Result := (Square shl 1);
end;

function TBaseChess.WSquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and NotACol) <> 0 then
    Result := (Square shr 1);
end;

function TBaseChess.SSquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not1stRow) <> 0 then
    Result := (Square shl 8);
end;

function TBaseChess.SESquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not1stRow and NotHCol) <> 0 then
    Result := (Square shl 9);
end;

function TBaseChess.SWSquare(Square: QWord): QWord;
begin
  Result := 0;
  if (Square and Not1stRow and NotACol) <> 0 then
    Result := (Square shl 7);
end;

function TBaseChess.AllPawnMoves(Pawn, Board: QWord): QWord; overload;
begin
  Result := (NESquare(Pawn) and Board) or
            (NWSquare(Pawn) and Board);
end;

function TBaseChess.AllPawnMoves(Pawn, Board: QWord; isWhite: Boolean): QWord;
begin
  if isWhite then
    Result := (NESquare(Pawn) and Board) or
              (NWSquare(Pawn) and Board)
  else
    Result := (SESquare(Pawn) and Board) or
              (SWSquare(Pawn) and Board);
end;

function TBaseChess.AllKnightMoves(Knight, Board: QWord): QWord;
begin
  Result := (NESquare(NSquare(Knight)) and Board) or
            (NWSquare(NSquare(Knight)) and Board) or
            (NESquare(ESquare(Knight)) and Board) or
            (SESquare(ESquare(Knight)) and Board) or
            (NWSquare(WSquare(Knight)) and Board) or
            (SWSquare(WSquare(Knight)) and Board) or
            (SESquare(SSquare(Knight)) and Board) or
            (SWSquare(SSquare(Knight)) and Board);
end;

function TBaseChess.SlideToEndSquare(Square: QWord;
                          CaptureBoard: QWord;
                          Direction: Byte): QWord;
  function GetNewSquare(Square: QWord; Direction: Byte): QWord;
  begin
    Result := 0;
    case direction of
    1:
      Result := NSquare(Square);
    2:
      Result := ESquare(Square);
    3:
      Result := WSquare(Square);
    4:
      Result := SSquare(Square);
    5:
      Result := NESquare(Square);
    6:
      Result := NWSquare(Square);
    7:
      Result := SESquare(Square);
    8:
      Result := SWSquare(Square);
    end;
  end;
begin
  Result := 0;
  Square := GetNewSquare(Square, Direction);
  while Square <> 0 do
  begin
    if (Square and CaptureBoard) <> 0 then
    begin
      Result := Square;
      Exit;
    end;
    Square := GetNewSquare(Square, Direction);
  end;
end;

function TBaseChess.AllBishopMoves(Bishop, Board: QWord): QWord;
var
  i: Byte;
begin
  Result := 0;
  for i := 5 to 8 do
  begin
    Result += SlideToEndSquare(Bishop, Board, i);
  end;
end;

function TBaseChess.AllRookMoves(Rook, Board: QWord): QWord;
var
  i: Byte;
begin
  Result := 0;
  for i := 1 to 4 do
  begin
    Result += SlideToEndSquare(Rook, Board, i);
  end;
end;

function TBaseChess.AllQueenMoves(Queen, Board: QWord): QWord;
var
  i: Byte;
begin
  Result := 0;
  for i := 1 to 8 do
  begin
    Result += SlideToEndSquare(Queen, Board, i);
  end;
end;

function TBaseChess.AllKingMoves(King, Board: QWord): QWord;
begin
  Result := (NSquare (King) and Board) or
            (NESquare(King) and Board) or
            (NWSquare(King) and Board) or
            (ESquare (King) and Board) or
            (WSquare (King) and Board) or
            (SSquare (King) and Board) or
            (SESquare(King) and Board) or
            (SWSquare(King) and Board);
end;

{%endregion}

{%region Piece attacks square}

function TBaseChess.AttacksPawnSquare(Pawn, Square: Qword; isWhite: Boolean): Boolean;
begin
  if isWhite then
    Result := ((NESquare(Pawn) and Square) or
               (NWSquare(Pawn) and Square)) <> 0
  else
    Result := ((SESquare(Pawn) and Square) or
               (SWSquare(Pawn) and Square)) <> 0;
end;

function TBaseChess.AttacksPawnSquare(Pawn, Square: Qword): Boolean;
begin
  Result := ((NESquare(Pawn) and Square) or
             (NWSquare(Pawn) and Square)) <> 0;
end;

function TBaseChess.AttacksKnightSquare(Knight, Square: Qword): Boolean;
begin
  Result := ((NESquare(NSquare(Knight)) and Square) or
             (NWSquare(NSquare(Knight)) and Square) or
             (NESquare(ESquare(Knight)) and Square) or
             (SESquare(ESquare(Knight)) and Square) or
             (NWSquare(WSquare(Knight)) and Square) or
             (SWSquare(WSquare(Knight)) and Square) or
             (SESquare(SSquare(Knight)) and Square) or
             (SWSquare(SSquare(Knight)) and Square)) <> 0;
end;

function TBaseChess.AttacksBishopSquare(Bishop, Square, Board: Qword): Boolean;
var
  i: Byte;
begin
  Result := False;
  Board := Board or Square;
  for i := 5 to 8 do
  begin
    Result := Result or ((SlideToEndSquare(Bishop, Board, i) and Square) <> 0);
  end;
end;

function TBaseChess.AttacksRookSquare(Rook, Square, Board: Qword): Boolean;
var
  i: Byte;
begin
  Result := False;
  Board := Board or Square;
  for i := 1 to 4 do
  begin
    Result := Result or ((SlideToEndSquare(Rook, Board, i) and Square) <> 0);
  end;
end;

function TBaseChess.AttacksQueenSquare(Queen, Square, Board: Qword): Boolean;
var
  i: Byte;
begin
  Result := False;
  Board := Board or Square;
  for i := 1 to 8 do
  begin
    Result := Result or ((SlideToEndSquare(Queen, Board, i) and Square) <> 0);
  end;
end;

function TBaseChess.AttacksKingSquare(King, Square: QWord): Boolean;
begin
  Result := ((NSquare (King) and Square) or
             (NESquare(King) and Square) or
             (NWSquare(King) and Square) or
             (ESquare (King) and Square) or
             (WSquare (King) and Square) or
             (SSquare (King) and Square) or
             (SESquare(King) and Square) or
             (SWSquare(King) and Square)) <> 0;
end;

{%endregion}

function TBaseChess.DecodeSolution(Solution: TAllMoves): String;
var
  Move: TMove;
  startSqr: Integer;
  endSQr: Integer;
begin
  Result := '';
  for Move in Solution do
  begin
    startSqr := round(Log2(Move.startSqr));
    endSqr   := round(Log2(Move.endSqr));
    Result += Chr(Ord('a') + (startSqr mod 8)) + IntToStr(8-(startSqr div 8));
    Result += Chr(Ord('a') + (endSqr mod 8)) + IntToStr(8-(endSqr div 8)) + ',';
  end;
  SetLength(Result, Length(Result)-1);
end;

function TBaseChess.DecodeMoves(AllMoves: TAllMoves): String;
var
  MoveList: TStringList;
  startSqr, endSqr: QWord;
  Move: TMove;
  temp: String;
begin
  Result := '';
  try
    MoveList := TStringList.Create;
    for Move in AllMoves do
    begin
      startSqr := round(Log2(Move.startSqr));
      endSqr   := round(Log2(Move.endSqr));
      temp := Chr(Ord('a') + (startSqr mod 8)) + IntToStr(8-(startSqr div 8));
      temp += Chr(Ord('a') + (endSqr mod 8)) + IntToStr(8-(endSqr div 8));
      MoveList.Append(temp);
    end;
    MoveList.Sort;
    Result := MoveList.CommaText;
  finally
    MoveList.Free;
  end;
end;

procedure TBaseChess.ShuffleSquares(var Squares: Array of QWord);
var
  i, j: Integer;
  temp: QWord;
begin
  for i := High(Squares) downto 1 do
  begin
    j := RandomRange(Low(Squares), i+1);
    temp := Squares[i];
    Squares[i] := Squares[j];
    Squares[j] := temp;
  end;
end;

procedure TBaseChess.RemoveSquareInEmptySquares(Square: QWord; var EmptySquares: TArrayQWord);
var
  i, SkipValue: Integer;
begin
  SkipValue := 0;
  for i := 0 to High(EmptySquares) do
  begin
    if (Square = EmptySquares[i]) then
    begin
      SkipValue := 1;
    end
    else
    begin
      EmptySquares[i-SkipValue] := EmptySquares[i];
    end;
  end;
  SetLength(EmptySquares, Length(EmptySquares)-1);
end;

function TBaseChess.GetRandomPieceArray(AddKing: Boolean): TPieceTypeArray;
var
  Lowest, Highest, i: Integer;
  TempArray: TArrayQWord;
begin
  Result := Nil;
  Lowest := Ord(EBoards.EPawns);
  if AddKing then Highest := Ord(EBoards.EKings)
  else Highest := Ord(EBoards.EQueens);
  TempArray := Nil;
  SetLength(TempArray, Highest-Lowest+1);
  SetLength(Result, Highest-Lowest+1);
  for i := 0 to Highest-Lowest do
    TempArray[i] := i+Lowest;

  ShuffleSquares(TempArray);
  for i := 0 to Highest-Lowest do
    Result[i] := EBoards(TempArray[i]);
end;

end.


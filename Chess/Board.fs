namespace SpellChess.Chess

module Hexuple =
    let toArray (a, b, c, d, e, f) = [|a; b; c; d; e; f|]
    let toList (a, b, c, d, e, f) = [a; b; c; d; e; f]

type Color = 
    | Nil = 0
    | White = 8
    | Black = 16

module Color = 
    let oppositeColor color = 
        match color with
        | Color.Nil -> Color.Nil
        | Color.White -> Color.Black
        | Color.Black -> Color.White
        | _ -> Color.Nil

type PieceType = 
| King = 0
| Queen = 1
| Rook = 2
| Bishop = 3
| Knight = 4
| Pawn = 5

type Piece = byte

module Piece = 
    let generate (color: Color) (piece: PieceType): Piece =
        byte color ||| byte piece

    let color (piece: Piece): Color =
        enum (int piece &&& 0b11000)

    let pieceType (piece: Piece): PieceType =
        enum (int piece &&& 7)

    let promote (piece: Piece) (target: PieceType) =
        generate (color piece) target

    let toString piece =
        match piece with
            | p when p = generate Color.White PieceType.King -> "♔ "
            | p when p = generate Color.Black PieceType.King -> "♚ "

            | p when p = generate Color.White PieceType.Queen -> "♕ "
            | p when p = generate Color.Black PieceType.Queen -> "♛ "

            | p when p = generate Color.White PieceType.Rook -> "♖ "
            | p when p = generate Color.Black PieceType.Rook -> "♜ "

            | p when p = generate Color.White PieceType.Bishop -> "♗ "
            | p when p = generate Color.Black PieceType.Bishop -> "♝ "

            | p when p = generate Color.White PieceType.Knight -> "♘ "
            | p when p = generate Color.Black PieceType.Knight -> "♞ "

            | p when p = generate Color.White PieceType.Pawn -> "♙ "
            | p when p = generate Color.Black PieceType.Pawn -> "♟ "

            | _ -> "  "

type BitBoard = uint64
module BitBoard =
    let getLSB (bitBoard: BitBoard) =
        bitBoard &&& ~~~bitBoard + 1UL // isolates the LSB
        |> System.Numerics.BitOperations.TrailingZeroCount

    let private sumOfANDandShiftedAND andNum shiftCount n  = 
        (n &&& andNum) + (n >>> shiftCount &&& andNum)

    let pieceCount = 
        sumOfANDandShiftedAND 0x5555555555555555uL 1
        >> sumOfANDandShiftedAND 0x3333333333333333uL 2
        >> sumOfANDandShiftedAND 0x0F0F0F0F0F0F0F0FuL 4
        >> sumOfANDandShiftedAND 0x00FF00FF00FF00FFuL 8
        >> sumOfANDandShiftedAND 0x0000FFFF0000FFFFuL 16
        >> sumOfANDandShiftedAND 0x00000000FFFFFFFFuL 32

type PieceBoards = BitBoard * BitBoard * BitBoard * BitBoard * BitBoard * BitBoard
module PieceBoards =
    let getBitBoard pieceType (pieces: PieceBoards) =
        let king, queen, rook, bishop, knight, pawn = pieces
        match pieceType with 
        | PieceType.King -> king
        | PieceType.Queen -> queen
        | PieceType.Rook -> rook
        | PieceType.Bishop -> bishop
        | PieceType.Knight -> knight
        | PieceType.Pawn -> pawn
        | _ -> 0uL

    let setBitBoard piece pieces bitBoard =
        let king, queen, rook, bishop, knight, pawn = pieces
        match piece with 
        | PieceType.King -> bitBoard, queen, rook, bishop, knight, pawn
        | PieceType.Queen -> king, bitBoard, rook, bishop, knight, pawn
        | PieceType.Rook -> king, queen, bitBoard, bishop, knight, pawn
        | PieceType.Bishop -> king, queen, rook, bitBoard, knight, pawn
        | PieceType.Knight -> king, queen, rook, bishop, bitBoard, pawn
        | PieceType.Pawn -> king, queen, rook, bishop, knight, bitBoard
        | _ -> pieces

    let getPieceAtLocation (pieces: PieceBoards) (location: int) =
        let king, queen, rook, bishop, knight, pawn = pieces
        match 1uL <<< location with
        | x when king &&& x <> 0uL -> Some PieceType.King
        | x when queen &&& x <> 0uL -> Some PieceType.Queen
        | x when rook &&& x <> 0uL -> Some PieceType.Rook
        | x when bishop &&& x <> 0uL -> Some PieceType.Bishop
        | x when knight &&& x <> 0uL -> Some PieceType.Knight
        | x when pawn &&& x <> 0uL -> Some PieceType.Pawn
        | x -> None

type Player = {
    KingLocation: int
    Castling: bool * bool
}

type Rank = 
    | One = 0
    | Two = 1 
    | Three = 2
    | Four = 3
    | Five = 4 
    | Six = 5
    | Seven = 6
    | Eight = 7

type File = 
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | E = 4
    | F = 5
    | G = 6
    | H = 7

type Location = File * Rank
module Location =
    let fromString (str: string): Location option = 
        if str = "-" then None
        else 
        let file: File = enum (int (str.[0] - 'a'))
        let rank: Rank = enum (int (str.[1] - '1'))
        Some (file, rank)

    let toInt (file: File, rank: Rank) =
        (int rank <<< 3) + int file

type CastleDirection = 
    | KingSide
    | QueenSide

type MoveType =
    | Normal
    | Capture of Piece
    | Castle of CastleDirection
    | EnPassant
    | Promotion of Piece
    | CapturePromotion of Piece * Piece

type Move = {
    Piece: Piece
    Source: int
    Target: int
    Flags: MoveType
}

type Board = {
    ActiveColor: Color
    Board: byte[]
    White: Player
    Black: Player
    EnPassant: int option
    HalfmoveCount: int
}

module Board = 
    let create (fen: string) = 
        let Board = Array.create 64 0uy
        let mutable wKing, bKing = -1, -1;
        let mutable idx = 56

        let pieces, active, castling, enPassant, halfMove = 
            match fen.Split ' ' with
            | [|a;b;c;d;e;f|] -> a,b,c,d,e
            | _ -> "a","b","c","d","e"

        String.iter (fun char -> 
            if char = '/' then idx <- idx - 16
            else
            match char with 
                | 'K' -> 
                    wKing <- idx
                    Piece.generate Color.White PieceType.King
                | 'k' -> 
                    bKing <- idx
                    Piece.generate Color.Black PieceType.King
                | 'Q' -> Piece.generate Color.White PieceType.Queen
                | 'q' -> Piece.generate Color.Black PieceType.Queen
                | 'R' -> Piece.generate Color.White PieceType.Rook
                | 'r' -> Piece.generate Color.Black PieceType.Rook
                | 'B' -> Piece.generate Color.White PieceType.Bishop
                | 'b' -> Piece.generate Color.Black PieceType.Bishop
                | 'N' -> Piece.generate Color.White PieceType.Knight
                | 'n' -> Piece.generate Color.Black PieceType.Knight
                | 'P' -> Piece.generate Color.White PieceType.Pawn
                | 'p' -> Piece.generate Color.Black PieceType.Pawn
                | _ -> 0uy
            |> Array.set Board idx

            if char < '9' then idx <- idx + int (char - '1')

            idx <- idx + 1

        ) pieces

        let ActiveColor = 
            match active with
            | "w" -> Color.White
            | "b" -> Color.Black
            | _ -> Color.White

        let mutable wKingSide, wQueenSide= false, false
        let mutable bKingSide, bQueenSide= false, false

        String.iter (fun char -> 
            match char with 
            | 'K' -> wKingSide <- true
            | 'k' -> bKingSide <- true
            | 'Q' -> wQueenSide <- true
            | 'q' -> bQueenSide <- true
            | _ -> ()
        ) castling

        let enPassantTarget = 
            enPassant
            |> Location.fromString
            |> Option.map Location.toInt 

        let HalfmoveCount = int halfMove

        let White = {
            KingLocation = wKing
            Castling = wKingSide, wQueenSide
        }

        let Black = {
            KingLocation = bKing
            Castling = bKingSide, bQueenSide
        }

        {
            ActiveColor = ActiveColor
            Board = Board
            White = White
            Black = Black
            EnPassant = enPassantTarget
            HalfmoveCount = HalfmoveCount
        }

    let getPlayer board =
        match board.ActiveColor with
        | Color.White -> board.White
        | Color.Black -> board.Black
        | _ -> board.White

    let setPlayer player board =
        match board.ActiveColor with
        | Color.White -> {board with White = player}
        | Color.Black -> {board with Black = player}
        | _ -> board

    let getOpponent board = 
        match board.ActiveColor with
        | Color.White -> board.Black
        | Color.Black -> board.White
        | _ -> board.White

    let setOpponent player board =
        match board.ActiveColor with
        | Color.White -> {board with Black = player}
        | Color.Black -> {board with White = player}
        | _ -> board
    
    let private boardArrayToString (boardArray: byte[]) =
        fun index -> 
            let rank = index >>> 3
            let file = index &&& 7
            let correctedRank = 7 - rank
            let piece = Piece.toString boardArray.[(correctedRank <<< 3) + file]
            if index &&& 7 = 0 then $"\n|-------------------------------|\n| %s{piece}| " else $"%s{piece}| "
        |> String.init 64
        |> (+) <| "\n|-------------------------------|\n"

    let toString (board: Board) = 
        match board.ActiveColor with
        | Color.White -> "White to Move\n"
        | Color.Black -> "Black to Move\n"
        | _ -> "Unexpected error!\n"
        |> (+) (boardArrayToString board.Board)
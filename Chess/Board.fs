namespace SpellChess.Chess

module Hexuple =
    let toArray (a, b, c, d, e, f) = [|a; b; c; d; e; f|]
    let toList (a, b, c, d, e, f) = [a; b; c; d; e; f]

type Piece = 
| King = 0
| Queen = 1
| Rook = 2
| Bishop = 3
| Knight = 4
| Pawn = 5
module Piece = 
    let toString piece =
        match piece with
            | Piece.King -> "♔ "
            | Piece.Queen -> "♕ "
            | Piece.Rook -> "♖ "
            | Piece.Bishop -> "♗ "
            | Piece.Knight -> "♘ "
            | _ -> "♙ "

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
        | Piece.King -> king
        | Piece.Queen -> queen
        | Piece.Rook -> rook
        | Piece.Bishop -> bishop
        | Piece.Knight -> knight
        | Piece.Pawn -> pawn
        | _ -> 0uL

    let setBitBoard piece pieces bitBoard =
        let king, queen, rook, bishop, knight, pawn = pieces
        match piece with 
        | Piece.King -> bitBoard, queen, rook, bishop, knight, pawn
        | Piece.Queen -> king, bitBoard, rook, bishop, knight, pawn
        | Piece.Rook -> king, queen, bitBoard, bishop, knight, pawn
        | Piece.Bishop -> king, queen, rook, bitBoard, knight, pawn
        | Piece.Knight -> king, queen, rook, bishop, bitBoard, pawn
        | Piece.Pawn -> king, queen, rook, bishop, knight, bitBoard
        | _ -> pieces

    let getPieceAtLocation (pieces: PieceBoards) (location: int) =
        let king, queen, rook, bishop, knight, pawn = pieces
        match 1uL <<< location with
        | x when king &&& x <> 0uL -> Some Piece.King
        | x when queen &&& x <> 0uL -> Some Piece.Queen
        | x when rook &&& x <> 0uL -> Some Piece.Rook
        | x when bishop &&& x <> 0uL -> Some Piece.Bishop
        | x when knight &&& x <> 0uL -> Some Piece.Knight
        | x when pawn &&& x <> 0uL -> Some Piece.Pawn
        | x -> None

type Player = {
    Pieces: PieceBoards
    Castling: bool * bool
}

type Side = 
    | White = 0
    | Black = 1

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
    Player: Side
    White: Player
    Black: Player
    EnPassant: int option
    HalfmoveCount: int
    Moves: Move list
}

module Board = 
    let create (fen: string) = 
        let mutable wKing, wQueen, wRook, wBishop, wKnight, wPawn = 0uL, 0uL, 0uL, 0uL, 0uL, 0uL
        let mutable bKing, bQueen, bRook, bBishop, bKnight, bPawn = 0uL, 0uL, 0uL, 0uL, 0uL, 0uL

        let mutable idx = 56

        let pieces, active, castling, enPassant, halfMove = 
            match fen.Split ' ' with
            | [|a;b;c;d;e;f|] -> a,b,c,d,e
            | _ -> "a","b","c","d","e"

        String.iter (fun char -> 
            if char = '/' then idx <- idx - 16
            else
            match char with 
                | 'K' -> wKing <- wKing ||| (1uL <<< idx)
                | 'k' -> bKing <- bKing ||| (1uL <<< idx)
                | 'Q' -> wQueen <- wQueen ||| (1uL <<< idx)
                | 'q' -> bQueen <- bQueen ||| (1uL <<< idx)
                | 'R' -> wRook <- wRook ||| (1uL <<< idx)
                | 'r' -> bRook <- bRook ||| (1uL <<< idx)
                | 'B' -> wBishop <- wBishop ||| (1uL <<< idx)
                | 'b' -> bBishop <- bBishop ||| (1uL <<< idx)
                | 'N' -> wKnight <- wKnight ||| (1uL <<< idx)
                | 'n' -> bKnight <- bKnight ||| (1uL <<< idx)
                | 'P' -> wPawn <- wPawn ||| (1uL <<< idx)
                | 'p' -> bPawn <- bPawn ||| (1uL <<< idx)
                | _ -> ()

            if char < '9' then idx <- idx + int (char - '1')

            idx <- idx + 1

        ) pieces

        let Player = match active with
                        | "w" -> Side.White
                        | "b" -> Side.Black
                        | _ -> Side.White

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
            Option.map 
                (fun (f, r) -> int r * 8 + int f) 
                (Location.fromString enPassant)

        let HalfmoveCount = int halfMove

        let White = {
            Pieces = wKing, wQueen, wRook, wBishop, wKnight, wPawn
            Castling = wKingSide, wQueenSide
        }

        let Black = {
            Pieces = bKing, bQueen, bRook, bBishop, bKnight, bPawn
            Castling = bKingSide, bQueenSide
        }

        {
            White = White
            Black = Black
            Player = Player
            EnPassant = enPassantTarget
            HalfmoveCount = HalfmoveCount
            Moves = []
        }

    let getPlayer board =
        match board.Player with
        | Side.White -> board.White
        | Side.Black -> board.Black
        | _ -> board.White

    let setPlayer player board =
        match board.Player with
        | Side.White -> {board with White = player}
        | Side.Black -> {board with Black = player}
        | _ -> board

    let getOpponent board = 
        match board.Player with
        | Side.White -> board.Black
        | Side.Black -> board.White
        | _ -> board.White

    let setOpponent player board =
        match board.Player with
        | Side.White -> {board with Black = player}
        | Side.Black -> {board with White = player}
        | _ -> board
    
    let private uintArraysToString whitePieces blackPieces =
        let wKing, wQueen, wRook, wBishop, wKnight, wPawn = whitePieces
        let bKing, bQueen, bRook, bBishop, bKnight, bPawn = blackPieces
        
        fun index -> 
            let rank = 7 - index / 8
            let file = index % 8

            match 1uL <<< rank*8 + file with
            // | _ -> "|" + string (char file + 'a') + string (char rank + '1')
            | v when v &&& wKing <> 0uL -> "| ♔ "
            | v when v &&& wQueen <> 0uL -> "| ♕ "
            | v when v &&& wRook <> 0uL -> "| ♖ "
            | v when v &&& wBishop <> 0uL -> "| ♗ "
            | v when v &&& wKnight <> 0uL -> "| ♘ "
            | v when v &&& wPawn <> 0uL -> "| ♙ "
            | v when v &&& bKing <> 0uL -> "| ♚ "
            | v when v &&& bQueen <> 0uL -> "| ♛ "
            | v when v &&& bRook <> 0uL -> "| ♜ "
            | v when v &&& bBishop <> 0uL -> "| ♝ "
            | v when v &&& bKnight <> 0uL -> "| ♞ "
            | v when v &&& bPawn <> 0uL -> "| ♟ "
            | _ -> "|   "
            |> fun s -> if index % 8 = 7 then s + "|\n|-------------------------------|\n" else s
        |> String.init 64
        |> (+) "|-------------------------------|\n"

    let toString (board: Board) = 
        match board.Player with
        | Side.White -> "White to Move\n"
        | Side.Black -> "Black to Move\n"
        | _ -> "Unexpected error!\n"
        |> (+) (uintArraysToString board.White.Pieces board.Black.Pieces)

module Move =
        let toString (move: Move) =
            let piece = Piece.toString move.Piece

            let targetSquare = 
                string (char (move.Target &&& 7) + 'a') + string ((move.Target >>> 3) + 1)
                // string move.Target
            
            match move.Flags with
            | Normal -> piece + targetSquare
            | Capture _ | EnPassant -> piece + "x" + targetSquare
            | Promotion target -> piece + targetSquare + Piece.toString target
            | CapturePromotion (_,target) -> piece + "x" + targetSquare + Piece.toString target
            | Castle KingSide -> "O-O"
            | Castle QueenSide -> "O-O-O"

        let private normalMove (board: Board) (move: Move) =
            let player = Board.getPlayer board
            let playerBoard = player.Pieces
            
            let newPlayer = 
                PieceBoards.getBitBoard move.Piece playerBoard
                |> (&&&) ~~~(1uL <<< move.Source)
                |> (|||) (1uL <<< move.Target)
                |> PieceBoards.setBitBoard move.Piece playerBoard
                |> fun pieces -> {player with Pieces = pieces}

            Board.setPlayer newPlayer board

        let private capture (board: Board) (move: Move) (target: Piece) =
            let player = Board.getPlayer board
            let opponent = Board.getOpponent board
            let playerBoard = player.Pieces
            let opponentBoard = opponent.Pieces

            let newPlayer = 
                PieceBoards.getBitBoard move.Piece playerBoard
                |> (&&&) ~~~(1uL <<< move.Source)
                |> (|||) (1uL <<< move.Target)
                |> PieceBoards.setBitBoard move.Piece playerBoard
                |> fun pieces -> {player with Pieces = pieces}

            let newOpponent = 
                PieceBoards.getBitBoard target opponentBoard
                |> (&&&) ~~~(1uL <<< move.Target)
                |> PieceBoards.setBitBoard target opponentBoard
                |> fun pieces -> {opponent with Pieces = pieces}

            board
            |> Board.setPlayer newPlayer
            |> Board.setOpponent newOpponent

        let private castle (move: Move) (direction: CastleDirection) (board: Board) = 
            let player = Board.getPlayer board
            let playerBoard = player.Pieces
            
            let rookLocation = 
                match direction with
                | KingSide -> 3
                | QueenSide -> -4
                |> (+) move.Source

            let newRookLocation = 
                match direction with
                | KingSide -> - 1
                | QueenSide -> 1
                |> (+) move.Target

            let newPlayer = 
                PieceBoards.getBitBoard Piece.Rook playerBoard
                |> (&&&) ~~~(1uL <<< rookLocation)
                |> (|||) (1uL <<< newRookLocation)
                |> PieceBoards.setBitBoard Piece.Rook playerBoard
                |> fun pieces -> {player with Pieces = pieces}

            Board.setPlayer newPlayer board

        let private enPassant (board: Board) (move: Move) =
            let player = Board.getPlayer board
            let opponent = Board.getOpponent board
            let playerBoard = player.Pieces
            let opponentBoard = opponent.Pieces

            let newPlayer = 
                PieceBoards.getBitBoard move.Piece playerBoard
                |> (&&&) ~~~(1uL <<< move.Source)
                |> (|||) (1uL <<< move.Target)
                |> PieceBoards.setBitBoard move.Piece playerBoard
                |> fun pieces -> {player with Pieces = pieces}

            let newOpponent = 
                PieceBoards.getBitBoard Piece.Pawn opponentBoard
                |> (&&&) ~~~(1uL <<< move.Target + 8)
                |> (&&&) ~~~(1uL <<< move.Target - 8)
                |> PieceBoards.setBitBoard move.Piece opponentBoard
                |> fun pieces -> {opponent with Pieces = pieces}

            board
            |> Board.setPlayer newPlayer
            |> Board.setOpponent newOpponent

        let private promote (board: Board) (move: Move) (pieceType: Piece) = 
            let player = Board.getPlayer board
            let playerBoard = player.Pieces

            let deletedPawnPieces = 
                PieceBoards.getBitBoard move.Piece playerBoard
                |> (&&&) ~~~(1uL <<< move.Source)
                |> PieceBoards.setBitBoard move.Piece playerBoard

            let newPlayer = 
                PieceBoards.getBitBoard pieceType deletedPawnPieces
                |> (|||) (1uL <<< move.Target)
                |> PieceBoards.setBitBoard pieceType playerBoard
                |> fun pieces -> {player with Pieces = pieces}

            board
            |> Board.setPlayer newPlayer
            
        let private capturePromotion (move: Move) (target: Piece) (board: Board) = 
            let opponent = Board.getOpponent board
            let opponentBoard = opponent.Pieces

            let newOpponent = 
                PieceBoards.getBitBoard target opponentBoard
                |> (&&&) ~~~(1uL <<< move.Target)
                |> PieceBoards.setBitBoard move.Piece opponentBoard
                |> fun pieces -> {opponent with Pieces = pieces}

            board
            |> Board.setOpponent newOpponent

        let private nextPlayer (board: Board) =
            let nextPlayer = 
                match board.Player with
                | Side.White -> Side.Black
                | Side.Black -> Side.White
                | _ -> Side.Black

            {board with Player = nextPlayer}

        let fiftyMoveRule move board = 
            match move.Flags with
            | Capture _ -> true
            | _ -> false
            |> fun capture -> 
                if capture || move.Piece = Piece.Pawn then {board with HalfmoveCount = 0}
                else {board with HalfmoveCount = board.HalfmoveCount + 1}

        let move (board: Board) (move: Move) = 
            match move.Flags with
            | Normal -> normalMove board move
            | Capture targetType -> capture board move targetType
            | Castle direction -> 
                normalMove board move
                |> castle move direction
            | EnPassant -> enPassant board move
            | Promotion pieceType -> promote board move pieceType
            | CapturePromotion (target, pieceType) -> 
                promote board move pieceType
                |> capturePromotion move target
            |> nextPlayer
            |> fiftyMoveRule move
            |> fun x -> {x with Moves = move :: x.Moves}

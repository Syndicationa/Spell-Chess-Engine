namespace SpellChess.Tinyhouse

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
| Nil = 0
| King = 1
| Wazir = 2
| Ferz = 3
| Xiangqi = 4
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
        |> (+) 32uy //This flags it was a pawn

    let originalType (piece: Piece) =
        if piece &&& 32uy = 32uy then PieceType.Pawn
        else pieceType piece

    let toString piece =
        match piece &&& 31uy with
            | p when p = generate Color.White PieceType.King -> "K "
            | p when p = generate Color.Black PieceType.King -> "k "

            | p when p = generate Color.White PieceType.Wazir -> "W "
            | p when p = generate Color.Black PieceType.Wazir -> "w "

            | p when p = generate Color.White PieceType.Ferz -> "F "
            | p when p = generate Color.Black PieceType.Ferz -> "f "

            | p when p = generate Color.White PieceType.Xiangqi -> "U "
            | p when p = generate Color.Black PieceType.Xiangqi -> "u "

            | p when p = generate Color.White PieceType.Pawn -> "♙ "
            | p when p = generate Color.Black PieceType.Pawn -> "♟ "

            | _ -> "  "

type Player = {
    KingLocation: int
    Placeables: int array
}

type Rank = 
    | One = 0
    | Two = 1 
    | Three = 2
    | Four = 3

type File = 
    | A = 0
    | B = 1
    | C = 2
    | D = 3

type Location = File * Rank
module Location =
    let fromString (str: string): Location option = 
        if str = "-" then None
        else 
        let file: File = enum (int (str.[0] - 'a'))
        let rank: Rank = enum (int (str.[1] - '1'))
        Some (file, rank)

    let toInt (file: File, rank: Rank) =
        (int rank <<< 2) + int file

type Board = {
    ActiveColor: Color
    Board: byte[]
    White: Player
    Black: Player
    MoveCount: int
    HalfmoveCount: int
}

module Board = 
    let create (fen: string) = 
        let Board = Array.create 16 0uy
        let mutable wKing, bKing = -1, -1;
        let mutable idx = 12

        let pieces, active, whiteInventory, blackInventory, halfMove, fullMove = 
            match fen.Split ' ' with
            | [|board; active; white; black; half; full|] -> board, active, white, black, half, full
            | _ -> "board","active","white","black","half","full"

        String.iter (fun char -> 
            if char = '/' then idx <- idx - 8
            else
            match char with 
                | 'K' -> 
                    wKing <- idx
                    Piece.generate Color.White PieceType.King
                | 'k' -> 
                    bKing <- idx
                    Piece.generate Color.Black PieceType.King
                | 'W' -> Piece.generate Color.White PieceType.Wazir
                | 'w' -> Piece.generate Color.Black PieceType.Wazir
                | 'F' -> Piece.generate Color.White PieceType.Ferz
                | 'f' -> Piece.generate Color.Black PieceType.Ferz
                | 'U' -> Piece.generate Color.White PieceType.Xiangqi
                | 'u' -> Piece.generate Color.Black PieceType.Xiangqi
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

        let HalfmoveCount = int halfMove
        let MoveCount = int fullMove

        let WhitePlaceables = Array.init 4 (fun _ -> 0)
        let BlackPlaceables = Array.init 4 (fun _ -> 0)

        let matchToPlace array piece =
            let index = 
                match piece with 
                | 'W' | 'w' -> 0
                | 'F' | 'f' -> 1
                | 'U' | 'u' -> 2
                | 'P' | 'p' -> 3
                | _ -> -1
            if index < 0 then ()
            else
            Array.get array index
            |> (+) 1
            |> Array.set array index

        String.iter (matchToPlace WhitePlaceables) whiteInventory
        String.iter (matchToPlace BlackPlaceables) blackInventory

        let White = {
            KingLocation = wKing
            Placeables = WhitePlaceables
        }

        let Black = {
            KingLocation = bKing
            Placeables = BlackPlaceables
        }

        {
            ActiveColor = ActiveColor
            Board = Board
            White = White
            Black = Black
            MoveCount = MoveCount
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
            let rank = index >>> 2
            let file = index &&& 3
            let correctedRank = 3 - rank
            let piece = Piece.toString boardArray.[(correctedRank <<< 2) + file]
            if index &&& 3 = 0 then $"\n|-------------------------------|\n| %s{piece}| " else $"%s{piece}| "
        |> String.init 16
        |> (+) <| "\n|-------------------------------|\n"

    let toString (board: Board) = 
        match board.ActiveColor with
        | Color.White -> "White to Move\n"
        | Color.Black -> "Black to Move\n"
        | _ -> "Unexpected error!\n"
        |> (+) (boardArrayToString board.Board)

    let toFENstring board = 
        let mutable str = ""
        let mutable count = 0
        for rank = 7 downto 0 do
            for file = 0 to 7 do
                let piece = board.Board.[(rank <<< 3) + file]
                if piece <> 0uy && count > 0 then
                    str <- str + string count
                    count <- 0
                
                if piece = 0uy then
                    count <- count + 1
                else
                    str <- 
                        match Piece.pieceType piece with
                        | PieceType.King -> 'k'
                        | PieceType.Wazir -> 'w'
                        | PieceType.Ferz -> 'f'
                        | PieceType.Xiangqi -> 'u'
                        | PieceType.Pawn -> 'p'
                        | _ -> ' '
                        |> match Piece.color piece with
                            | Color.White -> (+) ('A' - 'a')
                            | _ -> (+) (char 0)
                        |> string
                        |> (+) str
            if count <> 0 then 
                str <- str + string count
                count <- 0
            str <- str + "/"

        str <- str.[0..(String.length str - 2)]

        str <- str + 
            match board.ActiveColor with
            | Color.White -> " w "
            | _ -> " b "

        $"{str} {board.HalfmoveCount} {board.MoveCount}"
            
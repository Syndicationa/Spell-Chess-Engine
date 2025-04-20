namespace SpellChess

module Hexuple =
    let toArray (a, b, c, d, e, f) = [|a; b; c; d; e; f|]
    let toList (a, b, c, d, e, f) = [a; b; c; d; e; f]

module Chess = 
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

    module Generate =
        let rookDirections = [1, 0; -1, 0; 0, 1; 0, -1]
        let bishopDirections = [1, 1; -1, 1; -1, 1; 1, -1]
        let queenDirections = List.append rookDirections bishopDirections

        let knightDirections = [
            2, 1
            1, 2
            -2, 1
            1, -2
            2, -1
            -1, 2
            -2, -1
            -1, -2
        ]

        let private generateNormalsAndCaptures board location piece =
            let flags = PieceBoards.getPieceAtLocation (Board.getOpponent board).Pieces location
                        |> Option.map (fun piece -> Capture piece)
                        |> Option.defaultValue Normal 
            
            fun newLocation -> {Piece = piece; Source = location; Target = newLocation; Flags = flags}
            |> List.map

        let private generateSideInts board =
            let white = Array.fold (fun a i -> a ||| i) 0uL (Hexuple.toArray board.White.Pieces)
            let black = Array.fold (fun a i -> a ||| i) 0uL (Hexuple.toArray board.Black.Pieces)

            match board.Player with
            | Side.White -> white, black
            | Side.Black -> black, white
            | _ -> white, white

        let rec private findCapture friendly enemy position depth (d_file, d_rank) =
            let new_rank = position / 8 + d_rank
            let new_file = position % 8 + d_file

            if new_rank > 7 || new_rank < 0 || new_file > 7 || new_file < 0 || depth = 0 then false
            else

            match new_rank * 8 + new_file with
            | n when 1uL <<< n &&& enemy <> 0uL -> true
            | n when 1uL <<< n &&& friendly <> 0uL -> false
            | n -> findCapture friendly enemy n (depth - 1) (d_file, d_rank)
        
        let rec private squaresInDirection friendly enemy position depth list (d_file, d_rank) =
            let new_rank = (position >>> 3) + d_rank
            let new_file = (position &&& 7) + d_file

            if new_rank > 7 || new_rank < 0 || new_file > 7 || new_file < 0 || depth = 0 then list
            else

            // if d_file = 2 || d_file = -2 || d_rank = 2 || d_rank = -1
            //     then 
            //         printfn "%i + %i -> %i" position ((d_rank <<< 3) + d_file) ((new_rank <<< 3) + new_file)
            //         printfn "%i %i + (%i %i) -> %i %i" (position >>> 3) (position &&& 7) d_rank d_file new_rank new_file
            //         printfn "Is Valid: %b" (1uL <<< (new_rank <<< 3) + new_file &&& friendly = 0uL)

            match (new_rank <<< 3) + new_file with
            | n when 1uL <<< n &&& friendly <> 0uL -> list
            | n when 1uL <<< n &&& enemy <> 0uL -> n :: list
            | n -> squaresInDirection friendly enemy n (depth - 1) (n :: list) (d_file, d_rank)

        let private linearMoves (board: Board) (location: int) (piece: Piece) =
            let friendly, opponent= generateSideInts board

            let depth, list = 
                match piece with
                | Piece.King -> 1, queenDirections
                | Piece.Queen -> 8, queenDirections
                | Piece.Rook -> 8, rookDirections
                | Piece.Bishop -> 8, bishopDirections
                | Piece.Knight -> 1, knightDirections
                | _ -> 0, []

            list
            |> List.fold (squaresInDirection friendly opponent location depth) []
            |> generateNormalsAndCaptures board location piece

        let private castlingMoves (board: Board) (location: int): Move list = 
            let friendly, opponent = generateSideInts board
            let allPieces = friendly ||| opponent
            let kingSide, queenSide = (Board.getPlayer board).Castling

            let castleKingSide = 
                kingSide 
                && 1uL <<< location + 1 &&& allPieces = 0uL 
                && 1uL <<< location + 2 &&& allPieces = 0uL
            let castleQueenSide = 
                queenSide 
                && 1uL <<< location - 1 &&& allPieces = 0uL 
                && 1uL <<< location - 2 &&& allPieces = 0uL 
                && 1uL <<< location - 3 &&& allPieces = 0uL

            []
            |> fun list -> if not castleKingSide 
                                        then list 
                                        else {
                                            Piece = Piece.King;
                                            Source = location; Target = location + 2;
                                            Flags = Castle KingSide} :: list
            |> fun list -> if not castleQueenSide 
                                        then list 
                                        else {
                                            Piece = Piece.King;
                                            Source = location; Target = location - 2;
                                            Flags = Castle QueenSide} :: list

        let private generatePromotionsForMove (move: Move) =
            match move.Flags with
            | Normal -> 
                [
                    {move with Flags = Promotion Piece.Queen}
                    {move with Flags = Promotion Piece.Rook}
                    {move with Flags = Promotion Piece.Bishop}
                    {move with Flags = Promotion Piece.Knight}
                ]
            | Capture target -> 
                [
                    {move with Flags = CapturePromotion (target, Piece.Queen)}
                    {move with Flags = CapturePromotion (target, Piece.Rook)}
                    {move with Flags = CapturePromotion (target, Piece.Bishop)}
                    {move with Flags = CapturePromotion (target, Piece.Knight)}
                ]
            | _ -> [move]

        let rec private generatePromotions (generatedList: Move list) (moves: Move list) = 
            match moves with
            | [] -> generatedList
            | move::[] -> List.append generatedList (generatePromotionsForMove move)
            | move::rest -> generatePromotions (List.append generatedList (generatePromotionsForMove move)) rest

        let private checkThenGeneratePromotions (board: Board) (location: int) (moves: Move list) =
            let promotionRank = 
                match board.Player with
                | Side.White -> 6
                | Side.Black -> 1
                | _ -> -1
            
            if location >>> 3 <> promotionRank then moves
            else 
            generatePromotions [] moves

        let private pawnMoves (board: Board) (location: int) = 
            let friendly, opponent = generateSideInts board
            
            let side = match board.Player with
                            | Side.White -> 1
                            | Side.Black -> -1
                            | _ -> 0

            let rank = location >>> 3
            let file = location &&& 7

            let forward = location + side*8
            let leftAttack = forward - 1
            let rightAttack = forward + 1
            let forward2 = forward + side*8

            let canMoveForward = 1uL <<< forward &&& (friendly ||| opponent) = 0uL

            let canAttackLeft = 
                file > 0 
                && 1uL <<< leftAttack &&& opponent <> 0uL

            let canEnPassantLeft =
                file > 0 
                && Some leftAttack = board.EnPassant
            
            let canAttackRight = 
                file < 7
                && 1uL <<< rightAttack &&& opponent <> 0uL

            let canEnPassantRight =
                file < 7 
                && Some rightAttack = board.EnPassant
            
            let canMove2 = 
                (if board.Player = Side.White then 1 else 6) = rank 
                && canMoveForward 
                && 1uL <<< forward2 &&& (friendly ||| opponent) = 0uL

            [canMoveForward, forward; canAttackLeft, leftAttack; canAttackRight, rightAttack; canMove2, forward2]
            |> List.filter (fun (canMove, _) -> canMove)
            |> List.map (fun (_, location) -> location)
            |> generateNormalsAndCaptures board location Piece.Pawn
            |> checkThenGeneratePromotions board location
            |> fun list -> if not canEnPassantLeft 
                                        then list 
                                        else {
                                            Piece = Piece.Pawn;
                                            Source = location; Target = leftAttack;
                                            Flags = EnPassant} :: list
            |> fun list -> if not canEnPassantRight 
                                        then list 
                                        else {
                                            Piece = Piece.Pawn;
                                            Source = location; Target = rightAttack;
                                            Flags = EnPassant} :: list

        let private safeSquare (board: Board) (location: int) =
            let fKing, fQueen, fRook, fBishop, fKnight, fPawn = (Board.getOpponent board).Pieces
            let oKing, oQueen, oRook, oBishop, oKnight, oPawn = (Board.getPlayer board).Pieces
            
            let allPieces = 
                fKing ||| fQueen ||| fRook ||| fBishop ||| fKnight ||| fPawn ||| oKing ||| oQueen ||| oRook ||| oBishop ||| oKnight ||| oPawn

            match board.Player with
            | Side.White -> 1
            | Side.Black -> -1
            | _ -> 0
            |> fun direction -> 
                1uL <<< location + 7*direction &&& oPawn <> 0uL 
                || 1uL <<< location + 9*direction &&& oPawn <> 0uL
            |> (||) (List.exists (findCapture allPieces oKnight location 1) knightDirections)
            |> (||) (List.exists (findCapture allPieces oBishop location 8) bishopDirections)
            |> (||) (List.exists (findCapture allPieces oRook   location 8) rookDirections)
            |> (||) (List.exists (findCapture allPieces oQueen  location 8) queenDirections)
            |> (||) (List.exists (findCapture allPieces oKing   location 1) queenDirections)
            |> not

        let private noCheck (board: Board) =
            fun (move: Move) -> 
                let nextBoard = Move.move board move

                let testSquares  = 
                    match move.Flags with
                    | Castle KingSide | Castle QueenSide -> [move.Source..move.Target]
                    | _ -> [BitBoard.getLSB (PieceBoards.getBitBoard Piece.King (Board.getOpponent nextBoard).Pieces)]

                testSquares
                |> List.forall (safeSquare nextBoard) 
            |> List.filter
        
        let validMoves (board: Board) (intLoc: int) (piece: Piece) =
            match piece with
            | Piece.Queen | Piece.Rook | Piece.Bishop | Piece.Knight -> linearMoves board intLoc piece
            | Piece.King -> List.append (linearMoves board intLoc piece) (castlingMoves board intLoc)
            // | Piece.Knight -> otherMoves board intLoc piece
            | Piece.Pawn -> pawnMoves board intLoc
            | _ -> []
            |> noCheck board

        let allValidMoves (board: Board) =
            let king, queen, rook, bishop, knight, pawn = (Board.getPlayer board).Pieces

            fun (index: int) ->
                match 1uL <<< index with
                | x when x &&& king <> 0uL -> Piece.King, index
                | x when x &&& queen <> 0uL -> Piece.Queen, index
                | x when x &&& rook <> 0uL -> Piece.Rook, index
                | x when x &&& bishop <> 0uL -> Piece.Bishop, index
                | x when x &&& knight <> 0uL -> Piece.Knight, index
                | x when x &&& pawn <> 0uL -> Piece.Pawn, index
                | _ -> Piece.Pawn, -1
            |> List.init 64
            |> List.filter (fun (_, i) -> i >= 0)
            |> List.fold (fun list (piece, loc) -> List.append list (validMoves board loc piece)) []

    module Evaluate = 
        let private pieceValue piece =
            match piece with
            | Piece.Queen -> 9
            | Piece.Rook -> 5
            | Piece.Bishop -> 3
            | Piece.Knight -> 3
            | Piece.Pawn -> 1
            | _ -> 0

        let private boardValue (king, queen, rook, bishop, knight, pawn) =
            int (BitBoard.pieceCount queen) * pieceValue Piece.Queen
            |> (+) (int (BitBoard.pieceCount rook) * pieceValue Piece.Rook)
            |> (+) (int (BitBoard.pieceCount bishop) * pieceValue Piece.Bishop)
            |> (+) (int (BitBoard.pieceCount knight) * pieceValue Piece.Knight)
            |> (+) (int (BitBoard.pieceCount pawn) * pieceValue Piece.Pawn)

        let determineMate board = 
            Generate.allValidMoves board
            |> List.length
            |> (=) 0

        let evaluate (board: Board) =
            if determineMate board then -10000000
            else 
            boardValue (Board.getPlayer board).Pieces - boardValue (Board.getOpponent board).Pieces

    module Transposition = 
        module Random = 
            let private xorShiftLeft shift x = x ^^^ (x <<< shift)
            let private xorShiftRight shift x = x ^^^ (x >>> shift)

            //pseudoRNG stolen from Wikipedia
            let generate = xorShiftLeft 13 >> xorShiftRight 7 >> xorShiftLeft 17
        
        type Generator = {
            WhitePositions: PieceBoards list
            BlackPositions: PieceBoards list
            Black: uint64
            CastlingRights: uint64 * uint64 * uint64 * uint64
            EnPassantFile: uint64 array
        }

        module private Generator =
            let makePositions (seed: uint64) =
                let king = Random.generate seed
                let queen = Random.generate king
                let rook = Random.generate queen
                let bishop = Random.generate rook
                let knight = Random.generate bishop
                let pawn = Random.generate knight

                (king, queen,rook, bishop, knight, pawn), pawn

        let buildGenerator seed =
            let mutable seed = Random.generate seed
            let WhitePositions = 
                (fun x -> 
                    let pieces, nextSeed = Generator.makePositions seed
                    seed <- nextSeed
                    pieces
                ) |> List.init 64
            let BlackPositions = 
                (fun x -> 
                    let pieces, nextSeed = Generator.makePositions seed
                    seed <- nextSeed
                    pieces
                ) |> List.init 64
            let Black = Random.generate seed
            let CastlingRights = 
                Random.generate Black
                |> fun x -> x, Random.generate x
                |> fun (x, y) -> x, y, Random.generate y
                |> fun (x, y, z) -> seed <- Random.generate z; x, y, z, seed
            let EnPassantFile = 
                (fun x -> 
                    seed <- Random.generate seed
                    seed
                ) |> Array.init 4
            
            {
                WhitePositions = WhitePositions
                BlackPositions = BlackPositions
                Black = Black
                CastlingRights = CastlingRights
                EnPassantFile = EnPassantFile
            }

        module private Encode =
            let inline position board index (white, black) =
                let wKing, wQueen, wRook, wBishop, wKnight, wPawn = white
                let bKing, bQueen, bRook, bBishop, bKnight, bPawn = black

                let whitePiece = 
                    match PieceBoards.getPieceAtLocation board.White.Pieces index with
                    | Some Piece.King -> wKing
                    | Some Piece.Queen -> wQueen
                    | Some Piece.Rook -> wRook
                    | Some Piece.Bishop -> wBishop
                    | Some Piece.Knight -> wKnight
                    | Some Piece.Pawn -> wPawn
                    | Some _ -> 0uL
                    | None -> 0uL

                let blackPiece = 
                    match PieceBoards.getPieceAtLocation board.Black.Pieces index with
                    | Some Piece.King -> bKing
                    | Some Piece.Queen -> bQueen
                    | Some Piece.Rook -> bRook
                    | Some Piece.Bishop -> bBishop
                    | Some Piece.Knight -> bKnight
                    | Some Piece.Pawn -> bPawn
                    | Some _ -> 0uL
                    | None -> 0uL

                whitePiece ^^^ blackPiece
            
            let inline castling generator board =
                let wKingSide, wQueenSide = board.White.Castling
                let bKingSide, bQueenSide = board.Black.Castling
                
                let (wKing, wQueen, bKing, bQueen) = generator.CastlingRights

                if wKingSide then wKing else 0uL
                |> (^^^) (if wQueenSide then wQueen else 0uL)
                |> (^^^) (if bKingSide then bKing else 0uL)
                |> (^^^) (if bQueenSide then bQueen else 0uL)

        let encodeBoard (generator: Generator) (board: Board): uint64 =
            generator.Positions
            |> List.mapi (Encode.position board)
            |> List.fold (fun accumulator item -> accumulator ^^^ item) 0uL
            |> (^^^) (if board.Player = Side.Black then generator.Black else 0uL)
            |> (^^^) (Encode.castling generator board)
            |> (^^^) (match board.EnPassant with | Some i -> Array.get generator.EnPassantFile (i &&& 7) | None -> 0uL)

        let encodeMove generator transposition previousEnPassant move = 
            

    module Search =
        let rec alphaBetaNegaMax board alpha beta depth =
            if depth = 0 then Evaluate.evaluate board
            else

            let rec loopThroughAllMoves board alpha beta bestValue moveList =
                match moveList with
                | move :: rest -> 
                    let score = - alphaBetaNegaMax (Move.move board move) -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (6 - depth) "  ") (Move.toString move) score
                    let newBest = max score bestValue
                    let newAlpha = max score alpha

                    if score >= beta then newBest
                    else loopThroughAllMoves board newAlpha beta newBest rest
                | [] -> bestValue

            Generate.allValidMoves board
            // |> fun l -> List.iter (fun i -> printfn "%s" (Move.toString i)) l; l
            // |> fun l -> printfn "%i" (List.length l); l
            |> loopThroughAllMoves board alpha beta System.Int32.MinValue

        let findBestMove transposition board depth =
            let rec loopThroughAllMoves board alpha beta bestValue bestMove moveList =
                match moveList with
                | move :: rest -> 
                    let score = - alphaBetaNegaMax (Move.move board move) -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (6 - depth) "  ") (Move.toString move) score
                    let newBest, newBestMove =
                        if score > bestValue then score, Some move else bestValue, bestMove
                    let newAlpha = max score alpha

                    if score >= beta then newBest, newBestMove
                    else loopThroughAllMoves board newAlpha beta newBest newBestMove rest
                | [] -> bestValue, bestMove

            Generate.allValidMoves board
            // |> (fun list -> [List.item 19 list])
            |> loopThroughAllMoves board -System.Int32.MaxValue System.Int32.MaxValue -System.Int32.MaxValue None
            |> fun x -> transposition, x


// int alphaBeta( int alpha, int beta, int depthleft ) {
//    if( depthleft == 0 ) return quiesce( alpha, beta );
//    bestValue = -infinity;
//    for ( all moves)  {
//       score = -alphaBeta( -beta, -alpha, depthleft - 1 );
//       if( score > bestValue )
//       {
//          bestValue = score;
//          if( score > alpha )
//             alpha = score; // alpha acts like max in MiniMax
//       }
//       if( score >= beta )
//          return bestValue;   //  fail soft beta-cutoff, existing the loop here is also fine
//    }
//    return bestValue;
// }
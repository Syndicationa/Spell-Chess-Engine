namespace SpellChess.Chess
    open System.Collections.Generic
    type Generator = {
            WhitePositions: PieceBoards list
            BlackPositions: PieceBoards list
            Black: uint64
            CastlingRights: uint64 * uint64 * uint64 * uint64
            EnPassantFile: uint64 array
        }

    
    type EntryType =
            | Exact
            | Lower
            | Upper

    type Entry = {
        Encoding: uint64
        Depth: int
        Score: int
        Type: EntryType
        Age: int
    }

    type TranspositionTable = {
        Generator: Generator
        Table: Dictionary<uint64, Entry>
    }


    module Transposition = 
        module Random = 
            let private xorShiftLeft shift x = x ^^^ (x <<< shift)
            let private xorShiftRight shift x = x ^^^ (x >>> shift)

            //pseudoRNG stolen from Wikipedia
            let generate = xorShiftLeft 13 >> xorShiftRight 7 >> xorShiftLeft 17
        
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
                ) |> Array.init 8
            
            {
                WhitePositions = WhitePositions
                BlackPositions = BlackPositions
                Black = Black
                CastlingRights = CastlingRights
                EnPassantFile = EnPassantFile
            }

        module private Encode =
            let position board index white black =
                let wKing, wQueen, wRook, wBishop, wKnight, wPawn = white
                let bKing, bQueen, bRook, bBishop, bKnight, bPawn = black

                let color = Piece.color board.Board.[index]
                let pieceType = Piece.pieceType board.Board.[index]

                match color, pieceType with
                | Color.White, PieceType.King -> wKing
                | Color.Black, PieceType.King -> bKing
                | Color.White, PieceType.Queen -> wQueen
                | Color.Black, PieceType.Queen -> bQueen
                | Color.White, PieceType.Rook -> wRook
                | Color.Black, PieceType.Rook -> bRook
                | Color.White, PieceType.Bishop -> wBishop
                | Color.Black, PieceType.Bishop -> bBishop
                | Color.White, PieceType.Knight -> wKnight
                | Color.Black, PieceType.Knight -> bKnight
                | Color.White, PieceType.Pawn -> wPawn
                | Color.Black, PieceType.Pawn -> bPawn
                | _, _ -> 0uL

            let inline castling generator board =
                let wKingSide, wQueenSide = board.White.Castling
                let bKingSide, bQueenSide = board.Black.Castling
                
                let wKing, wQueen, bKing, bQueen = generator.CastlingRights

                if wKingSide then wKing else 0uL
                |> (^^^) (if wQueenSide then wQueen else 0uL)
                |> (^^^) (if bKingSide then bKing else 0uL)
                |> (^^^) (if bQueenSide then bQueen else 0uL)

        let encodeBoard (generator: Generator) (board: Board): uint64 =
            List.mapi2 (Encode.position board) generator.WhitePositions generator.BlackPositions
            |> List.fold (fun accumulator item -> accumulator ^^^ item) 0uL
            |> (^^^) (if board.ActiveColor = Color.Black then generator.Black else 0uL)
            |> (^^^) (Encode.castling generator board)
            |> (^^^) (match board.EnPassant with | Some i -> Array.get generator.EnPassantFile (i &&& 7) | None -> 0uL)

        let encodeMove generator hash oldBoard newBoard move =
            let oldPosition = 
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Source]
                |> Hexuple.toArray
                |> Array.get <| int (Piece.pieceType move.Piece)

            let newPosition =
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Target]
                |> Hexuple.toArray
                |> fun x ->
                    match move.Flags with
                    | Promotion target | CapturePromotion (_, target) -> Array.get x  (int (Piece.pieceType target))
                    | _ -> Array.get x (int (Piece.pieceType move.Piece))

            let capturedPiece = 
                match move.Flags with
                | Normal | Promotion _ | Castle _ -> 0uL
                | Capture enemyPiece  | CapturePromotion (enemyPiece, _) -> 
                    match newBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Hexuple.toArray
                    |> Array.get <| int (Piece.pieceType enemyPiece)
                | EnPassant -> 
                    match newBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions.[(move.Source &&& ~~~7) + (move.Target &&& 7)]
                    | _ -> generator.BlackPositions.[(move.Source &&& ~~~7) + (move.Target &&& 7)]
                    |> Hexuple.toArray
                    |> Array.get <| int PieceType.Pawn
            
            let oldCastle = Encode.castling generator oldBoard
            let newCastle = Encode.castling generator newBoard

            let oldPassant = 
                match oldBoard.EnPassant with 
                | None -> 0uL
                | Some i -> generator.EnPassantFile.[i &&& 7]

            let newPassant = 
                match newBoard.EnPassant with
                | None -> 0uL
                | Some i -> generator.EnPassantFile.[i &&& 7]

            hash
            |> (^^^) oldPosition
            |> (^^^) newPosition
            |> (^^^) capturedPiece
            |> (^^^) oldCastle
            |> (^^^) newCastle
            |> (^^^) oldPassant
            |> (^^^) newPassant
            |> (^^^) generator.Black

        let createTable seed =
            {
                Generator = buildGenerator seed
                Table = Dictionary<uint64, Entry>(1 <<< 20)
            }

        let tryLookup table hash =
            match table.Table.TryGetValue hash with
            | true, value -> Some value
            | false, _ -> None

        let save table entry = 
            match table.Table.TryGetValue entry.Encoding with
            | true, current when current.Depth >= entry.Depth -> ()
            | _ -> table.Table.[entry.Encoding] <- entry

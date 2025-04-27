namespace SpellChess.Tinyhouse
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
                let wazir = Random.generate king
                let ferz = Random.generate wazir
                let xiangqi = Random.generate ferz
                let pawn = Random.generate xiangqi

                (king, wazir, ferz, xiangqi, pawn), pawn

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
                let wKing, wWazir, wFerz, wXiangqi, wPawn = white
                let bKing, bWazir, bFerz, bXiangqi, bPawn = black

                let color = Piece.color board.Board.[index]
                let pieceType = Piece.pieceType board.Board.[index]

                match color, pieceType with
                | Color.White, PieceType.King -> wKing
                | Color.Black, PieceType.King -> bKing
                | Color.White, PieceType.Wazir -> wWazir
                | Color.Black, PieceType.Wazir -> bWazir
                | Color.White, PieceType.Ferz -> wFerz
                | Color.Black, PieceType.Ferz -> bFerz
                | Color.White, PieceType.Xiangqi -> wXiangqi
                | Color.Black, PieceType.Xiangqi -> bXiangqi
                | Color.White, PieceType.Pawn -> wPawn
                | Color.Black, PieceType.Pawn -> bPawn
                | _, _ -> 0uL

        let encodeBoard (generator: Generator) (board: Board): uint64 =
            List.mapi2 (Encode.position board) generator.WhitePositions generator.BlackPositions
            |> List.fold (fun accumulator item -> accumulator ^^^ item) 0uL
            |> (^^^) (if board.ActiveColor = Color.Black then generator.Black else 0uL)

        let encodeMove generator hash oldBoard newBoard move =
            let oldPosition = 
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Source]
                |> Pentuple.toArray
                |> Array.get <| int (Piece.pieceType move.Piece)

            let newPosition =
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Target]
                |> Pentuple.toArray
                |> fun x ->
                    match move.Flags with
                    | Promotion target | CapturePromotion (_, target) -> Array.get x  (int (Piece.pieceType target))
                    | _ -> Array.get x (int (Piece.pieceType move.Piece))

            let capturedPiece = 
                match move.Flags with
                | Normal | Promotion _ -> 0uL
                | Capture enemyPiece  | CapturePromotion (enemyPiece, _) -> 
                    match newBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Pentuple.toArray
                    |> Array.get <| int (Piece.pieceType enemyPiece)
                | Place -> 
                    match oldBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Pentuple.toArray
                    |> fun x -> Array.get x (int (Piece.pieceType move.Piece))

            hash
            |> (^^^) oldPosition
            |> (^^^) newPosition
            |> (^^^) capturedPiece
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

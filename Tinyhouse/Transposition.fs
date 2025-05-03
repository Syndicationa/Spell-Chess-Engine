namespace SpellChess.Tinyhouse
    open System.Collections.Generic
    
    module Octuple =
        let toArray (king, wazir, ferz, xiangqi, pawn, pawnWazir, pawnFerz, pawnXiangqi) = 
            [|king; wazir; ferz; xiangqi; pawn; pawnWazir; pawnFerz; pawnXiangqi|]
        let toList (king, wazir, ferz, xiangqi, pawn, pawnWazir, pawnFerz, pawnXiangqi) = 
            [king; wazir; ferz; xiangqi; pawn; pawnWazir; pawnFerz; pawnXiangqi]

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

    type PieceBoards = BitBoard * BitBoard * BitBoard * BitBoard * BitBoard * BitBoard * BitBoard * BitBoard
    
    type Generator = {
            WhitePositions: PieceBoards list
            BlackPositions: PieceBoards list
            WhiteInventory: uint64 array
            BlackInventory: uint64 array
            Black: uint64
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
            let makePositions (seed: uint64): PieceBoards * uint64 =
                let king = Random.generate seed
                let wazir = Random.generate king
                let ferz = Random.generate wazir
                let xiangqi = Random.generate ferz
                let pawn = Random.generate xiangqi
                let pawnWazir = Random.generate pawn
                let pawnFerz = Random.generate pawnWazir
                let pawnXiangqi = Random.generate pawnFerz

                (king, wazir, ferz, xiangqi, pawn, pawnWazir, pawnFerz, pawnXiangqi), pawnXiangqi

        let buildGenerator seed =
            let mutable seed = Random.generate seed
            let WhitePositions = 
                (fun x -> 
                    let pieces, nextSeed = Generator.makePositions seed
                    seed <- nextSeed
                    pieces
                ) |> List.init 16
            let BlackPositions = 
                (fun x -> 
                    let pieces, nextSeed = Generator.makePositions seed
                    seed <- nextSeed
                    pieces
                ) |> List.init 16
            let Black = Random.generate seed
            let WhiteInventory = 
                (fun x -> 
                    seed <- Random.generate seed
                    seed
                ) |> Array.init 12

            let BlackInventory = 
                (fun x -> 
                    seed <- Random.generate seed
                    seed
                ) |> Array.init 12
            
            {
                WhitePositions = WhitePositions
                BlackPositions = BlackPositions
                WhiteInventory = WhiteInventory
                BlackInventory = BlackInventory
                Black = Black
            }

        module private Encode =
            let position board index (white: PieceBoards) (black: PieceBoards) =
                let wKing, wWazir, wFerz, wXiangqi, wPawn, wPawnWazir, wPawnFerz, wPawnXiangqi = white
                let bKing, bWazir, bFerz, bXiangqi, bPawn, bPawnWazir, bPawnFerz, bPawnXiangqi = black

                let color = Piece.color board.Board.[index]
                let pieceType = Piece.pieceType board.Board.[index]
                let promoted = board.Board.[index] &&& 32uy = 32uy

                match color, pieceType, promoted with
                | Color.White, PieceType.King, _ -> wKing
                | Color.Black, PieceType.King, _ -> bKing
                | Color.White, PieceType.Wazir, false -> wWazir
                | Color.White, PieceType.Wazir, true -> wPawnWazir
                | Color.Black, PieceType.Wazir, false -> bWazir
                | Color.Black, PieceType.Wazir, true -> bPawnWazir
                | Color.White, PieceType.Ferz, false -> wFerz
                | Color.White, PieceType.Ferz, true -> wPawnFerz
                | Color.Black, PieceType.Ferz, false -> bFerz
                | Color.Black, PieceType.Ferz, true -> bPawnFerz
                | Color.White, PieceType.Xiangqi, false -> wXiangqi
                | Color.White, PieceType.Xiangqi, true -> wPawnXiangqi
                | Color.Black, PieceType.Xiangqi, false -> bXiangqi
                | Color.Black, PieceType.Xiangqi, true -> bPawnXiangqi
                | Color.White, PieceType.Pawn, _ -> wPawn
                | Color.Black, PieceType.Pawn, _ -> bPawn
                | _, _, _ -> 0uL
            
            let inventory player (gen: uint64 array) =
                Array.mapi (fun index count -> gen.[count * 4 + index]) player.Placeables
                |> Array.fold (^^^) 0uL

        let encodeBoard (generator: Generator) (board: Board): uint64 =
            List.mapi2 (Encode.position board) generator.WhitePositions generator.BlackPositions
            |> List.fold (^^^) 0uL
            |> (^^^) (Encode.inventory board.White generator.WhiteInventory)
            |> (^^^) (Encode.inventory board.Black generator.BlackInventory)
            |> (^^^) (if board.ActiveColor = Color.Black then generator.Black else 0uL)

        let encodeMove generator hash oldBoard newBoard move =
            let oldPosition = 
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Source]
                |> Octuple.toArray
                |> Array.get <| int (Piece.pieceType move.Piece) - 1

            let newPosition =
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Target]
                |> Octuple.toArray
                |> fun x ->
                    match move.Flags with
                    | Promotion target | CapturePromotion (_, target) -> Array.get x  (int (Piece.pieceType target) - 1)
                    | _ -> Array.get x (int (Piece.pieceType move.Piece) - 1)

            let capturedPiece = 
                match move.Flags with
                | Normal | Promotion _ -> 0uL
                | Capture enemyPiece  | CapturePromotion (enemyPiece, _) -> 
                    match newBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Octuple.toArray
                    |> Array.get <| int (Piece.pieceType enemyPiece) - 1
                | Place -> 
                    match oldBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Octuple.toArray
                    |> fun x -> Array.get x (int (Piece.pieceType move.Piece) - 1)

            let oldPlayer = Board.getPlayer oldBoard
            let newPlayer = Board.getOpponent newBoard
            let inventory = match oldBoard.ActiveColor with | Color.White -> generator.WhiteInventory | _ -> generator.BlackInventory

            hash
            |> (^^^) oldPosition
            |> (^^^) newPosition
            |> (^^^) capturedPiece
            |> (^^^) (Encode.inventory oldPlayer inventory)
            |> (^^^) (Encode.inventory newPlayer inventory)
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

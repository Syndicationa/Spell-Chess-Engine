namespace SpellChess.Chess

    module Transposition = 
        open System.Collections.Generic

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
                |> Array.get <| int move.Piece

            let newPosition =
                match oldBoard.ActiveColor with
                | Color.White -> generator.WhitePositions
                | _ -> generator.BlackPositions
                |> fun x -> x.[move.Target]
                |> Hexuple.toArray
                |> fun x ->
                    match move.Flags with
                    | Promotion target | CapturePromotion (_, target) -> Array.get x  (int target)
                    | _ -> Array.get x (int move.Piece)

            let capturedPiece = 
                match move.Flags with
                | Normal | Promotion _ | Castle _ -> 0uL
                | Capture enemyType  | CapturePromotion (enemyType, _) -> 
                    match newBoard.ActiveColor with
                    | Color.White -> generator.WhitePositions
                    | _ -> generator.BlackPositions
                    |> fun x -> x.[move.Target]
                    |> Hexuple.toArray
                    |> Array.get <| int enemyType
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
                | Some i -> generator.EnPassantFile.[i]

            let newPassant = 
                match newBoard.EnPassant with
                | None -> 0uL
                | Some i -> generator.EnPassantFile.[i]

            hash
            |> (^^^) oldPosition
            |> (^^^) newPosition
            |> (^^^) capturedPiece
            |> (^^^) oldCastle
            |> (^^^) newCastle
            |> (^^^) oldPassant
            |> (^^^) newPassant
            |> (^^^) generator.Black

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

        type Table = {
            Generator: Generator
            Table: Dictionary<uint64, Entry>
        }

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

    module Search =
        let rec alphaBetaNegaMax (table: Transposition.Table) board alpha beta depth =
            let hashing = Transposition.encodeBoard table.Generator board

            let previousEval = Transposition.tryLookup table hashing
            let previousDepth = 
                match previousEval with 
                | Some eval -> eval.Depth
                | None -> -1
            
            if depth = 0 then Evaluate.evaluate board
            elif depth < previousDepth then 
                previousEval
                |> Option.map (fun entry -> entry.Score)
                |> Option.defaultValue System.Int32.MinValue
            else

            let rec loopThroughAllMoves board alpha beta bestValue moveList =
                match moveList with
                | move :: rest -> 
                    let newBoard = Move.move board move
                    let newHash: uint64 = Transposition.encodeMove table.Generator hashing board newBoard move

                    let score = - alphaBetaNegaMax table newBoard -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (6 - depth) "  ") (Move.toString move) score
                    let newBest = max score bestValue
                    let newAlpha = max score alpha

                    Transposition.save table {
                        Encoding = newHash
                        Depth = depth - 1
                        Score = score
                        Type = Transposition.Exact
                        Age = 100
                    }

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
                    let score = - alphaBetaNegaMax transposition (Move.move board move) -beta -alpha (depth - 1)
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

        let rec countValidMoves board depth =
            if  depth = 0 then 1
            else
            
            Generate.allValidMoves board
            |> List.sumBy (fun move -> countValidMoves (Move.move board move) (depth - 1))

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
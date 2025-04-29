namespace SpellChess.Tinyhouse
    module Evaluate = 
        let pieceValue piece =
            match piece with
            | PieceType.Wazir -> 30
            | PieceType.Ferz -> 20
            | PieceType.Xiangqi -> 20
            | PieceType.Pawn -> 10
            | _ -> 0

        let private boardValue =
            (fun (white, black) piece -> 
                let value = pieceValue (Piece.pieceType piece)
                match Piece.color piece with
                | Color.White -> white + value, black
                | Color.Black -> white, black + value
                | _ -> white, black
            )
            |> Array.fold <| (0, 0)

        let private inventoryValue (player: Player) =
            Array.mapi (fun index count -> pieceValue (enum (index + 2)) * count) player.Placeables
            |> Array.sum
            |> (/) <| 2

        let private captureValue capturingPiece targetPiece =
            10 * pieceValue (Piece.pieceType targetPiece) - pieceValue (Piece.pieceType capturingPiece)

        let private promotionValue promotionTarget =
            pieceValue PieceType.Pawn
            |> (-) (pieceValue (Piece.pieceType promotionTarget))

        let prescore move =
            match move.Flags with
            | Capture target -> captureValue move.Piece target
            | Promotion target -> promotionValue target
            | CapturePromotion (captured, promotion) -> 
                captureValue move.Piece captured + promotionValue promotion
            | Place -> pieceValue (Piece.pieceType move.Piece)
            | _ -> 0

        let determineCheck board =
            (Board.getPlayer board).KingLocation
            |> Generate.safeSquare board
            |> not

        let determineMate board = 
            Generate.allValidMoves board
            |> List.length
            |> (=) 0
            |> (&&) (determineCheck board)

        let determineStalemate board =
            Generate.allValidMoves board
            |> List.length
            |> (=) 0
            |> (&&) (not (determineCheck board))
            |> (||) (board.HalfmoveCount >= 50)

        let evaluate (board: Board) =
            if determineMate board then 1200000000
            else 
            let white, black = boardValue board.Board
            let whiteInventory = inventoryValue board.White
            let blackInventory = inventoryValue board.Black

            match board.ActiveColor with
            | Color.White -> white + whiteInventory - (black + blackInventory)
            | Color.Black -> black + blackInventory - (white + whiteInventory)
            | _ -> -3000

    module Search =
        open System.Diagnostics

        let sortMoves =
            List.sortByDescending Evaluate.prescore

        let rec alphaBetaNegaMax (stopwatch: Stopwatch) timeLimit (table: TranspositionTable) board hash alpha beta depth =
            if stopwatch.ElapsedMilliseconds >= timeLimit then System.Int32.MaxValue
            else

            let rec loopThroughAllMoves alpha beta bestValue moveList =
                match moveList with
                | move :: rest -> 
                    let newBoard = Move.move board move
                    let newHash: uint64 = Transposition.encodeMove table.Generator hash board newBoard move

                    let score = - alphaBetaNegaMax stopwatch timeLimit table newBoard newHash -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (10 - depth) "  ") (Move.toString move) score
                    let newBest = max score bestValue
                    let newAlpha = max score alpha

                    if stopwatch.ElapsedMilliseconds >= timeLimit then System.Int32.MaxValue
                    else

                    if score >= beta then score
                    else 
                    Transposition.save table {
                        Encoding = newHash
                        Depth = depth - 1
                        Score = score
                        Type = if score >= beta then Lower elif score <= alpha then Upper else Exact
                        Age = board.MoveCount
                    }

                    loopThroughAllMoves newAlpha beta newBest rest
                | [] -> bestValue
            
            let previousScore = 
                match Transposition.tryLookup table hash with 
                | Some eval when eval.Depth > depth ->
                    match eval.Type with
                    | Exact -> Some eval.Score
                    | Lower when eval.Score >= beta -> Some eval.Score
                    | Upper when eval.Score <= alpha -> Some eval.Score
                    | _ -> None
                | _ -> None
            
            match previousScore, depth with
            | Some n, _ -> n
            | None, 0 -> 
                let eval = Evaluate.evaluate board
                Transposition.save table {
                        Encoding = hash
                        Depth = depth
                        Score = eval
                        Type = Exact
                        Age = board.MoveCount
                }
                eval
            | None, _ ->
                Generate.allValidMoves board
                |> sortMoves
                |> function
                    | [] when not (Evaluate.determineCheck board) || board.MoveCount > 50 -> 0
                    | [] -> - 1200000000 - depth
                    | list -> loopThroughAllMoves alpha beta -System.Int32.MaxValue list

        let findBestMove (transposition: TranspositionTable) board length =
            let boardHash = Transposition.encodeBoard transposition.Generator board

            let stopwatch = Stopwatch.StartNew()
            let mutable moveList = 
                Generate.allValidMoves board
                |> List.toArray
                |> Array.map (fun x -> Evaluate.prescore x, x)
                |> Array.sortBy fst
            let mutable iterationDepth = 1
            let mutable alpha = -System.Int32.MaxValue
            let mutable beta = System.Int32.MaxValue
            let mutable bestScore = - System.Int32.MaxValue
            let mutable bestMove = snd moveList.[0]
            let mutable currentBestScore, currentBestMove = bestScore, bestMove

            if Array.length moveList = 1 then transposition, moveList.[0]
            else

            while stopwatch.ElapsedMilliseconds < length do
                alpha <- - System.Int32.MaxValue
                beta <- System.Int32.MaxValue
                currentBestScore <- alpha

                moveList <- Array.map (fun (previousScore: int, move) ->
                    if stopwatch.ElapsedMilliseconds >= length then previousScore, move
                    else
                    let newBoard = Move.move board move
                    let newHash = Transposition.encodeMove transposition.Generator boardHash board newBoard move

                    let score = 
                        - alphaBetaNegaMax stopwatch length transposition newBoard newHash -beta -alpha iterationDepth

                    if stopwatch.ElapsedMilliseconds >= length then previousScore, move
                    else

                    if score > currentBestScore then
                        currentBestScore <- score
                        currentBestMove <- move

                    alpha <- max alpha score

                    Transposition.save transposition {
                        Encoding = newHash
                        Depth = iterationDepth - 1
                        Score = score
                        Type = Exact
                        Age = board.MoveCount
                    }

                    score, move
                ) moveList

                moveList <- Array.sortByDescending fst moveList
                bestScore <- fst moveList.[0]
                // printfn "Best score at %i %i" iterationDepth bestScore
                bestMove <- snd moveList.[0]

                if stopwatch.ElapsedMilliseconds < length then
                    iterationDepth <- iterationDepth + 1

            stopwatch.Stop()
            printfn "Searched to a depth of %i taking %i" iterationDepth stopwatch.ElapsedMilliseconds
            Array.iter (fun (score, move) -> printfn "Score of %s is %i at depth %i" (Move.toString move) score iterationDepth) moveList
            
            transposition, (bestScore, bestMove)

        let rec countValidMoves board depth =
            if  depth = 0 then 1
            else
            
            Generate.allValidMoves board
            |> List.sumBy (fun move -> 
                countValidMoves (Move.move board move) (depth - 1)
            )
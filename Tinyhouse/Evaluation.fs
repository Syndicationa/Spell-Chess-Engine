namespace SpellChess.Tinyhouse
    type Config = {
        Wazir: int
        Ferz: int
        Xiangqi: int
        Pawn: int
        Inventory: int -> int
    }
    module Evaluate = 
        let pieceValue config piece =
            match piece with
            | PieceType.Wazir -> config.Wazir
            | PieceType.Ferz -> config.Ferz
            | PieceType.Xiangqi -> config.Xiangqi
            | PieceType.Pawn -> config.Pawn
            | _ -> 0

        let private boardValue config =
            (fun (white, black) piece -> 
                let value = pieceValue config (Piece.pieceType piece)
                match Piece.color piece with
                | Color.White -> white + value, black
                | Color.Black -> white, black + value
                | _ -> white, black
            )
            |> Array.fold <| (0, 0)

        let private inventoryValue config (player: Player) =
            Array.mapi (fun index count -> pieceValue config (enum (index + 2)) * count) player.Placeables
            |> Array.sum
            |> config.Inventory

        let private captureValue config capturingPiece targetPiece =
            10 * pieceValue config (Piece.pieceType targetPiece) - pieceValue config (Piece.pieceType capturingPiece)

        let private promotionValue config promotionTarget =
            pieceValue config PieceType.Pawn
            |> (-) (pieceValue config (Piece.pieceType promotionTarget))

        let prescore config move =
            match move.Flags with
            | Capture target -> captureValue config move.Piece target
            | Promotion target -> promotionValue config target
            | CapturePromotion (captured, promotion) -> 
                captureValue config move.Piece captured + promotionValue config promotion
            | Place -> pieceValue config (Piece.pieceType move.Piece)
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

        let evaluate config (board: Board) =
            if determineMate board then -1200000000
            else 
            let moveCount = 
                Generate.allValidMoves board
                |> List.length
            
            let white, black = boardValue config board.Board
            let whiteInventory = inventoryValue config board.White
            let blackInventory = inventoryValue config board.Black

            match board.ActiveColor with
            | Color.White -> white + whiteInventory - (black + blackInventory)
            | Color.Black -> black + blackInventory - (white + whiteInventory)
            | _ -> -3000
            |> (+) moveCount

    module Search =
        open System.Diagnostics

        type Data = {
            Config: Config
            Timer: Stopwatch
            TimeLimit: int64
            Table: TranspositionTable
        }

        module Data =
            let timeReached data =
                data.Timer.ElapsedMilliseconds >= data.TimeLimit

        let sortMoves config =
            List.sortByDescending (Evaluate.prescore config)

        let rec alphaBetaNegaMax (data: Data) board hash alpha beta depth =
            if Data.timeReached data then System.Int32.MaxValue
            else

            let rec loopThroughAllMoves alpha beta bestValue moveList =
                match moveList with
                | move :: rest -> 
                    let newBoard = Move.move board move
                    let newHash: uint64 = Transposition.encodeMove data.Table.Generator hash board newBoard move

                    let score = - alphaBetaNegaMax data newBoard newHash -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (10 - depth) "  ") (Move.toString move) score
                    let newBest = max score bestValue
                    let newAlpha = max score alpha

                    if Data.timeReached data then System.Int32.MaxValue 
                    else

                    if score >= beta then score
                    else 
                    Transposition.save data.Table {
                        Encoding = newHash
                        Depth = depth - 1
                        Score = score
                        Type = if score >= beta then Lower elif score <= alpha then Upper else Exact
                        Age = board.MoveCount
                    }

                    loopThroughAllMoves newAlpha beta newBest rest
                | [] -> bestValue
            
            let previousScore = 
                match Transposition.tryLookup data.Table hash with 
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
                Evaluate.evaluate data.Config board
            | None, _ ->
                Generate.allValidMoves board
                |> sortMoves data.Config
                |> function
                    | [] when not (Evaluate.determineCheck board) || board.HalfmoveCount > 50 -> 0
                    | [] -> - 1200000000 - depth
                    | list -> loopThroughAllMoves alpha beta -System.Int32.MaxValue list

        let findBestMove config (transposition: TranspositionTable) board length =
            let boardHash = Transposition.encodeBoard transposition.Generator board

            let stopwatch = Stopwatch.StartNew()
            let mutable moveList = 
                Generate.allValidMoves board
                |> List.toArray
                |> Array.map (fun x -> Evaluate.prescore config x, x)
                |> Array.sortBy fst
            let mutable iterationDepth = 1
            let mutable alpha = -System.Int32.MaxValue
            let mutable beta = System.Int32.MaxValue
            let mutable bestScore = - System.Int32.MaxValue
            let mutable bestMove = snd moveList.[0]
            let mutable currentBestScore, currentBestMove = bestScore, bestMove

            if Array.length moveList = 1 then 
                stopwatch.Stop()
                transposition, moveList.[0]
            else

            let data: Data = {
                Config = config
                Timer = stopwatch
                TimeLimit = length
                Table = transposition
            }

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
                        - alphaBetaNegaMax data newBoard newHash -beta -alpha iterationDepth

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
                printfn "Best score at %i %i" iterationDepth bestScore
                bestMove <- snd moveList.[0]

                if stopwatch.ElapsedMilliseconds < length then
                    iterationDepth <- iterationDepth + 1

            stopwatch.Stop()
            printfn "Searched to a depth of %i taking %i" iterationDepth stopwatch.ElapsedMilliseconds
            Array.iter (fun (score, move) -> printfn "Score of %s is %i at depth %i" (Move.toString move) score iterationDepth) moveList
            
            transposition, (bestScore, bestMove)

        let findBestAtDepth config (transposition: TranspositionTable) board depth =
            let boardHash = Transposition.encodeBoard transposition.Generator board

            let stopwatch = Stopwatch.StartNew()
            stopwatch.Stop()
            let moveList = 
                Generate.allValidMoves board
                |> List.toArray
                |> Array.map (fun x -> Evaluate.prescore config x, x)
                |> Array.sortBy fst

            if Array.length moveList = 1 then transposition, moveList.[0]
            else
            
            let mutable alpha = -System.Int32.MaxValue
            let beta = System.Int32.MaxValue
            let mutable currentBestScore, currentBestMove = System.Int32.MinValue, snd moveList.[0]

            let data: Data = {
                Config = config
                Timer = stopwatch
                TimeLimit = 100
                Table = transposition
            }

            Array.iter (fun (_, move) ->
                let newBoard = Move.move board move
                let newHash = Transposition.encodeMove transposition.Generator boardHash board newBoard move

                let score = 
                    - alphaBetaNegaMax data newBoard newHash -beta -alpha depth

                if score > currentBestScore then
                    currentBestScore <- score
                    currentBestMove <- move

                alpha <- max alpha score

                Transposition.save transposition {
                    Encoding = newHash
                    Depth = depth
                    Score = score
                    Type = Exact
                    Age = board.MoveCount
                }

                ()
            ) moveList
            
            transposition, (currentBestScore, currentBestMove)


        let rec countValidMoves board depth =
            if  depth = 0 then 1
            else
            
            Generate.allValidMoves board
            |> List.sumBy (fun move -> 
                countValidMoves (Move.move board move) (depth - 1)
            )
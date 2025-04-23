namespace SpellChess.Chess
    module Evaluate = 
        let pieceValue piece =
            match piece with
            | PieceType.Queen -> 90
            | PieceType.Rook -> 50
            | PieceType.Bishop -> 30
            | PieceType.Knight -> 30
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

        let private captureValue capturingPiece targetPiece =
            pieceValue (Piece.pieceType targetPiece) - pieceValue (Piece.pieceType capturingPiece)

        let private promotionValue promotionTarget =
            pieceValue PieceType.Pawn
            |> (-) (pieceValue (Piece.pieceType promotionTarget))

        let prescore move =
            match move.Flags with
            | Capture target -> captureValue move.Piece target
            | Promotion target -> promotionValue target
            | CapturePromotion (captured, promotion) -> 
                captureValue move.Piece captured + promotionValue promotion
            | _ -> 0

        let determineCheck board =
            (Board.getPlayer board).KingLocation
            |> Generate.safeSquare board

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
            if determineMate board then -10000000
            else 
            let white, black = boardValue board.Board

            match board.ActiveColor with
            | Color.White -> white - black
            | Color.Black -> black - white
            | _ -> -3000

    module Search =
        // let mutable evilNumber = 0
        let sortMoves =
            List.sortBy Evaluate.prescore

        let rec alphaBetaNegaMax (table: Transposition.Table) board hash alpha beta depth =
            let hashing = 
                hash
                |> Option.defaultValue (Transposition.encodeBoard table.Generator board)

            let previousEval = Transposition.tryLookup table hashing
            let previousDepth = 
                match previousEval with 
                | Some eval -> eval.Depth
                | None -> -1
            
            if depth < previousDepth then 
                // printfn "Transposition Hit!"
                previousEval
                |> Option.map (fun entry -> entry.Score)
                |> Option.defaultValue System.Int32.MinValue
            elif depth = 0 then Evaluate.evaluate board
            else

            // evilNumber <- evilNumber + 1

            let rec loopThroughAllMoves board alpha beta bestValue moveList =
                match moveList with
                | move :: rest -> 
                    let newBoard = Move.move board move
                    let newHash: uint64 = Transposition.encodeMove table.Generator hashing board newBoard move

                    let score = - alphaBetaNegaMax table newBoard (Some newHash) -beta -alpha (depth - 1)
                    // printfn "%s %s %i" (String.replicate (6 - depth) "  ") (Move.toString move) score
                    let newBest = max score bestValue
                    let newAlpha = max score alpha

                    let previousEval = Transposition.tryLookup table newHash;
                    let previousDepth = 
                        match previousEval with 
                        | Some eval -> eval.Depth
                        | None -> -1

                    if previousDepth < depth - 1 then
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
            |> sortMoves
            |> fun list -> 
                if List.length list > 0 then loopThroughAllMoves board alpha beta System.Int32.MinValue list
                else -1200000000 - depth

        let findBestMove transposition board depth =
            // evilNumber <- 0
            let rec loopThroughAllMoves board alpha beta bestValue bestMove moveList =
                match moveList with
                | move :: rest -> 
                    let score = - alphaBetaNegaMax transposition (Move.move board move) None -beta -alpha depth
                    // printfn "%s %s %i" (String.replicate 0 "  ") (Move.toString move) score
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
            // |> fun x -> printfn "Positions searched: %i" evilNumber; x

        let rec countValidMoves board depth =
            if  depth = 0 then 1
            else
            
            Generate.allValidMoves board
            |> fun l -> 
                // List.iter (Move.toDirectString >> printfn "    %s") l
                l
            |> List.sumBy (fun move -> 
                countValidMoves (Move.move board move) (depth - 1)
            )
namespace SpellChess.Chess
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

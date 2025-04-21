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
            fun newLocation -> {
                Piece = piece
                Source = location
                Target = newLocation
                Flags = 
                    match board.Board.[newLocation] with
                    | 0uy -> Normal
                    | piece -> Capture piece            
            }
            |> List.map

        let rec private findCapture board targetPiece position depth (d_file, d_rank) =
            let new_rank = position / 8 + d_rank
            let new_file = position % 8 + d_file

            if new_rank > 7 || new_rank < 0 || new_file > 7 || new_file < 0 || depth = 0 then false
            else

            match new_rank * 8 + new_file with
            | n when Piece.color board.Board.[n] = Color.Nil -> findCapture board targetPiece n (depth - 1) (d_file, d_rank)
            | n when board.Board[n] = targetPiece -> true
            | _ -> false
        
        let rec private squaresInDirection board start position depth list (d_file, d_rank) =
            let new_rank = (position >>> 3) + d_rank
            let new_file = (position &&& 7) + d_file

            if new_rank > 7 || new_rank < 0 || new_file > 7 || new_file < 0 || depth = 0 then list
            else

            let color = Piece.color board.Board.[start]

            match (new_rank <<< 3) + new_file with
            | n when Piece.color board.Board.[n] = Color.Nil -> 
                squaresInDirection board start n (depth - 1) (n :: list) (d_file, d_rank)
            | n when Piece.color board.Board.[n] = color -> list
            | n -> n :: list

        let private linearMoves (board: Board) (location: int) (piece: Piece) =
            let depth, list = 
                match enum (int piece &&& 7) with
                | PieceType.King -> 1, queenDirections
                | PieceType.Queen -> 8, queenDirections
                | PieceType.Rook -> 8, rookDirections
                | PieceType.Bishop -> 8, bishopDirections
                | PieceType.Knight -> 1, knightDirections
                | _ -> 0, []

            list
            |> List.fold (squaresInDirection board location location depth) []
            |> generateNormalsAndCaptures board location piece

        let private castlingMoves (board: Board) (location: int): Move list =
            let kingSide, queenSide = (Board.getPlayer board).Castling

            let castleKingSide = 
                kingSide 
                && board.Board.[location + 1] = 0uy 
                && board.Board.[location + 2] = 0uy
            let castleQueenSide = 
                queenSide 
                && board.Board.[location - 1] = 0uy
                && board.Board.[location - 2] = 0uy
                && board.Board.[location - 3] = 0uy

            []
            |> fun list -> if not castleKingSide 
                                        then list 
                                        else {
                                            Piece = board.Board.[location];
                                            Source = location; Target = location + 2;
                                            Flags = Castle KingSide} :: list
            |> fun list -> if not castleQueenSide 
                                        then list 
                                        else {
                                            Piece = board.Board.[location];
                                            Source = location; Target = location - 2;
                                            Flags = Castle QueenSide} :: list

        let private generatePromotionsForMove (move: Move) =
            match move.Flags with
            | Normal -> 
                [
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Queen)}
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Rook)}
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Bishop)}
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Knight)}
                ]
            | Capture target -> 
                [
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Queen)}
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Rook)}
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Bishop)}
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Knight)}
                ]
            | _ -> [move]

        let rec private generatePromotions (generatedList: Move list) (moves: Move list) = 
            match moves with
            | [] -> generatedList
            | move::[] -> List.append generatedList (generatePromotionsForMove move)
            | move::rest -> generatePromotions (List.append generatedList (generatePromotionsForMove move)) rest

        let private checkThenGeneratePromotions (board: Board) (location: int) (moves: Move list) =
            let promotionRank = 
                match board.ActiveColor with
                | Color.White -> 6
                | Color.Black -> 1
                | _ -> -1
            
            if location >>> 3 <> promotionRank then moves
            else 
            generatePromotions [] moves

        let private pawnMoves (board: Board) (location: int) =  
            let pawn = board.Board.[location]
            let enemyColor = Color.oppositeColor board.ActiveColor

            let direction, doubleRank, enPassantRank = 
                match board.ActiveColor with
                | Color.White -> 1, 1, 5
                | Color.Black -> -1, 1, 4
                | _ -> 0, -1, -1


            let rank = location >>> 3
            let file = location &&& 7

            let forward = location + direction*8
            let leftAttack = forward - 1
            let rightAttack = forward + 1
            let forward2 = forward + direction*8

            let canMoveForward = board.Board.[forward] = 0uy

            let canAttackLeft = 
                file > 0 
                && Piece.color board.Board.[leftAttack] = enemyColor

            let canEnPassantLeft =
                file > 0 
                && rank = enPassantRank
                && Some leftAttack = board.EnPassant
            
            let canAttackRight = 
                file < 7
                && Piece.color board.Board.[rightAttack] = enemyColor

            let canEnPassantRight =
                file < 7 
                && Some rightAttack = board.EnPassant
            
            let canMove2 = 
                doubleRank = rank 
                && canMoveForward
                && board.Board.[forward2] = 0uy

            [canMoveForward, forward; canAttackLeft, leftAttack; canAttackRight, rightAttack; canMove2, forward2]
            |> List.filter (fun (canMove, _) -> canMove)
            |> List.map (fun (_, location) -> location)
            |> generateNormalsAndCaptures board location pawn
            |> checkThenGeneratePromotions board location
            |> fun list -> if not canEnPassantLeft 
                                        then list 
                                        else {
                                            Piece = pawn;
                                            Source = location; Target = leftAttack;
                                            Flags = EnPassant} :: list
            |> fun list -> if not canEnPassantRight 
                                        then list 
                                        else {
                                            Piece = pawn;
                                            Source = location; Target = rightAttack;
                                            Flags = EnPassant} :: list

        let private safeSquare (board: Board) (location: int) =
            let opponentColor = Color.oppositeColor board.ActiveColor
            let oPawn = Piece.generate opponentColor PieceType.Pawn
            let oKnight = Piece.generate opponentColor PieceType.Knight
            let oBishop = Piece.generate opponentColor PieceType.Bishop
            let oRook = Piece.generate opponentColor PieceType.Rook
            let oQueen = Piece.generate opponentColor PieceType.Queen
            let oKing = Piece.generate opponentColor PieceType.King

            match board.ActiveColor with
            | Color.White -> 1
            | Color.Black -> -1
            | _ -> 0
            |> fun direction -> 
                board.Board.[location + 7*direction] = oPawn 
                || board.Board.[location + 9*direction] = oPawn
            |> (||) (List.exists (findCapture board oKnight location 1) knightDirections)
            |> (||) (List.exists (findCapture board oBishop location 8) bishopDirections)
            |> (||) (List.exists (findCapture board oRook   location 8) rookDirections)
            |> (||) (List.exists (findCapture board oQueen  location 8) queenDirections)
            |> (||) (List.exists (findCapture board oKing   location 1) queenDirections)
            |> not

        let private noCheck (board: Board) =
            fun (move: Move) -> 
                let nextBoard = Move.move board move

                let testSquares  = 
                    match move.Flags with
                    | Castle KingSide | Castle QueenSide -> [move.Source..move.Target]
                    | _ -> [(Board.getPlayer board).KingLocation]

                testSquares
                |> List.forall (safeSquare nextBoard) 
            |> List.filter
        
        let validMoves (board: Board) (intLoc: int) =
            let piece = board.Board.[intLoc]
            match Piece.pieceType piece with
            | PieceType.Queen | PieceType.Rook | PieceType.Bishop | PieceType.Knight -> linearMoves board intLoc piece
            | PieceType.King -> 
                List.append 
                    (linearMoves board intLoc piece) 
                    (castlingMoves board intLoc)
            // | Piece.Knight -> otherMoves board intLoc piece
            | PieceType.Pawn -> pawnMoves board intLoc
            | _ -> []
            |> noCheck board

        let allValidMoves (board: Board) =
            fun (index: int) ->
                if Piece.color board.Board.[index] = board.ActiveColor then index else -1
            |> List.init 64
            |> List.filter (fun i -> i >= 0)
            |> List.fold (fun list loc -> List.append list (validMoves board loc)) []

    module Evaluate = 
        let private pieceValue piece =
            match piece with
            | PieceType.Queen -> 9
            | PieceType.Rook -> 5
            | PieceType.Bishop -> 3
            | PieceType.Knight -> 3
            | PieceType.Pawn -> 1
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


        let determineMate board = 
            Generate.allValidMoves board
            |> List.length
            |> (=) 0

        let evaluate (board: Board) =
            if determineMate board then -10000000
            else 
            let white, black = boardValue board.Board

            match board.ActiveColor with
            | Color.White -> white - black
            | Color.Black -> black - white
            | _ -> -3000

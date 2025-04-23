namespace SpellChess.Chess
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

        let toDirectString (move: Move) =
            let piece = Piece.toString move.Piece

            let sourceSquare = 
                string (char (move.Source &&& 7) + 'a') + string ((move.Source >>> 3) + 1)

            let targetSquare = 
                string (char (move.Target &&& 7) + 'a') + string ((move.Target >>> 3) + 1)
                // string move.Target
            
            $"{sourceSquare} {targetSquare}"

        let fromString (source: string) (target: string) (promotion: PieceType option) board =
            let sourceInt = 
                Location.fromString source
                |> Option.defaultValue (File.A, Rank.One)
                |> Location.toInt
            
            let targetInt = 
                Location.fromString target
                |> Option.defaultValue (File.A, Rank.One)
                |> Location.toInt

            let piece = board.Board.[sourceInt]
            let targetPiece = board.Board.[targetInt]

            let moveType =
                match Piece.pieceType piece, targetPiece, promotion with
                | PieceType.Pawn, 0uy, None when sourceInt - targetInt &&& 7 <> 0 -> EnPassant
                | PieceType.Pawn, 0uy, Some promotion -> Promotion (Piece.promote piece promotion)
                | _, 0uy, _ -> Normal
                | PieceType.Pawn, target, Some promotion -> CapturePromotion (target, Piece.promote piece promotion)
                | _, target, _ -> Capture target

            {
                Source = sourceInt
                Target = targetInt
                Piece = piece
                Flags = moveType
            }


        let private normalMove (board: Board) (move: Move) =
            let newBoardArray = Array.copy board.Board
            
            Array.get newBoardArray move.Source
            |> Array.set newBoardArray move.Target
            Array.set newBoardArray move.Source 0uy
            
            {board with Board = newBoardArray}

        let private castle (move: Move) (direction: CastleDirection) (board: Board) = 
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
            
            Array.get board.Board rookLocation //This is okay because the cloning happens in the normal move
            |> Array.set board.Board newRookLocation
            Array.set board.Board rookLocation 0uy

            board

        let private enPassant (move: Move) (board: Board) = 
            //Okay again due to the normal move
            Array.set board.Board (move.Source >>> 3 <<< 3 ||| (move.Target &&& 7)) 0uy
            board

        let private promote (move: Move) (pieceType: Piece) (board: Board) = 
            Array.set board.Board move.Target pieceType //See above on why this is okay
            board

        let private updateKing move board =
            if Piece.pieceType move.Piece <> PieceType.King then board
            else
            {Board.getPlayer board with KingLocation = move.Target}
            |> Board.setPlayer <| board

        let private updateEnPassant move board =
            let enPassant = 
                if Piece.pieceType move.Piece <> PieceType.Pawn || abs (move.Source - move.Target) <> 16 then None
                else Some move.Target
            {board with EnPassant = enPassant}

        let private invalidateCastling move board =
            let kingMove = Piece.pieceType move.Piece = PieceType.King
            let baseRookMove = 
                match board.ActiveColor with
                | Color.White -> move.Source = 0 || move.Source = 7
                | Color.Black -> move.Source = 56 || move.Source = 63
                | _ -> false

            let baseRookCapture = 
                match board.ActiveColor with
                | Color.White -> move.Target = 56 || move.Target = 63
                | Color.Black -> move.Target = 0 || move.Target = 7
                | _ -> false

            if not baseRookCapture && not kingMove && not baseRookMove then board
            else

            let kingSideMove = move.Source &&& 7 = 7
            let kingSideCapture = move.Target &&& 7 = 7

            let player = Board.getPlayer board
            let opponent = Board.getOpponent board

            let playerKing, playerQueen = player.Castling
            let opponentKing, opponentQueen = opponent.Castling

            let newPlayer = {
                player 
                with Castling = if kingMove then false, false
                                elif baseRookMove && kingSideMove then false, playerQueen
                                elif baseRookMove then playerKing, false
                                else playerKing, playerQueen
            }

            let newOpponent = {
                opponent 
                with Castling = if baseRookCapture && kingSideCapture then false, opponentQueen
                                elif baseRookCapture then opponentKing, false
                                else opponentKing, opponentQueen
            }

            Board.setPlayer newPlayer board
            |> Board.setOpponent newOpponent

        let private nextPlayer (board: Board) =
            let nextPlayer = 
                match board.ActiveColor with
                | Color.White -> Color.Black
                | Color.Black -> Color.White
                | _ -> Color.Black

            {board with ActiveColor = nextPlayer}

        let private fiftyMoveRule move board = 
            match move.Flags with
            | Capture _ -> true
            | _ -> false
            |> fun capture -> 
                if capture || move.Piece &&& 7uy = byte PieceType.Pawn then {board with HalfmoveCount = 0}
                else {board with HalfmoveCount = board.HalfmoveCount + 1}

        let move (board: Board) (move: Move) = 
            match move.Flags with
            | Normal | Capture _ -> normalMove board move
            | Castle direction -> 
                normalMove board move
                |> castle move direction
            | EnPassant -> 
                normalMove board move
                |> enPassant move
            | Promotion pieceType -> 
                normalMove board move
                |> promote move pieceType
            | CapturePromotion (_, pieceType) -> 
                normalMove board move
                |> promote move pieceType
            |> updateKing move
            |> updateEnPassant move
            |> invalidateCastling move
            |> nextPlayer
            |> fiftyMoveRule move

    module Generate =
        let rookDirections = [1, 0; -1, 0; 0, 1; 0, -1]
        let bishopDirections = [1, 1; -1, 1; -1, -1; 1, -1]
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
                | Color.Black -> -1, 6, 4
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
                && Some (location - 1) = board.EnPassant
            
            let canAttackRight = 
                file < 7
                && Piece.color board.Board.[rightAttack] = enemyColor

            let canEnPassantRight =
                file < 7 
                && Some (location + 1) = board.EnPassant
            
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

        let private findPawnCapture board targetPiece location =
            let direction: int = 
                match board.ActiveColor with
                | Color.White -> -1
                | Color.Black -> 1
                | _ -> 0

            let file = location &&& 7

            let forward = location + direction*8
            let leftAttack = forward - 1
            let rightAttack = forward + 1

            let canAttackLeft = 
                file > 0 
                && leftAttack >= 0
                && leftAttack < 64
                && board.Board.[leftAttack] = targetPiece
            
            let canAttackRight = 
                file < 7
                && rightAttack >= 0
                && rightAttack < 64
                && board.Board.[rightAttack] = targetPiece

            canAttackLeft || canAttackRight

        let safeSquare (board: Board) (location: int) =
            let oPawn = Piece.generate board.ActiveColor PieceType.Pawn
            let oKnight = Piece.generate board.ActiveColor PieceType.Knight
            let oBishop = Piece.generate board.ActiveColor PieceType.Bishop
            let oRook = Piece.generate board.ActiveColor PieceType.Rook
            let oQueen = Piece.generate board.ActiveColor PieceType.Queen
            let oKing = Piece.generate board.ActiveColor PieceType.King


            findPawnCapture board oPawn location
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
                    | Castle KingSide -> [move.Source..move.Target]
                    | Castle QueenSide -> [move.Target..move.Source]
                    | _ -> [(Board.getOpponent nextBoard).KingLocation]

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

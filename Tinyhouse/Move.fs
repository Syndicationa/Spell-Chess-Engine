namespace SpellChess.Tinyhouse
    type MoveType =
        | Normal
        | Capture of Piece
        | Promotion of Piece
        | CapturePromotion of Piece * Piece
        | Place

    type Move = {
        Piece: Piece
        Source: int
        Target: int
        Flags: MoveType
    }

    module Move =
        let toString (move: Move) =
            let piece = Piece.toString move.Piece

            let targetSquare = 
                string (char (move.Target &&& 3) + 'a') + string ((move.Target >>> 2) + 1)
            
            match move.Flags with
            | Normal -> piece + targetSquare
            | Capture _ -> piece + "x" + targetSquare
            | Promotion target -> piece + targetSquare + Piece.toString target
            | CapturePromotion (_,target) -> piece + "x" + targetSquare + Piece.toString target
            | Place -> piece + targetSquare

        let toDirectString (move: Move) =
            let sourceSquare = 
                string (char (move.Source &&& 3) + 'a') + string ((move.Source >>> 2) + 1)

            let targetSquare = 
                string (char (move.Target &&& 3) + 'a') + string ((move.Target >>> 2) + 1)
            
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
        let private promote (move: Move) (pieceType: Piece) (board: Board) = 
            Array.set board.Board move.Target pieceType
            board

        let private updateKing move board =
            if Piece.pieceType move.Piece <> PieceType.King then board
            else
            {Board.getPlayer board with KingLocation = move.Target}
            |> Board.setPlayer <| board

        let private updatePlaceables move board =
            match move.Flags with
            | Normal -> board
            | Promotion _ -> board
            | Capture taken | CapturePromotion (taken, _) ->
                let oldPlayer = Board.getPlayer board
                let player = {oldPlayer with Placeables = Array.copy oldPlayer.Placeables}
                let index = int (Piece.originalType taken ) - 2
                if index < 0 then 
                    printfn "Something bad happened %s %s" (toString move) (Piece.toString taken)
                    board
                else 
                printfn "Index: %i" index
                Array.get player.Placeables index
                |> (+) 1
                |> Array.set player.Placeables index
                Board.setPlayer player board
            | Place -> 
                let oldPlayer = Board.getPlayer board
                let player = {oldPlayer with Placeables = Array.copy oldPlayer.Placeables}

                let index = int (Piece.pieceType move.Piece) - 1
                Array.get player.Placeables index
                |> (-) 1
                |> Array.set player.Placeables index

                Board.setPlayer player board

        let private nextPlayer (board: Board) =
            let nextPlayer, moveCount = 
                match board.ActiveColor with
                | Color.White -> Color.Black, board.MoveCount
                | Color.Black -> Color.White, board.MoveCount + 1
                | _ -> Color.Black, -1

            {board with ActiveColor = nextPlayer; MoveCount = moveCount}

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
            | Promotion pieceType -> 
                normalMove board move
                |> promote move pieceType
            | CapturePromotion (_, pieceType) -> 
                normalMove board move
                |> promote move pieceType
            | Place ->
                normalMove board move
                |> promote move move.Piece
            |> updateKing move
            |> updatePlaceables move
            |> nextPlayer
            |> fiftyMoveRule move

    module Generate =
        let wazirDirections = [1, 0; -1, 0; 0, 1; 0, -1]
        let ferzDirections = [1, 1; -1, 1; -1, -1; 1, -1]
        let kingDirections = List.append wazirDirections ferzDirections

        let xiangqiDirectionPairs = [
            (1, 0), [1, 1; 1, -1]
            (-1, 0), [-1, 1; -1, -1]
            (0, 1), [1, 1; -1, 1]
            (0, -1), [-1, -1; 1, -1]
        ]

        let invertedXiangqiDirectionPairs = [
            (1, 1), [1, 0; 0, 1]
            (-1, 1), [-1, 0; 0, 1]
            (-1, -1), [-1, 0; 0, -1]
            (1, -1), [1, 0; 0, -1]
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
            let new_rank = (position >>> 2) + d_rank
            let new_file = (position &&& 3) + d_file

            if new_rank > 3 || new_rank < 0 || new_file > 3 || new_file < 0 || depth = 0 then false
            else

            match (new_rank <<< 2) + new_file with
            | n when Piece.color board.Board.[n] = Color.Nil -> findCapture board targetPiece n (depth - 1) (d_file, d_rank)
            | n when board.Board[n] = targetPiece -> true
            | _ -> false
        
        let rec private squaresInDirection board position list (d_file, d_rank) =
            let new_rank = (position >>> 2) + d_rank
            let new_file = (position &&& 3) + d_file

            if new_rank > 3 || new_rank < 0 || new_file > 3 || new_file < 0 then list
            else

            let color = board.ActiveColor

            match (new_rank <<< 2) + new_file with
            | n when Piece.color board.Board.[n] = color -> list
            | n -> n :: list

        let private linearMoves (board: Board) (location: int) (piece: Piece) =
            let list = 
                match enum (int piece &&& 7) with
                | PieceType.King -> kingDirections
                | PieceType.Wazir -> wazirDirections
                | PieceType.Ferz -> ferzDirections
                | _ -> []

            list
            |> List.fold (squaresInDirection board location) []
            |> generateNormalsAndCaptures board location piece

        let private xiangqiMoves (board: Board) (location: int) (piece: Piece): Move list =
            xiangqiDirectionPairs
            |> List.fold (fun list ((df, dr), nextDirections) -> 
                let new_rank = (location >>> 2) + dr
                let new_file = (location &&& 3) + df

                if new_rank > 3 || new_rank < 0 || new_file > 3 || new_file < 0 then list
                else

                match (new_rank <<< 2) + new_file with
                | n when Piece.color board.Board.[n] = Color.Nil -> 
                    nextDirections
                    |> List.fold (squaresInDirection board n) list
                    |> fun x -> printfn "%i" (List.length x);x
                | _ -> list

            ) []
            |> generateNormalsAndCaptures board location piece

        let private xiangqiCaptures (board: Board) (location: int): bool =
            let opponentXiangqi = Piece.generate board.ActiveColor PieceType.Xiangqi
            
            invertedXiangqiDirectionPairs
            |> List.exists (fun ((df, dr), nextDirections) -> 
                let new_rank = (location >>> 2) + dr
                let new_file = (location &&& 3) + df

                if new_rank > 3 || new_rank < 0 || new_file > 3 || new_file < 0 then false
                else

                match (new_rank <<< 2) + new_file with
                | n when Piece.color board.Board.[n] = Color.Nil -> 
                    nextDirections
                    |> List.exists (findCapture board opponentXiangqi n 1)
                | _ -> false
            )

        let private generatePromotionsForMove (move: Move) =
            match move.Flags with
            | Normal -> 
                [
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Wazir)}
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Ferz)}
                    {move with Flags = Promotion (Piece.promote move.Piece PieceType.Xiangqi)}
                ]
            | Capture target -> 
                [
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Wazir)}
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Ferz)}
                    {move with Flags = CapturePromotion (target, Piece.promote move.Piece PieceType.Xiangqi)}
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

            let direction = 
                match board.ActiveColor with
                | Color.White -> 1
                | Color.Black -> -1
                | _ -> 0

            let file = location &&& 7

            let forward = location + direction*4
            let leftAttack = forward - 1
            let rightAttack = forward + 1

            let canMoveForward = board.Board.[forward] = 0uy

            let canAttackLeft = 
                file > 0 
                && Piece.color board.Board.[leftAttack] = enemyColor
            
            let canAttackRight = 
                file < 3
                && Piece.color board.Board.[rightAttack] = enemyColor
            
            [canMoveForward, forward; canAttackLeft, leftAttack; canAttackRight, rightAttack]
            |> List.filter (fun (canMove, _) -> canMove)
            |> List.map (fun (_, location) -> location)
            |> generateNormalsAndCaptures board location pawn
            |> checkThenGeneratePromotions board location

        let private findPawnCapture board targetPiece location =
            let direction: int = 
                match board.ActiveColor with
                | Color.White -> -1
                | Color.Black -> 1
                | _ -> 0

            let file = location &&& 3

            let forward = location + direction*4
            let leftAttack = forward - 1
            let rightAttack = forward + 1

            let canAttackLeft = 
                file > 0
                && leftAttack >= 0
                && leftAttack < 16
                && board.Board.[leftAttack] = targetPiece
            
            let canAttackRight = 
                file < 3
                && rightAttack >= 0
                && rightAttack < 16
                && board.Board.[rightAttack] = targetPiece

            canAttackLeft || canAttackRight

        let private placePieces board (location: int) =
            (Board.getPlayer board).Placeables
            |> Array.mapi (fun index count -> 
                    if count = 0 then None
                    else 
                    let piece = Piece.generate board.ActiveColor (enum (index + 2))

                    let invalidPawnRank = 
                        match board.ActiveColor with
                        | Color.White -> 3
                        | _ -> 0
                    
                    if enum (index + 2) = PieceType.Pawn && location >>> 2 = invalidPawnRank then None
                    else Some {
                        Piece = piece
                        Source = location
                        Target = location
                        Flags = Place
                    }
                )
            |> Array.toList
            |> List.filter Option.isSome
            |> List.map Option.get

        let safeSquare (board: Board) (location: int) =
            let oPawn = Piece.generate board.ActiveColor PieceType.Pawn
            let oFerz = Piece.generate board.ActiveColor PieceType.Ferz
            let oWazir = Piece.generate board.ActiveColor PieceType.Wazir
            let oKing = Piece.generate board.ActiveColor PieceType.King


            findPawnCapture board oPawn location
            |> (||) (xiangqiCaptures board location)
            |> (||) (List.exists (findCapture board oFerz location 1) ferzDirections)
            |> (||) (List.exists (findCapture board oWazir location 1) wazirDirections)
            |> (||) (List.exists (findCapture board oKing   location 1) kingDirections)
            |> not

        let private noCheck (board: Board) =
            fun (move: Move) -> 
                let nextBoard = Move.move board move
                safeSquare nextBoard (Board.getOpponent nextBoard).KingLocation 
            |> List.filter
        
        let validMoves (board: Board) (intLoc: int) =
            let piece = board.Board.[intLoc]
            match Piece.pieceType piece with
            | PieceType.King | PieceType.Wazir | PieceType.Ferz -> linearMoves board intLoc piece
            | PieceType.Xiangqi -> xiangqiMoves board intLoc piece
            | PieceType.Pawn -> pawnMoves board intLoc
            | _ -> placePieces board intLoc
            |> noCheck board

        let allValidMoves (board: Board) =
            fun (index: int) ->
                if Piece.color board.Board.[index] = Color.oppositeColor board.ActiveColor then -1 else index
            |> List.init 16
            |> List.filter (fun i -> i >= 0)
            |> List.fold (fun list loc -> List.append list (validMoves board loc)) []

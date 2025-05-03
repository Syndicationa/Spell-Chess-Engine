namespace SpellChess
module Common =
    let encodingCorrection () = System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let secTup f (a, b) =
            a, f b
        
namespace SpellChess.Chess
    open System.Text.RegularExpressions
    
    module Run =
        let initialBoard = Board.create "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - - 0 1"

        let moveRegex =  Regex "([a-h][1-8])([a-h][1-8])(?:=([QRBN]))?"

        let private piece (str: string) =
            match str with
            | "K" | "k" -> PieceType.King
            | "Q" | "q" -> PieceType.Queen
            | "R" | "r" -> PieceType.Rook
            | "B" | "b" -> PieceType.Bishop
            | "N" | "n" -> PieceType.Knight
            | "" -> PieceType.Pawn
            | _ -> PieceType.Pawn

        let private kingSideCastle (human: Color): Move = 
            let king = match human with
                        | Color.White -> "e1"
                        | Color.Black -> "e8"
                        | _ -> "a1"
                        |> Location.fromString
                        |> Option.map Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Piece.generate human PieceType.King
                Source = king
                Target = king + 2
                Flags = Castle KingSide
            }
        
        let private queenSideCastle (human: Color): Move = 
            let king = match human with
                        | Color.White -> "e1"
                        | Color.Black -> "e8"
                        | _ -> "a1"
                        |> Location.fromString
                        |> Option.map Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Piece.generate human PieceType.King
                Source = king
                Target = king - 2
                Flags = Castle QueenSide
            }

        let private readInput human board str =
            if str = "O-O" then Some (kingSideCastle human)
            elif str = "O-O-O" then Some (queenSideCastle human)
            else
            str
            |> moveRegex.Match
            |> fun m -> [for g in m.Groups do g.Value]
            |> fun list -> 
                match list with
                | [move; source; target; promotion] ->
                    let promotionType = 
                        match piece promotion with
                        | PieceType.Pawn -> None
                        | value -> Some value
                    Some (Move.fromString source target promotionType board)
                | _ -> None

        let rec private temporaryGameLoop human (transposition: TranspositionTable, board) =
            if List.length (Generate.allValidMoves board) = 0 then "Game Over\n" + Board.toString board else
            printfn "Size of Table %i" transposition.Table.Count

            if board.ActiveColor = human then
                Board.toString board
                |> printfn "%s"

                // Board.toFENstring board
                // |> printfn "%s"

                System.Console.ReadLine()
                |> readInput human board
                |> Option.map (Move.move board)
                |> Option.defaultValue board
                |> fun x -> transposition, x
                //Do Human Stuff
            else
                let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                Search.findBestMove transposition board 1000
                |> fun x -> 
                    stopwatch.Stop()
                    printfn "Move took: %i" stopwatch.ElapsedMilliseconds
                    x
                |> SpellChess.Common.secTup (fun (score, m) -> printfn "Black played: %s scored at %i" (Move.toString m) score; m)
                |> SpellChess.Common.secTup (Move.move board)
            |> temporaryGameLoop human

        let humanContest color =
            let transpositionTable = Transposition.createTable 43816uL
            temporaryGameLoop color (transpositionTable, initialBoard)
            |> printfn "%s"

namespace SpellChess.Tinyhouse
    open System.Text.RegularExpressions
    
    module Run =
        let initialBoard = Board.create "fuwk/3p/P3/KWUF w - - 0 1"
        
        let moveRegex =  Regex "([a-d][1-4])([a-d][1-4])(?:=([WUFPwufp]))?"

        let private piece (str: string) =
            match str with
            | "K" | "k" -> PieceType.King
            | "W" | "w" -> PieceType.Wazir
            | "F" | "f" -> PieceType.Ferz
            | "U" | "u" -> PieceType.Xiangqi
            | "P" | "p" -> PieceType.Pawn
            | _ -> PieceType.Nil

        let private readInput board str =
            if str = "list-count" then 
                Generate.allValidMoves board
                |> List.length
                |> printfn "Move Count: %i"
            elif str = "list" then
                Generate.allValidMoves board
                |> List.iteri (fun index x -> printfn "%i: Move %s aka %s" index (Move.toString x) (Move.toDirectString x))
            elif str = "inv" then
                (Board.getPlayer board).Placeables
                |> Array.iteri (fun index count -> printfn "%s: %i" (Piece.toString (Piece.generate board.ActiveColor (enum (index + 2)))) count)
            elif str = "onv" then
                (Board.getOpponent board).Placeables
                |> Array.iteri (fun index count -> printfn "%s: %i" (Piece.toString (Piece.generate board.ActiveColor (enum (index + 2)))) count)
            str
            |> moveRegex.Match
            |> fun m -> [for g in m.Groups do g.Value]
            |> fun list -> 
                match list with
                | [move; source; target; promotion] ->
                    let promotionType = 
                        match piece promotion with
                        | PieceType.Nil -> None
                        | value -> Some value
                    Some (Move.fromString source target promotionType board)
                | _ -> None

        let rec private temporaryGameLoop human config (transposition: TranspositionTable, board) =
            if List.length (Generate.allValidMoves board) = 0 then "Game Over\n" + Board.toString board else
            printfn "Size of Table %i" transposition.Table.Count

            if board.ActiveColor = human then
                Board.toString board
                |> printfn "%s"

                System.Console.ReadLine()
                |> readInput board
                |> fun x -> 
                    Option.iter (fun (x: Move) -> 
                        printfn "Piece: %i Source: %i Target: %i" x.Piece x.Source x.Target
                    ) x
                    x
                |> Option.map (Move.move board)
                |> Option.defaultValue board
                |> fun x -> transposition, x
                //Do Human Stuff
            else
                let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                Search.findBestMove config transposition board 10000
                |> fun x -> 
                    stopwatch.Stop()
                    printfn "Move took: %i" stopwatch.ElapsedMilliseconds
                    x
                |> SpellChess.Common.secTup (fun (score, m) -> printfn "Computer played: %s aka %s scored at %i" (Move.toString m) (Move.toDirectString m) score; m)
                |> SpellChess.Common.secTup (Move.move board)
            |> temporaryGameLoop human config

        let humanContest color =
            let transpositionTable = Transposition.createTable 4381645632uL

            let config = {
                Wazir = 40
                Ferz = 25
                Xiangqi = 15
                Pawn = 10
                Inventory = fun x -> x / 2
            }

            temporaryGameLoop color config (transpositionTable, initialBoard)
            |> printfn "%s"

        let configA = {
            Wazir = 10
            Ferz = 30
            Xiangqi = 10
            Pawn = 10
            Inventory = fun x -> x * 2
        }

        let configB = {
            Wazir = 10
            Ferz = 10
            Xiangqi = 10
            Pawn = 10
            Inventory = fun x -> x * 2
        }

        type outcome = 
        | Win
        | Draw
        | Loss

        let add (w,d,l) = function
        | Win -> w + 1, d, l
        | Draw -> w, d + 1, l
        | Loss -> w, d, l + 1

        let rec private computerLoop a (transpositionA: TranspositionTable, transpositionB, board) =
            if Evaluate.determineMate board then
                if board.ActiveColor = a then Loss else Win
            elif Evaluate.determineStalemate board then
                Draw
            else

            // printfn "Count: %i" board.MoveCount

            let config = if a = board.ActiveColor then configA else configB
            // let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            Search.findBestAtDepth config transpositionA board 2
            |> SpellChess.Common.secTup snd
            |> SpellChess.Common.secTup (Move.move board)
            |> (fun (a, b) -> transpositionB, a, b)
            |> computerLoop a

        let computerContest () =
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()

            let list =  List.init 100 (fun index -> Transposition.Random.generate (uint64 stopwatch.ElapsedMilliseconds + uint64 index + 1uL))

            stopwatch.Stop()
            let mutable count = 0
            let percent = List.length list / 10

            let win, draw, loss = 
                List.fold (fun total seed -> 
                    count <- count + 1

                    // stopwatch.Restart()
                    let transpositionA = Transposition.createTable seed
                    let transpositionB = Transposition.createTable seed

                    let whiteTotal = 
                        computerLoop Color.White (transpositionA,transpositionB, initialBoard)
                        |> add total

                    // stopwatch.Stop()
                    // printfn "\nGame a done\n"

                    transpositionA.Table.Clear()
                    transpositionB.Table.Clear()

                    let ret =
                        computerLoop Color.Black (transpositionB, transpositionA, initialBoard)
                        |> add whiteTotal

                    if count % percent = 0 then printfn "Completed: %i%%" (count*10 / percent)

                    ret
                ) (0,0,0) list

            printfn "W: %i D: %i L: %i" win draw loss
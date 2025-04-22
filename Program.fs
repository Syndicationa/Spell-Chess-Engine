namespace SpellChess

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Spell Chess"
        base.Content <- Main.view ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =
    open System.Text.RegularExpressions

    [<EntryPoint>]
    let main(args: string[]) =
        System.Console.OutputEncoding <- System.Text.Encoding.UTF8
        
        let board = Chess.Board.create "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let moveRegex =  Regex "([a-h][1-8])([a-h][1-8])(?:=([QRBN]))?"

        let piece (str: string) =
            match str with
            | "K" | "k" -> Chess.PieceType.King
            | "Q" | "q" -> Chess.PieceType.Queen
            | "R" | "r" -> Chess.PieceType.Rook
            | "B" | "b" -> Chess.PieceType.Bishop
            | "N" | "n" -> Chess.PieceType.Knight
            | "" -> Chess.PieceType.Pawn
            | _ -> Chess.PieceType.Pawn

        let kingSideCastle (human: Chess.Color): Chess.Move = 
            let king = match human with
                        | Chess.Color.White -> "e1"
                        | Chess.Color.Black -> "e8"
                        | _ -> "a1"
                        |> Chess.Location.fromString
                        |> Option.map Chess.Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Chess.Piece.generate human Chess.PieceType.King
                Source = king
                Target = king + 2
                Flags = Chess.MoveType.Castle Chess.CastleDirection.KingSide
            }
        
        let queenSideCastle (human: Chess.Color): Chess.Move = 
            let king = match human with
                        | Chess.Color.White -> "e1"
                        | Chess.Color.Black -> "e8"
                        | _ -> "a1"
                        |> Chess.Location.fromString
                        |> Option.map Chess.Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Chess.Piece.generate human Chess.PieceType.King
                Source = king
                Target = king - 2
                Flags = Chess.MoveType.Castle Chess.CastleDirection.QueenSide
            }

        let secTup f (a, b) =
            a, f b

        let readInput human board str =
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
                        | Chess.PieceType.Pawn -> None
                        | value -> Some value
                    Some (Chess.Move.fromString source target promotionType board)
                | _ -> None

        let rec temporaryGameLoop human (transposition: Chess.Transposition.Table, board) =
            if Chess.Evaluate.determineMate board then "Game Over\n" + Chess.Board.toString board else

            if board.ActiveColor = human then
                Chess.Board.toString board
                |> printfn "%s"

                System.Console.ReadLine()
                |> readInput human board
                |> Option.map (Chess.Move.move board)
                |> Option.defaultValue board
                |> fun x -> transposition, x
                //Do Human Stuff
            else
                Chess.Search.findBestMove transposition board 4
                |> secTup (secTup (fun move -> 
                    move
                    |> Option.defaultValue {Piece = 0uy; Source = -1; Target = -1; Flags = Chess.MoveType.Normal}))
                |> secTup (fun (score, m) -> printfn "Black played: %s scored at %i" (Chess.Move.toString m) score; m)
                |> secTup (Chess.Move.move board)
            |> temporaryGameLoop human

        let transpositionTable = Chess.Transposition.createTable 30241uL

        temporaryGameLoop Chess.Color.Black (transpositionTable, board)
        |> printfn "%s"

        0
        // AppBuilder
        //     .Configure<App>()
        //     .UsePlatformDetect()
        //     .UseSkia()
        //     .StartWithClassicDesktopLifetime(args)

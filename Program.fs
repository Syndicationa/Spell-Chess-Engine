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
        
        // board
        // |> Chess.Board.toString
        // |> printfn "%s"
        
        // Chess.Search.findBestMove board 10
        // |> fun (v, move) -> 
        //     printfn "Score: %i" v
            
        //     move
        //     |> Option.map Chess.Move.toString 
        //     |> Option.defaultValue "None"
        //     |> printfn "Move: %s"

        let moveRegex =  Regex "([KkQqRrBbNn]?)(x?)([a-h][1-8])([a-h][1-8])(?:=([QRBN]))?"

        let piece (str: string) =
            match str with
            | "K" | "k" -> Chess.Piece.King
            | "Q" | "q" -> Chess.Piece.Queen
            | "R" | "r" -> Chess.Piece.Rook
            | "B" | "b" -> Chess.Piece.Bishop
            | "N" | "n" -> Chess.Piece.Knight
            | "" -> Chess.Piece.Pawn
            | _ -> Chess.Piece.Pawn

        let kingSideCastle (human: Chess.Side): Chess.Move = 
            let king = match human with
                        | Chess.Side.White -> "e1"
                        | Chess.Side.Black -> "e8"
                        | _ -> "a1"
                        |> Chess.Location.fromString
                        |> Option.map Chess.Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Chess.Piece.King
                Source = king
                Target = king + 2
                Flags = Chess.MoveType.Castle Chess.CastleDirection.KingSide
            }
        
        let queenSideCastle (human: Chess.Side): Chess.Move = 
            let king = match human with
                        | Chess.Side.White -> "e1"
                        | Chess.Side.Black -> "e8"
                        | _ -> "a1"
                        |> Chess.Location.fromString
                        |> Option.map Chess.Location.toInt
                        |> Option.defaultValue -1
            {
                Piece = Chess.Piece.King
                Source = king
                Target = king - 2
                Flags = Chess.MoveType.Castle Chess.CastleDirection.QueenSide
            }

        let secTup f (a, b) =
            a, f b

        let readInput human str =
            if str = "O-O" then Some (kingSideCastle human)
            elif str = "O-O-O" then Some (queenSideCastle human)
            else
            str
            |> moveRegex.Match
            |> fun m -> [for g in m.Groups do g.Value]
            |> fun list -> 
                match list with
                | [move; pieceLetter; capture; source; target; promotion] ->
                    let Source = Chess.Location.fromString source |> Option.map Chess.Location.toInt |> Option.defaultValue -1
                    let Target = Chess.Location.fromString target |> Option.map Chess.Location.toInt |> Option.defaultValue -1
                    
                    let move: Chess.Move = {
                        Piece = piece pieceLetter
                        Source = Source
                        Target = Target
                        Flags = match capture, promotion with
                                | "", "" -> Chess.MoveType.Normal
                                | "e", "" -> Chess.MoveType.EnPassant
                                | "x", "" -> Chess.MoveType.Capture (Chess.PieceBoards.getPieceAtLocation (Chess.Board.getOpponent board).Pieces Target |> Option.defaultValue Chess.Piece.Pawn)
                                | "", promote -> Chess.MoveType.Promotion (piece promote)
                                | "x", promote -> Chess.MoveType.CapturePromotion (Chess.PieceBoards.getPieceAtLocation (Chess.Board.getOpponent board).Pieces Target |> Option.defaultValue Chess.Piece.Pawn, piece promote)
                                | _ -> Chess.MoveType.Normal
                    }
                    Some move
                | _ -> None

        let rec temporaryGameLoop human (transposition, board) =
            if Chess.Evaluate.determineMate board then "Game Over\n" + Chess.Board.toString board else

            if board.Player = human then
                Chess.Board.toString board
                |> printfn "%s"
                System.Console.ReadLine()
                |> readInput human
                |> Option.map (Chess.Move.move board)
                |> Option.defaultValue board
                |> fun x -> transposition, x
                //Do Human Stuff
            else
                Chess.Search.findBestMove transposition board 4
                |> secTup (fun (v, move) -> move)
                |> secTup (Option.defaultValue {Piece = Chess.Piece.King; Source = -1; Target = -1; Flags = Chess.MoveType.Normal})
                |> secTup (fun m -> printfn "Black played: %s" (Chess.Move.toString m); m)
                |> secTup (Chess.Move.move board)
            |> temporaryGameLoop human

        temporaryGameLoop Chess.Side.White (1, board)
        |> printfn "%s"

        0
        // AppBuilder
        //     .Configure<App>()
        //     .UsePlatformDetect()
        //     .UseSkia()
        //     .StartWithClassicDesktopLifetime(args)

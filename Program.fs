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
        Common.encodingCorrection ()
        
        Tinyhouse.Run.humanContest Tinyhouse.Color.Black

        // let board = Tinyhouse.Board.create "1uw1/1Puk/1pF1/K1W1 w - - 0 0"

        // if board.ActiveColor = Tinyhouse.Color.White then printfn "Yes"

        // Tinyhouse.Evaluate.determineCheck board
        // |> printfn "%b"

        
        0
        // AppBuilder
        //     .Configure<App>()
        //     .UsePlatformDetect()
        //     .UseSkia()
        //     .StartWithClassicDesktopLifetime(args)

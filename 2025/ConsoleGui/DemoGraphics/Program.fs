open System
open ConsoleRenderer
open Types
open Screen
open Mindmagma.Curses

(*

Super-simple Graphics Library for .Net using terminal IO.
For more info, see: https://github.com/NinovanderMark/ConsoleRenderer

In that repository, you will find the following:

For all the methods of ConsoleCanvas, see:
ConsoleRenderer/ConsoleCanvas.cs

For more examples (written in C# - use chatgpt to convert to F# if need be), see:
ConsoleRenderer.Examples/Programs/*.cs


*)

(*
DEMO wanted:
1. Use ReadConsoleInput to read the next Keypress or mouse event
   https://learn.microsoft.com/en-us/windows/console/reading-input-buffer-events
2. Replace the direct ConsoleCanvas output here with the same via the untested but simple Screen.fs library.
   Purpose of this library is to cache screen output and only update the screen when necessary with render
3. Add to Screen a function string -> Model -> Model that draws a string starting at a given row and column into the NextScreen field of Model.
4. Write a simple tail recursive function cliLoop that reads the next keypress, updates the screen, and then calls itself
   until the user presses the ESC key. In addition, if the user clicks the mouse, it should write a * character at the mouse click location. 
5. The function should recurse with Model as its argument. UModel can be a record type with
    a single int field for this demo
6. The integer must be written to the top of the screen, and incrememnted by 1 each time the user presses a key
7. The screen should be rendered once per cliLoop iteration.
*)


type UModel = {
    Counter: int
    LastKey: string
}

type InputEvent =
    | KeyPress of char
    | MouseClick of int * int  // x, y coordinates
    | NoEvent

let drawString (row: int) (col: int) (text: string) (model: Model<'a>) : Model<'a> =
    let folder (model: Model<'a>) (colOffset, ch) =
        let currentRow = 
            match Map.tryFind row model.NextScreen with
            | Some row -> row
            | None -> Map.empty
        let newRow = Map.add (col + colOffset) ch currentRow
        { model with NextScreen = Map.add row newRow model.NextScreen }
    
    text 
    |> Seq.mapi (fun i c -> (i, c))
    |> Seq.fold folder model

let centerText (row: int) (text: string) (model: Model<'a>) : Model<'a> =
    let col = (model.Width - text.Length) / 2
    drawString row col text model

let readNextInput() : InputEvent =
    if Console.KeyAvailable then
        let key = Console.ReadKey(true)
        if key.Key = ConsoleKey.Escape then
            KeyPress('\u001b') // ESC character
        else
            KeyPress(key.KeyChar)
    else
        match NCurses.GetChar() with
        | CursesKey.MOUSE ->
            try
                let mouse = NCurses.GetMouse()
                MouseClick(mouse.x, mouse.y)
            with
            | _ -> NoEvent
        | _ -> NoEvent

let rec cliLoop (model: Model<UModel>) =
    let model = { model with NextScreen = Map.empty }
    
    let model = 
        model
        |> drawString 0 0 $"Counter: {model.UModel.Counter}"
        |> centerText (model.Height / 2) $"Last key pressed: {model.UModel.LastKey}"
        |> drawString (model.Height - 1) 0 "Press any key to increment counter. Press ESC to exit."
        |> render


    match readNextInput() with
    | KeyPress('\u001b') -> // ESC key
        model
    | KeyPress(key) ->
        let newUModel = { 
            Counter = model.UModel.Counter + 1
            LastKey = string key 
        }
        cliLoop { model with UModel = newUModel }
    | MouseClick(x, y) ->
        let model = drawString y x "*" model
        cliLoop model
    | NoEvent ->
        System.Threading.Thread.Sleep(50)
        cliLoop model


[<EntryPoint>]
let main argv =
    let canvas = new ConsoleRenderer.ConsoleCanvas()
    let Screen = NCurses.InitScreen()
    
    let initialState = { 
        ConsoleScreen = Map.empty, canvas
        NextScreen = Map.empty
        Width = canvas.Width
        Height = canvas.Height
        UModel = { 
            Counter = 0
            LastKey = "none"
        }
    }

    let screen = NCurses.InitScreen()
    NCurses.NoDelay(screen, true)
    NCurses.NoEcho()
    NCurses.Keypad(screen, true)
    let eventsToReport = 
        CursesMouseEvent.BUTTON1_CLICKED |||
        CursesMouseEvent.BUTTON2_CLICKED |||
        CursesMouseEvent.REPORT_MOUSE_POSITION

    let availableMouseEvents = NCurses.MouseMask(eventsToReport)


    
    let model = initScreen initialState
    
    let _ = cliLoop model

    NCurses.EndWin()
    
    0

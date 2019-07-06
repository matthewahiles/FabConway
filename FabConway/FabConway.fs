// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace FabConway

open System.Diagnostics
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

module App =

    type Cell =
      | Dead
      | Alive

    type Model = 
      { Generation: Cell[,] }

    type Msg = 
      | Step
      | Flip of int * int
      | Reset

    let initModel = { Generation = array2D [
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
      [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead;];
    ]}

    let calcNeighbors (gen: Cell[,]) x y v =
      let maxY = gen.[0, *].Length - 1
      let maxX = gen.[*, 0].Length - 1
      let count = (seq {
        for i in -1 .. 1 do
          for j in -1 .. 1 do
            let x' = x + i
            let y' = y + j
            if x' >= 0 && y' >= 0 && x' <= maxX && y' <= maxY && (i, j) <> (0, 0) then
              match gen.[x', y'] with
              | Alive -> yield 1
              | Dead -> yield 0
            else yield 0
      } |> Seq.sum)
      match (v, count) with
      | (Alive, 2) -> Alive
      | (Alive, 3) -> Alive
      | (Dead, 3) -> Alive
      | _ -> Dead

    let evolveGen (gen: Cell[,]): Cell[,] =
        gen
        |> Array2D.copy
        |> Array2D.mapi (calcNeighbors gen)

    let flipCell (grid: Cell[,]) x y: Cell[,] =
      let newGen = Array2D.copy grid
      Array2D.set newGen x y (
        match grid.[x, y] with
        | Alive -> Dead
        | Dead -> Alive)
      newGen

    let stepModel gen = { Generation = evolveGen gen }, Cmd.none
    let flipModel gen x y = { Generation = flipCell gen x y}, Cmd.none
    let init () = initModel, Cmd.none

    let makeGrid rows cols (f : (int * int) -> ViewElement) =
        View.Grid(rowdefs=[ for i in 0 .. rows - 1 -> box "auto" ],
            coldefs = [for i in 0 .. cols - 1 -> box "auto" ],
            horizontalOptions = LayoutOptions.Center,
            rowSpacing=0.4,
            columnSpacing=0.4,
            backgroundColor=Color.Black,
            verticalOptions = LayoutOptions.Center,
            children = [for i in 0 .. rows - 1 do
                            for j in 0 .. cols - 1 do
                                yield (f (i, j)).GridRow(i).GridColumn(j) ])

    let update msg model =
        match msg with
        | Reset -> init ()
        | Step -> stepModel model.Generation
        | Flip (x, y) -> flipModel model.Generation x y

    let view (model: Model) dispatch =
        View.ContentPage(
            View.StackLayout([
                 View.Label(text="Conway's Game of Life", horizontalTextAlignment=TextAlignment.Center)
                 makeGrid 16 10 (fun pos ->
                     View.BoxView(
                        widthRequest=40.0,
                        heightRequest=40.0,
                        gestureRecognizers= [
                          View.TapGestureRecognizer(command=(fun _ -> dispatch (Flip pos)))
                        ],
                        color= (
                            let (a, b) = pos
                            match model.Generation.[a, b] with
                               | Alive -> Color.RoyalBlue
                               | Dead -> Color.LightSlateGray)))
                 View.Button(text="Step", command=(fun _ -> dispatch Step))
                 View.Button(text="Reset", command=(fun _ -> dispatch Reset))
            ])
        )

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif



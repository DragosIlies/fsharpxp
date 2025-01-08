module Index

open Elmish
open SAFE
open Shared

type ButtonState =
    | Normal
    | Hover
    | Clicked

type Model = {
    Todos: RemoteData<Todo list>
    Input: string
    StartButtonState: ButtonState
}

type Msg =
    | SetInput of string
    | LoadTodos of ApiCall<unit, Todo list>
    | SaveTodo of ApiCall<string, Todo list>
    | HoverStartButton
    | NormalStartButton
    | ClickStartButton

let todosApi = Api.makeProxy<ITodosApi> ()

let init () =
    let initialModel = { Todos = NotStarted; Input = ""; StartButtonState = Normal }
    let initialCmd = LoadTodos(Start()) |> Cmd.ofMsg

    initialModel, initialCmd

let update msg model =
    match msg with
    | SetInput value -> { model with Input = value }, Cmd.none
    | LoadTodos msg ->
        match msg with
        | Start() ->
            let loadTodosCmd = Cmd.OfAsync.perform todosApi.getTodos () (Finished >> LoadTodos)

            { model with Todos = model.Todos.StartLoading() }, loadTodosCmd
        | Finished todos -> { model with Todos = Loaded todos }, Cmd.none
    | SaveTodo msg ->
        match msg with
        | Start todoText ->
            let saveTodoCmd =
                let todo = Todo.create todoText
                Cmd.OfAsync.perform todosApi.addTodo todo (Finished >> SaveTodo)

            { model with Input = "" }, saveTodoCmd
        | Finished todos ->
            {
                model with
                    Todos = RemoteData.Loaded todos
            },
            Cmd.none
    | HoverStartButton -> { model with StartButtonState = Hover }, Cmd.none
    | NormalStartButton -> { model with StartButtonState = Normal }, Cmd.none
    | ClickStartButton -> { model with StartButtonState = Clicked }, Cmd.none

open Feliz

module ViewComponents =

    let xpButtonNormal = "xp_btn_norm.png"
    let xpButtonHover = "xp_btn_hover.png"
    let xpButtonClicked = "xp_btn_clicked.png"



    let windowsApp = 
        Html.div [
            prop.text "App"
            prop.className "bg-gray-800 w-fit"
        ]

    let taskbar model dispatch = 

        let buttonBackground =
            match model.StartButtonState with
                | Normal -> xpButtonNormal
                | Hover -> xpButtonHover
                | Clicked -> xpButtonClicked

        Html.footer [
            prop.id "taskbar"
            prop.className "row-start-2 col-span-2 flex justify-between items-center h-full"
            prop.style [
                style.backgroundImage "linear-gradient(#1f2f86, #3165c4 3%, #3682e5 6%, #4490e6 10%, #3883e5 12%, #2b71e0 15%, #2663da 18%, #235bd6 20%, #2258d5 23%, #2157d6 38%, #245ddb 54%, #2562df 86%, #245fdc 89%, #2158d4 92%, #1d4ec0 95%, #1941a5 98%)"
            ]
            prop.children [
                Html.button [
                    prop.className "flex"
                    prop.style [
                        style.backgroundImageUrl $"xp_btn/{buttonBackground}"
                        style.width 97 // Set button width
                        style.height 30 // Set button height
                    ]
                    prop.onMouseEnter (fun _ -> dispatch HoverStartButton)
                    prop.onMouseLeave (fun _ -> dispatch NormalStartButton)
                    prop.onMouseDown (fun _ -> dispatch ClickStartButton)
                    prop.onMouseUp (fun _ -> dispatch HoverStartButton)
                ]
                Html.div [
                ]
                Html.div [
                ]
            ]

            ]

    let listApps model dispatch =
        Html.div [
            prop.id "listApps"
            prop.className "row-start-1 col-start-1 flex flex-col gap-2 m-2"
            prop.children [
                windowsApp
                windowsApp
                windowsApp
            ]
        ]

let view model dispatch =
    Html.section [
        prop.className "h-screen w-screen grid gap-4 grid-rows-[1fr_auto] grid_cols-[auto_1fr]"
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "bg.jpg"
            //style.display.grid; //or use directly in class name
        ]
        
        prop.children [
            //Apps are on left in a column
            ViewComponents.listApps model dispatch
            //Taskbar bottom taking whole width (flexbox or grids?)
            ViewComponents.taskbar model dispatch
        ]
    ]

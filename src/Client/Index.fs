module Index

open Elmish
open SAFE
open Shared
open System


type ButtonState =
    | Normal
    | Hover
    | Clicked


type WindowsShortcut = {
    Id: Guid
    Name: string
    ImageURL: string
    Selected: bool
}

type Window = {
    Id: Guid
    Active: bool
    Shortcut: WindowsShortcut
}

type Model = {
    Todos: RemoteData<Todo list>
    Input: string
    //
    StartButtonState: ButtonState
    Shortcuts: WindowsShortcut List
    Windows: Window List
}

type Msg =
    | SetInput of string
    | LoadTodos of ApiCall<unit, Todo list>
    | SaveTodo of ApiCall<string, Todo list>
    //
    | HoverStartButton
    | NormalStartButton
    | ClickStartButton
    | SelectWindowsShortcut of WindowsShortcut
    | DeselectWindowsShortcuts
    | OpenWindow of WindowsShortcut

let todosApi = Api.makeProxy<ITodosApi> ()

let init () =
    let shortcuts = [
        {    
        Id = Guid.NewGuid();
        Name = "My Computer";
        ImageURL = "app_icons/mycomputer.png";
        Selected = false
        };
        {    
        Id = Guid.NewGuid();
        Name = "My Work";
        ImageURL = "app_icons/mycomputer.png";
        Selected = false
        }
    ]

    let windows = 
        shortcuts
        |> List.map (fun currentShortcut -> { Id = Guid.NewGuid(); Active = false; Shortcut = currentShortcut }) 

    let initialModel = { Todos = NotStarted; Input = ""; StartButtonState = Normal; Shortcuts = shortcuts; Windows = windows }
    let initialCmd = LoadTodos(Start()) |> Cmd.ofMsg

    initialModel, initialCmd

let deselectWindowsShortcuts (windowsShortcutList:WindowsShortcut list) =
    windowsShortcutList
    |> List.map (fun app -> if app.Selected = true then { app with Selected = false} else app)

let updateWindowsShortcut (windowsShortcut:WindowsShortcut) (windowsShortcutList:WindowsShortcut list) =
    // WindowsShortcutList //TODO You should prob check if it exists? And throw error if none
    // |> List.exists (fun app -> app.Id = WindowsShortcut.Id)

    //deselect any previous app clicked
    windowsShortcutList
    |> deselectWindowsShortcuts
    |> List.map (fun app -> if app.Id = windowsShortcut.Id then { app with Selected = true} else app )
    
let openWindow (shortcut : WindowsShortcut) (windows :Window list)= 
    //set active the window which has same shortcut as the argument given
    windows
    |> List.map (fun window -> if window.Shortcut.Id = shortcut.Id then {window with Active = true} else window)


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
    | SelectWindowsShortcut windowsShortcut -> { model with Shortcuts = updateWindowsShortcut windowsShortcut model.Shortcuts } , Cmd.none
    | DeselectWindowsShortcuts -> { model with Shortcuts = deselectWindowsShortcuts model.Shortcuts } , Cmd.none
    | OpenWindow windowsShortcut-> {model with Windows = openWindow windowsShortcut model.Windows}, Cmd.none

open Feliz

module ViewComponents =

    let xpButtonNormal = "xp_btn_norm.png"
    let xpButtonHover = "xp_btn_hover.png"
    let xpButtonClicked = "xp_btn_clicked.png"

    let windowDisplay (window:Window) = 
        Html.div [
            prop.text window.Shortcut.Name
        ]

    let windowsDisplayed (windows: Window list) (dispatch:Msg -> Unit) =
        Html.div [
            for window in windows do
                if window.Active then
                    windowDisplay window
        ]

    let windowsShortcut (windowsShortcut: WindowsShortcut) (dispatch:Msg -> Unit) = 
        Html.button [
            prop.className "w-fit flex flex-col"
            prop.style [
                style.alignItems.center
                if windowsShortcut.Selected then
                    style.custom("filter", "drop-shadow(blue 0px 0px)")
            ]
            prop.onClick (fun e -> 
                e.stopPropagation() 
                if windowsShortcut.Selected then
                    dispatch (OpenWindow windowsShortcut)
                else 
                    dispatch (SelectWindowsShortcut windowsShortcut)
                )
            prop.children [
                //icon
                Html.div [
                    prop.style [
                        style.backgroundSize "contain"
                        style.width 32
                        style.height 32
                        if windowsShortcut.Selected then
                            style.custom("opacity", "0.5")
                        style.backgroundImageUrl $"{windowsShortcut.ImageURL}"
                    ]
                ]
                //text
                Html.div [
                    prop.text $"{windowsShortcut.Name}"
                    prop.style [
                        style.color "white";
                        style.fontSize 10
                        style.fontFamily "Tahoma, sans-serif"
                        if windowsShortcut.Selected then
                            style.custom("textShadow", "black 0px 1px 1px")
                            style.custom("backgroundColor", "rgb(11, 97, 255)")
                    ]
                ]
            ]
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
                    prop.onClick (fun _ -> dispatch ClickStartButton)
                    prop.onMouseUp (fun _ -> dispatch HoverStartButton)
                ]
                Html.div [
                ]
                Html.div [
                    prop.className "flex flex:row"
                    prop.style [
                        style.boxShadow (1, 0, 1, "#18bbff")
                        style.custom("boxShadow", "inset 1px 0 1px #18bbff") //TODO replace with actual methods? should be somewhere in style
                        style.justifyContent.spaceAround //TODO inconsistent styling with flex and grids either all in class name or all in methods
                        style.alignItems.center
                        style.width 160 // Set button width //TODO Make it dynamic based on content inside
                        style.height 30 // Set button height
                        style.backgroundImage "linear-gradient(#0c59b9 1%, #139ee9 6%, #18b5f2 10%, #139beb 14%, #1290e8 19%, #0d8dea 63%, #0d9ff1 81%, #0f9eed 88%, #119be9 91%, #1392e2 94%, #137ed7 97%, #095bc9)"
                    ]
                    prop.children [
                        //list of icon
                        Html.div [
                            prop.className "flex gap-2"

                            prop.children [
                                //Icons
                                Html.div [
                                    prop.style [
                                        style.backgroundSize "contain"

                                        style.width 16
                                        style.height 16
                                        style.backgroundImageUrl $"tray_icons/green_shield.png"
                                    ]
                                ]
                                Html.div [
                                    prop.style [
                                        style.backgroundSize "contain"

                                        style.width 16
                                        style.height 16
                                        style.backgroundImageUrl $"tray_icons/internet.png"
                                    ]
                                ]
                                Html.div [
                                    prop.style [
                                        style.backgroundSize "contain"
                                        style.width 16
                                        style.height 16
                                        style.backgroundImageUrl $"tray_icons/defaultprog.png"
                                    ]
                                ]
                            ]
                        ]
                        //time
                        Html.div [
                            prop.text "13:35 PM"
                            prop.style [
                                style.color "white"
                                style.fontSize 12;
                                style.fontFamily "Tahoma, sans-serif"
                                //find windows font
                            ]
                            prop.className ""
                        ]
                    ]
                ]
            ]

            ]

    let listApps (model:Model) (dispatch:Msg -> Unit) =
        Html.div [
            prop.id "listApps"
            prop.className "row-start-1 col-start-1 flex flex-col gap-2 m-2"
            prop.children [
                for app in model.Shortcuts do
                    windowsShortcut app dispatch
            ]
        ]

let view (model:Model) (dispatch:Msg -> Unit) =
    Html.section [
        prop.className "h-screen w-screen grid gap-4 grid-rows-[1fr_auto] grid_cols-[auto_1fr]"
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "bg.jpg"
            //style.display.grid; //or use directly in class name
        ]
        prop.onClick(fun _ -> dispatch DeselectWindowsShortcuts)
        prop.children [
            ViewComponents.listApps model dispatch
            ViewComponents.taskbar model dispatch
            ViewComponents.windowsDisplayed model.Windows dispatch
        ]
    ]

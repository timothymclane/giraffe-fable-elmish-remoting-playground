module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma
open Fulma.Layouts
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Components
open Fulma.BulmaClasses

open Fulma.BulmaClasses.Bulma
open Fulma.BulmaClasses.Bulma.Properties
open Fulma.Extra.FontAwesome
open Fable.Import
open Fable.Import.Browser

Fable.Core.JsInterop.importAll "formdata-polyfill"


type Model = {
  Counter: Counter option
  AuthToken: string option
  LoggingIn: bool
  LoginError: string option
 }


type NavigationCommand =
  | Dashboard

type ErrorCommand =
  | FailedLogin
  
type Msg =
| Failure of ErrorCommand
| Increment
| Decrement
| Login of LoginRequest
| LoginResult of Result<LoginResult, exn>
| Init of Result<Counter, exn>
| NavigateTo of NavigationCommand
| ChangeUsername of string
| ChangePassword of string
| LoginSuccess of adminSecureToken: string
| LoginFailed of error:string
| UpdateValidationErrors 


module Server = 

  open Shared
  open Fable.Remoting.Client
  
  /// A proxy you can use to talk to server directly
  let counterApi : ICounterProtocol = 
     Proxy.createSecureWithEndpointAndBuilder<ICounterProtocol> None Route.builder "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyIjoidGVzdHVzZXIifQ.KoAN4i4IJgQLCayUrxZqgAX3MAbO9vZVHcxCLWBKRNw"
  let loginApi : Login = 
    Proxy.createWithBuilder<Login> Route.builder
    

let init () = 
  let model = {
    AuthToken = None
    Counter = None
    LoggingIn = false
    LoginError = None
  }
  let cmd =
    Cmd.ofAsync 
      Server.counterApi.getInitCounter
      () 
      (Ok >> Init)
      (Error >> Init)
  model, cmd

let successHandler = function
  | Success token -> LoginSuccess token
  | UsernameDoesNotExist -> LoginFailed "Username does not exist"
  | PasswordIncorrect -> LoginFailed "The password you entered is incorrect"
  | LoginError error -> LoginFailed error

let update msg (model : Model) =
  let model' =
    match model,  msg with
    | x, Increment -> {x with Counter = Some <| x.Counter.Value + 1},Cmd.none
    | x, Decrement -> {x with Counter = Some <| x.Counter.Value - 1},Cmd.none
    | x, Init result ->
      match result with
      | Ok r -> {x with Counter = Some r}, Cmd.none
      | Error _ -> x,Cmd.none
    | x, Login y ->  
      Browser.console.log("login request", y)
      {x with LoggingIn = true},Cmd.ofAsync
        Server.loginApi.login
        y
        (Ok >> LoginResult)
        (Error >> LoginResult)    
    |   x, LoginResult result ->
        let x = {x with LoggingIn = false}
        match result with 
          | Ok r ->
              match r with
              | Success s ->
                      let nextModel = { x with AuthToken = Some s }
                      nextModel,Cmd.ofMsg <| NavigateTo Dashboard
              | e -> x,Cmd.none                  
          | Error e ->
            x,Cmd.none            
    | x, LoginFailed error ->
        let nextModel =  { x with 
                                  LoginError = Some error 
                                  LoggingIn = false }
        nextModel,Cmd.none
    | _ -> model,Cmd.none
  model'

let safeComponents =
  let intersperse sep ls =
    List.foldBack (fun x -> function
      | [] -> [x]
      | xs -> x::sep::xs) ls []

  let components =
    [
      "Giraffe", "https://github.com/giraffe-fsharp/Giraffe"
      "Fable", "http://fable.io"
      "Elmish", "https://fable-elmish.github.io/"
      "Fulma", "https://mangelmaxime.github.io/Fulma" 
      "Bulma\u00A0Templates", "https://dansup.github.io/bulma-templates/"
      "Fable.Remoting", "https://github.com/Zaid-Ajaj/Fable.Remoting"
    ]
    |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
    |> intersperse (str ", ")
    |> span [ ]

  p [ ]
    [ strong [] [ str "SAFE Template" ]
      str " powered by: "
      components ]

let show = function
| Some x -> string x
| None -> "Loading..."


let counter model dispatch =
  Form.Field.div [ Form.Field.IsGrouped ] 
    [ Form.Control.p [ Form.Control.CustomClass "is-expanded"] 
        [ Form.Input.text
            [ Form.Input.Disabled true
              Form.Input.Value (show model.Counter) ] ]
      Form.Control.p [ ]
        [ Button.a 
            [ Button.Color IsInfo
              Button.OnClick (fun _ -> dispatch Increment) ]
            [ str "+" ] ]
      Form.Control.p [ ]
        [ Button.a 
            [ Button.Color IsInfo
              Button.OnClick (fun _ -> dispatch Decrement) ]
            [ str "-" ] ] ]

type IRealFormData = 
  abstract get : string -> string

type LoginRecord = {
  username: string
  password: string
}

let buildLogin login =
  Login login
let log : obj -> unit = Fable.Core.JsInterop.import "log" "./log.js" 
let column model dispatch =
  Column.column 
    [ Column.Width (Column.All, Column.Is4)
      Column.Offset (Column.All, Column.Is4) ]
    [ Heading.h3
        [ Heading.CustomClass "title has-text-grey" ]
        [ str "Login" ]
      p [ Class "subtitle has-text-grey" ]
        [ str "Please login to proceed." ]
      Box.box' [ ]
        [ figure [ Class "avatar" ]
            [ img [ Src "https://placehold.it/128x128" ] ]
          form [ OnSubmit (fun event -> 
                            event.preventDefault()
                            let formElement = event.currentTarget :?> HTMLFormElement
                            // log <| formElement                            
                            let formData = FormData.Create formElement :?> IRealFormData
                            // log <| formData
                            dispatch <| buildLogin {
                                username = formData.get("email")
                                password = formData.get("password")
                              }
                            ) ]
            [ Field.div [ ]
                [ Control.div [ ]
                    [ Input.text 
                        [ Input.Size IsLarge
                          Input.Placeholder "Your Email"
                          Input.Props [ AutoFocus true; Name "email" ] ] ] ]
              Field.div [ ]
                [ Control.div [ ]
                    [ Input.password 
                        [ Input.Size IsLarge
                          Input.Placeholder "Your Password"
                          Input.Props [Name "password"] ] ] ]
              counter model dispatch
              Field.div [ ]
                [ Checkbox.checkbox [ ]
                    [ input [ Type "checkbox" ]
                      str "Remember me" ] ]
              Button.button 
                [ Button.Color IsInfo
                  Button.IsFullwidth
                  Button.CustomClass "is-large is-block" ]
                [ str "Login" ] ] ]
      p [ Class "has-text-grey" ]
        [ a [ ] [ str "Sign Up" ]
          str "\u00A0·\u00A0"
          a [ ] [ str "Forgot Password" ]
          str "\u00A0·\u00A0"
          a [ ] [ str "Need Help?" ] ]
      br [ ]
      p [ Class "has-text-grey" ] 
        [ safeComponents ] ]

let view model dispatch =
  Hero.hero 
    [ Hero.Color IsSuccess 
      Hero.IsFullHeight ]
    [ Hero.body [ ]
        [ Container.container 
            [ Container.CustomClass Alignment.HasTextCentered ]
            [ column model dispatch ] ] ]


  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run

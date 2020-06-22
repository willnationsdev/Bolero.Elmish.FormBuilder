// Derived from `thoth-org/Thoth.Elmish.FormBuilder.BasicFields`

namespace Bolero.Elmish.FormBuilder.BasicFields

open Bolero.Html
open Bolero.Elmish.FormBuilder
open Bolero.Elmish.FormBuilder.Types
open System
open Thoth.Json.Net

[<RequireQualifiedAccess>]
module Radio =

    type Key = string

    type State =
        {
            Label: string
            SelectedKey: Key option
            Values: (Key * string) list
            Group: string
            Validators: Validator list
            ValidationState: ValidationState
            Name: string
        }

    and Validator = State -> ValidationState

    type Msg =
    | ChangeValue of string
        interface IFieldMsg

    let private init (state: FieldState) =
        state, FormCmd.none

    let private validate (state: FieldState) =
        let state: State = state :?> State
        let rec applyValidators (validators: Validator list) (state: State) =
            match validators with
            | validator::rest ->
                match validator state with
                | Valid -> applyValidators rest state
                | Invalid msg ->
                    { state with ValidationState = Invalid msg }
            | [] -> state

        applyValidators state.Validators { state with ValidationState = Valid } |> box

    let private isValid (state: FieldState) =
        let state = state :?> State
        state.ValidationState = Valid

    let private setError (state: FieldState) (message: string) =
        let state: State = state :?> State
        { state with ValidationState = Invalid message } |> box

    let private toJson (state: FieldState) =
        let state: State = state :?> State
        state.Name, state.SelectedKey
                    |> Option.map Encode.string
                    |> Option.defaultValue Encode.nil

    let private update (msg: FieldMsg) (state: FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue key ->
            { state with SelectedKey = Some key }
            |> validate
            |> box, FormCmd.none

    let private renderRadio (group: string) (selectedKey: Key option) (dispatch: IFieldMsg -> unit) (key, value) =
        let radioId = group + "-" + key
        label [ attr.``class`` "radio"
                attr.``for`` radioId
                Bolero.Key key ]
            [ input [ attr.``type`` "radio"
                      attr.``class`` "radio"
                      attr.id radioId
                      on.change (fun _ -> ChangeValue key |> dispatch)
                      selectedKey
                      |> Option.map ((=) key)
                      |> Option.defaultValue false
                      |> attr.``checked`` ]
              text value ]

    let private view (state: FieldState) (dispatch: IFieldMsg -> unit) =
        let state: State = state :?> State

        div [ attr.``class`` "field" ]
            [ label
                [ attr.``class`` "label" ]
                [ text state.Label ]

              div [ attr.``class`` "control" ]
                  [
                    yield! state.Values
                           |> List.map (renderRadio state.Group state.SelectedKey dispatch)
                  ]

              span [ attr.classes ["help"; "is-danger" ] ]
                [ text state.ValidationState.Text ]
            ]
              
    let config: FieldConfig =
        {
            View = view
            Update = update
            Init = init
            Validate = validate
            IsValid = isValid
            ToJson = toJson
            SetError = setError
        }

type BasicRadio private (state: Radio.State) =
    
    member __.WithDefaultView (): FieldBuilder =
        {
            Type = "basic-radio-button"
            State = state
            Name = state.Name
            Config = Radio.config
        }

    member this.WithCustomView (view): FieldBuilder =
        let def = this.WithDefaultView ()
        { def with Config = { def.Config with View = view } }
        
    static member Create(name: string) =
        BasicRadio
            {
                Label = ""
                SelectedKey = None
                Values = []
                Group = (Guid.NewGuid()).ToString()
                Validators = []
                ValidationState = Valid
                Name = name
            }

    member __.WithLabel (label: string): BasicRadio =
        BasicRadio { state with Label = label }

    member __.WithValues (values: (Radio.Key * string) list): BasicRadio =
        BasicRadio { state with Values = values }

    member __.WithSelectedKey (selectedKey: string) =
        BasicRadio { state with SelectedKey = Some selectedKey }

    member __.AddValidator (validator) =
        BasicRadio { state with Validators = validator :: state.Validators }

    member this.IsRequired (?msg: string) =
        let msg = defaultArg msg "This field is required"

        let validator (state: Radio.State) =
            if Option.isSome state.SelectedKey then
                Valid
            else
                Invalid msg

        this.AddValidator validator


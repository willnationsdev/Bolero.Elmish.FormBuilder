// Derived from `thoth-org/Thoth.Elmish.FormBuilder.BasicFields`

namespace Bolero.Elmish.FormBuilder.BasicFields

open Bolero.Html
open Bolero.Elmish.FormBuilder
open Bolero.Elmish.FormBuilder.Types
open System
open Thoth.Json.Net

[<RequireQualifiedAccess>]
module Checkbox =

    type State =
        {
            Label: string
            IsChecked: bool
            Validators: Validator list
            ValidationState: ValidationState
            Name: string
        }

    and Validator = State -> ValidationState

    type Msg =
    | ToggleState
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
        state.Name, Encode.bool state.IsChecked

    let private update (msg: FieldMsg) (state: FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ToggleState ->
            { state with IsChecked = not state.IsChecked }
            |> validate
            |> box, FormCmd.none

    let private view (state: FieldState) (dispatch: IFieldMsg -> unit) =
        let state: State = state :?> State
        
        div [ attr.``class`` "field" ]
            [ div [ attr.``class`` "control" ]
                [ label [ attr.``class`` "checkbox" ]
                    [ input [ attr.``type`` "checkbox"
                              attr.``class`` "checkbox"
                              attr.``checked`` state.IsChecked
                              on.change (fun _ ->
                                dispatch ToggleState
                              )]
                      text state.Label
                    ] ]
              span [ attr.classes ["help"; "is-danger"] ]
                [ text state.ValidationState.Text ] ]
              
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

type BasicCheckbox private (state: Checkbox.State) =
    
    member __.WithDefaultView (): FieldBuilder =
        {
            Type = "basic-textarea"
            State = state
            Name = state.Name
            Config = Checkbox.config
        }

    member this.WithCustomView (view): FieldBuilder =
        let def = this.WithDefaultView ()
        { def with Config = { def.Config with View = view } }
        
    static member Create(name: string) =
        BasicCheckbox
            {
                Label = ""
                IsChecked = false
                Validators = []
                ValidationState = Valid
                Name = name
            }

    member __.WithLabel (label: string): BasicCheckbox =
        BasicCheckbox { state with Label = label }

    member __.WithValue (value: bool): BasicCheckbox =
        BasicCheckbox { state with IsChecked = value }

    member __.AddValidator (validator) =
        BasicCheckbox { state with Validators = validator :: state.Validators }

    member this.IsRequired (?msg: string) =
        let msg = defaultArg msg "This field is required"

        let validator (state: Checkbox.State) =
            if state.IsChecked then
                Valid
            else
                Invalid msg

        this.AddValidator validator


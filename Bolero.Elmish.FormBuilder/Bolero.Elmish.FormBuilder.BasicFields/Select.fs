// Derived from `thoth-org/Thoth.Elmish.FormBuilder.BasicFields`

namespace Bolero.Elmish.FormBuilder.BasicFields

open Bolero.Html
open Bolero.Elmish.FormBuilder
open Bolero.Elmish.FormBuilder.Types
open System
open Thoth.Json.Net

[<RequireQualifiedAccess>]
module Select =

    type Key = string

    type State =
        {
            Label: string
            Placeholder: (Key * string) option
            SelectedKey: Key option
            Values: (Key * string) list
            IsLoading: bool
            ValuesFromServer: Async<(Key * string) list> option
            Validators: Validator list
            ValidationState: ValidationState
            Name: string
        }

    and Validator = State -> ValidationState

    type Msg =
    | ChangeValue of string
    | ReceivedValueFromServer of (Key * string) list
    | OnError of exn
        interface IFieldMsg

    let private init (state: FieldState) =
        let state = state :?> State

        match state.ValuesFromServer with
        | Some fetchKeyValues ->
            let request () =
                async {
                    return! fetchKeyValues
                }

            box { state with IsLoading = true}, FormCmd.ofAsync request () ReceivedValueFromServer OnError

        | None -> box state, FormCmd.none

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
        not state.IsLoading && state.ValidationState = Valid

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

        | ReceivedValueFromServer values ->
            box { state with IsLoading = false
                             Values = values}, FormCmd.none

        | OnError error ->
            Console.Error.WriteLine error
            box state, FormCmd.none

    let private renderOption (key, value) =
        option [ attr.value key
                 Bolero.Key key ]
            [ text value]

    let private renderPlaceHolder (placeholder: (Key * string) option) =
        match placeholder with 
        | Some (key, value) ->
            option [ attr.value key
                     attr.disabled true ]
                [ text value ]
        | None -> Bolero.Empty

    let private view (state: FieldState) (dispatch: IFieldMsg -> unit) =
        let state : State = state :?> State
        
        let selectClass =
            // Don't display the select in red while waiting on the server
            attr.classes (
                [
                    "select"
                    "is-fullwidth"
                    if state.IsLoading then
                        "is-loading"
                    else if not (isValid state) then
                        "is_danger"
                    else ""
                ]
                |> List.filter (String.IsNullOrWhiteSpace >> not)
            )

        let placeholderKey =
            state.Placeholder
            |> Option.map fst
            |> Option.defaultValue ""

        div [ attr.``class`` "field" ]
            [ label [ attr.``class`` "label"
                      attr.``for`` state.Name ]
                [ text state.Label ]
              div [ attr.``class`` "control" ]
                [ div [ selectClass ]
                    [ select [ attr.value (state.SelectedKey |> Option.defaultValue placeholderKey)
                               on.change (fun ev ->
                                    ev.Value |> string |> ChangeValue |> dispatch
                               ) ]
                        [
                            yield! (state.Values |> List.map renderOption)
                            renderPlaceHolder state.Placeholder
                        ]
                    ]
                ]
              span [ attr.``class`` "help is-danger" ]
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

type BasicSelect private (state: Select.State) =
    
    member __.WithDefaultView (): FieldBuilder =
        {
            Type = "basic-select"
            State = state
            Name = state.Name
            Config = Select.config
        }

    member this.WithCustomView (view): FieldBuilder =
        let def = this.WithDefaultView ()
        { def with Config = { def.Config with View = view } }
        
    static member Create(name: string) =
        BasicSelect
            {
                Label = ""
                Placeholder = None
                SelectedKey = None
                Values = []
                IsLoading = false
                ValuesFromServer = None
                Validators = []
                ValidationState = Valid
                Name = name
            }

    member __.WithLabel (label: string): BasicSelect =
        BasicSelect { state with Label = label }

    member __.WithPlaceholder (placeholder: string, ?key: Select.Key): BasicSelect =
        let key = defaultArg key "this-is-the-placeholder-value"
        BasicSelect { state with Placeholder = Some (key, placeholder) }

    member __.WithSelectedKey (selectedKey: string) =
        BasicSelect { state with SelectedKey = Some selectedKey }

    member __.WithValues (values: (Select.Key * string) list): BasicSelect =
        BasicSelect { state with Values = values }

    member __.WithValuesFromServer (request: Async<(Select.Key * string) list>): BasicSelect =
        BasicSelect { state with ValuesFromServer = Some request
                                 IsLoading = true }

    member __.AddValidator (validator) =
        BasicSelect { state with Validators = validator :: state.Validators }

    member this.IsRequired (?msg: string) =
        let msg = defaultArg msg "This field is required"

        let validator (state: Select.State) =
            if Option.isSome state.SelectedKey then
                Valid
            else
                Invalid msg

        this.AddValidator validator


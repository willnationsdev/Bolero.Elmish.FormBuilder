// Derived from `thoth-org/Thoth.Elmish.FormBuilder`

namespace Bolero.Elmish.FormBuilder

open Elmish
open Types
open Thoth.Json.Net

type Form<'AppMsg> private (onFormMsg: Msg -> 'AppMsg, fieldBuilders: Types.FieldBuilder list) =

    static member Create(onFormMsg) =
        Form (onFormMsg, [])

    member this.AddField(fieldBuilder: FieldBuilder) =
        Form (onFormMsg, fieldBuilders @ [ fieldBuilder ] )

    member this.AddFields(fieldBuilders: FieldBuilder list) =
        Form (onFormMsg, fieldBuilders @ fieldBuilders )

    member this.Build() =
        let duplicateNames =
            fieldBuilders
            |> List.groupBy (fun builder -> builder.Name)
            |> List.filter (fun (_, set) -> set.Length > 1)
            |> List.map fst

        if duplicateNames.Length > 0 then
            let mutable msg = "Each field needs to have a unique name. I found the following duplicate names:"

            for name in duplicateNames do
                msg <- msg + "\n-" + name

            failwith msg

        let config: Types.Config<'AppMsg> =
            {
                ToMsg = onFormMsg
                FieldsConfig =
                    fieldBuilders
                    |> List.map (fun builder -> builder.Type, builder.Config)
                    |> Map.ofList
            }

        let fields: Types.Field list =
            fieldBuilders
            |> List.map (fun builder ->
                {
                    Type = builder.Type
                    State = builder.State
                    Name = builder.Name
                }
            )

        {
            Fields = fields
            IsLoading = false
        }, config

[<RequireQualifiedAccess>]
module Form =
    open Bolero
    open Bolero.Html
    
    module private Styles =
        
        let inline form<'a> =
            [
                "position" => "relative"
            ]

        let inline loaderContainer<'a> =
            [
                "display" => "flex"
                "position" => "absolute"
                "justify-content" => "center"
                "width" => "100%"
                "height" => "100%"
                "align-items" => "center"
                "z-index" => 10
                "opacity" => 0.4
                "background-color" => "white"
            ]

        // This code has been adapted from
        // https://loading.io/css/
        let inline loaderRings<'a> =
            [
                "content" => " "
                "display" => "block"
                "width" => "46px"
                "height" => "46px"
                "margin" => "1px"
                "border-radius" => "50%"
                "border" => "5px solid #000"
                "border-color" => "#000 transparent #000 transparent"
                "animation" => "thoth-form-loader-rings 1.2s linear infinite"
            ]

        [<Literal>]
        let loaderRingsFramesCss =
            """@keyframes thoth-form-loader-rings {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}
"""

    type LoaderContainer =
    | DefaultLoader
    | CustomLoader of (bool -> Node)

    type FormRenderProps<'Msg> =
        {
            Config: Config<'Msg>
            State: State
            Dispatch: 'Msg -> unit
            ActionsArea: Node
            Loader: LoaderContainer
        }

    /// `init` function to call from your `init` to intialize the form
    /// Returns:
    /// - `formState` - `State` - New state to be stored in your Model
    /// - `formCmd` - `Cmd<Msg>` - Commands to be mapped in your Elmish application
    let init (config: Config<_>) (form: State) =
        let mappedFields =
            form.Fields
            |> List.map (fun info ->
                let fieldConfig = Map.find info.Type config.FieldsConfig

                let newState, cmd = fieldConfig.Init info.State
                { info with State = newState }, cmd info.Name
            )

        let fields = mappedFields |> List.map fst
        let cmds = mappedFields |> List.map snd

        { form with Fields = fields }, Cmd.batch cmds

    /// `update` function to call when you received a message for the form
    /// Returns:
    /// - `formState` - `State` - New state to be stored in your Model
    /// - `formCmd` - `Cmd<Msg>` - Commands to be mapped in your Elmish application
    let update (config: Config<_>) (msg: Msg) (form: State) =
        match msg with
        | OnFieldMessage (fieldName, msg) ->
            let mappedFields =
                form.Fields
                |> List.map (fun info ->
                    if info.Name = fieldName then
                        let fieldConfig = Map.find info.Type config.FieldsConfig

                        let newState, cmd = fieldConfig.Update msg info.State
                        { info with State = newState }, cmd info.Name
                    else
                        info, FormCmd.none info.Name
                )

            let fields = mappedFields |> List.map fst
            let cmds = mappedFields |> List.map snd

            { form with Fields = fields }, Cmd.batch cmds

    let inline private defaultLoader (isLoading: bool) =
        if isLoading then
            div [
                    attr.``class`` "loader-container"
                    Attrs Styles.loaderContainer
                    // When using the defaultLoader, we append the needed key frames
                    // by appending a manually created `style` element.
                    // We do that in order to make Bolero.Elmish.FormBuilder
                    // directly usable without asking the user to load any CSS styles.
                    // Also, because all the functions/styles relative to the default loader
                    // are marked as `inline`, they will not be included in the bundle if not used

                    
                    Ref (System.Action<_>(fun elt ->
                        Helpers.appendStyle
                            "thoth-form-loader-container-key-frames"
                            Styles.loaderRingsFramesCss
                    ))
                ]
                [ div [ attr.``class`` "loader-rings"
                        Attrs Styles.loaderRings ]
                        [] ]
            else
                Bolero.Empty

    /// Render the form in your view
    /// Props description:
    /// - `Config` - `Config` - Configuration of your form. You get it when calling `Form.init`
    /// - `State` - `State<'Msg>` - Current state of the form. It's coming from your `Model`
    /// - `Dispatch` - `'Msg -> unit` - The `dispatch` coming from Elmish
    /// - `ActionsArea` - `Bolero.Node` - A `Node` to render at the bottom of the form. In general, it contains your buttons
    /// - `Loader` - `LoaderContainer` - Either `Form.DefaultLoader` or `Form.CustomLoader`. This represents the displayed `Node` when loading.
    let render (props: FormRenderProps<'Msg>) =
        let fields =
            props.State.Fields
            |> List.map (fun info ->
                let fieldConfig = Map.find info.Type props.Config.FieldsConfig

                let onFieldChange guid =
                    (fun v -> OnFieldMessage (guid, v)) >> props.Config.ToMsg >> props.Dispatch

                fieldConfig.View info.State (onFieldChange info.Name)
            )

        let loader =
            match props.Loader with
            | DefaultLoader -> defaultLoader props.State.IsLoading
            | CustomLoader loader -> loader props.State.IsLoading

        div [ attr.``class`` "thoth-form"
              Attrs Styles.form ]
            (loader :: fields @ [props.ActionsArea])

    /// Validate the model and check if it's valid.
    /// Returns a tuple of 2 elements.
    /// - First element is the new state of the form to store in your model.
    /// - Second element is `true` if the form is valid. `false` otherwise.
    let validate (config: Config<_>) (form: State): State * bool =
        let newFields =
            form.Fields
            |> List.map (fun field ->
                let fieldConfig = Map.find field.Type config.FieldsConfig
                { field with State = fieldConfig.Validate field.State }
            )
        
        let isValid =
            newFields
            |> List.filter (fun field ->
                let fieldConfig = Map.find field.Type config.FieldsConfig    
                not (fieldConfig.IsValid field.State)
            )
            |> List.length
            |> (=) 0

        { form with Fields = newFields }, isValid


    /// Generate a JSON representation from the current state
    let toJson (config: Config<_>) (form: State): string =
        form.Fields
        |> List.map (fun field ->
            let fieldConfig = Map.find field.Type config.FieldsConfig
            fieldConfig.ToJson field.State
        )
        |> Encode.object
        #if DEBUG
        |> Encode.toString 4
        #else
        |> Encode.toString 0
        #endif

    /// Set the loading state of the form
    let setLoading (isLoading: bool) (form: State): State =
        { form with IsLoading = isLoading }

    /// Check if the form is loading.
    let isLoading (form: State) = form.IsLoading

    /// Set error for each field based on a `ErrorDef list`
    /// You can use this function in order to set errors coming from your server
    let setErrors (config: Config<_>) (errors: ErrorDef list) (form: State) =
        let errors =
            errors
            |> List.groupBy (fun error -> error.Key)
            |> Map.ofList

        let newFields =
            form.Fields
            |> List.map (fun field ->
                match errors.TryGetValue field.Name with
                | (true, first::_) ->
                    let fieldConfig = Map.find field.Type config.FieldsConfig
                    { field with State = fieldConfig.SetError field.State first.Text }
                | (true, [])
                | (false, _) -> field
            )

        { form with Fields = newFields }


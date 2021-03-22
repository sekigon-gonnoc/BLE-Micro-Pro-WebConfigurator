port module App exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Carousel as Carousel exposing (Cycling(..), defaultStateOptions)
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select exposing (Item)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder, bool, field, map3, string)
import Json.Encode as E
import Task
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf)


port updateFirmware : E.Value -> Cmd msg


port updateConfig : E.Value -> Cmd msg


port updateResult : (E.Value -> msg) -> Sub msg


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type UpdateProgress
    = None
    | BootloaderActivated
    | Updating Float
    | Complete
    | Error String


type FirmwareType
    = Bootloader
    | Application


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , navbarState : Navbar.State
    , appInfo : Flag
    , needsHelp : Bool
    , setupRequirement : SetupRequirement
    , setupProcedure : List String
    , carouselState : Carousel.State
    , bootloader : Maybe String
    , application : Maybe String
    , updateProgress : UpdateProgress
    }


type alias Keyboard =
    { name : String
    , layout : List String
    , keymap : List String
    , firmware : String
    , split : Bool
    , lpme : Bool
    }


type alias Flag =
    { revision : String
    , webSerialEnabled : Bool
    , keyboards : List Keyboard
    , bootloaders : List String
    , applications : List String
    }


type alias FirmVersion =
    { keyboard : String
    , major : Int
    , minor : Int
    , revision : Int
    }


type BlemRole
    = SINGLE
    | SPLIT_MASTER
    | SPLIT_SLAVE
    | MASTER_WITH_LPME


type alias SetupRequirement =
    { keyboard : Keyboard
    , layout : String
    , role : BlemRole
    , isLeft : Bool
    , isJis : Bool
    , disableMsc : Bool
    , debounce : Int
    , centralInterval : Int
    , periphInterval : Int
    , autoSleep : Int
    }


type alias UpdateResult =
    { progress : Float
    , message : String
    }


flagDecoder : Decoder Flag
flagDecoder =
    D.map5 Flag
        (field "revision" D.string)
        (field "webSerialEnabled" bool)
        (field "keyboards"
            (D.list
                (D.map6
                    Keyboard
                    (field "name" string)
                    (field "layout" (D.list string))
                    (field "keymap" (D.list string))
                    (field "firmware" string)
                    (field "split" bool)
                    (field "lpme" bool)
                )
            )
        )
        (field "bootloaders" (D.list string))
        (field "applications" (D.list string))


updateProgressEncode : E.Value -> UpdateProgress
updateProgressEncode json =
    let
        decoder =
            D.map2 UpdateResult
                (field "progress" D.float)
                (field "message" D.string)

        result =
            Result.withDefault
                (UpdateResult -2
                    "Failed to decode message from updater"
                )
                (D.decodeValue
                    decoder
                    json
                )
    in
    if result.progress >= 100 then
        Complete

    else if result.progress >= 0 then
        Updating result.progress

    else if result.progress >= -1 then
        BootloaderActivated

    else
        Error result.message


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { url = url
      , key = key
      , navbarState = navbarState
      , appInfo =
            case D.decodeValue flagDecoder flags of
                Ok flag ->
                    flag

                Err _ ->
                    Flag "" False [] [] []
      , needsHelp = False
      , setupRequirement =
            { keyboard = Keyboard "" [] [] "" False False
            , layout = ""
            , role = SINGLE
            , isLeft = True
            , isJis = False
            , disableMsc = False
            , debounce = 1
            , centralInterval = 30
            , periphInterval = 30
            , autoSleep = 0
            }
      , setupProcedure = []
      , carouselState = Carousel.initialState
      , bootloader = Nothing
      , application = Nothing
      , updateProgress = None
      }
    , Cmd.batch
        [ navbarCmd
        ]
    )


applicationList : Model -> List String
applicationList model =
    filterOrAll
        model.setupRequirement.keyboard.firmware
        model.appInfo.applications


bootloaderList : Model -> List String
bootloaderList model =
    filterOrAll
        (String.toLower model.setupRequirement.keyboard.name)
        model.appInfo.bootloaders


keyboardLayoutList : Model -> List String
keyboardLayoutList model =
    if
        model.needsHelp
            && List.length model.setupRequirement.keyboard.layout
            > 1
    then
        model.setupRequirement.layout
            :: model.setupRequirement.keyboard.layout

    else
        model.setupRequirement.keyboard.layout



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Carousel.subscriptions model.carouselState CarouselMsg
        , updateResult UpdateResultMsg
        ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavbarMsg Navbar.State
    | CarouselMsg Carousel.Msg
    | StartNavigation
    | SelectKeyboard String
    | SelectLayout String
    | SelectBootloader String
    | SelectApplication String
    | SetProceduer
    | GoNextStep
    | GoSlaveSetup
    | UseMsc Bool
    | UseLpme Bool
    | UpdateBootloader
    | UpdateApplication
    | UpdateConfig
    | UpdateResultMsg E.Value
    | IncrementDebounce Int
    | IncrementAutoSleep Int
    | IncrementPeriphInterval Int
    | IncrementCentralInterval Int
    | IsSlave Bool
    | IsLeft Bool
    | IsJis Bool
    | UploadConfigRequested
    | UploadConfigSelected File
    | UploadConfigLoaded String


useSlave : BlemRole -> Bool
useSlave role =
    case role of
        SPLIT_MASTER ->
            True

        _ ->
            False


isSlave : BlemRole -> Bool
isSlave role =
    case role of
        SPLIT_SLAVE ->
            True

        _ ->
            False


isSplit : BlemRole -> Bool
isSplit role =
    case role of
        SINGLE ->
            False

        _ ->
            True


useLpme : BlemRole -> Bool
useLpme role =
    case role of
        MASTER_WITH_LPME ->
            True

        _ ->
            False


goNextStep : Model -> ( Model, Cmd Msg )
goNextStep model =
    let
        nextPage =
            Maybe.withDefault "/home" (List.head model.setupProcedure)
    in
    ( { model
        | setupProcedure = List.drop 1 model.setupProcedure
      }
    , Nav.pushUrl model.key nextPage
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , carouselState = Carousel.toSlide 0 model.carouselState
                , updateProgress = None
                , bootloader = Nothing
                , application = Nothing
              }
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        CarouselMsg subMsg ->
            ( { model | carouselState = Carousel.update subMsg model.carouselState }, Cmd.none )

        StartNavigation ->
            ( { model | needsHelp = True }
            , Cmd.batch [ Nav.pushUrl model.key "#/keyboard" ]
            )

        SelectKeyboard name ->
            let
                keyboard =
                    Maybe.withDefault
                        (Keyboard "" [] [] "" False False)
                        (List.head
                            (List.filter (\n -> n.name == name)
                                model.appInfo.keyboards
                            )
                        )

                currentSetup =
                    model.setupRequirement

                newRole =
                    if keyboard.split then
                        SPLIT_MASTER

                    else
                        SINGLE

                newSetup =
                    { currentSetup
                        | keyboard = keyboard
                        , layout = Maybe.withDefault "" (List.head keyboard.layout)
                        , role = newRole
                        , isLeft = True
                    }
            in
            ( { model
                | setupRequirement = newSetup
              }
            , Cmd.none
            )

        SelectLayout layout ->
            let
                currentSetup =
                    model.setupRequirement

                newSetup =
                    { currentSetup
                        | layout = layout
                    }
            in
            ( { model
                | setupRequirement = newSetup
              }
            , Cmd.none
            )

        SelectBootloader name ->
            ( { model | bootloader = Just name }, Cmd.none )

        SelectApplication name ->
            ( { model | application = Just name }, Cmd.none )

        SetProceduer ->
            ( { model
                | needsHelp = True
                , setupProcedure =
                    if useSlave model.setupRequirement.role then
                        [ "#/update/application"
                        , "#/config"
                        , "#/slave"
                        , "#/update/bootloader"
                        , "#/update/application"
                        , "#/config"
                        , "#/keymap"
                        ]

                    else
                        [ "#/update/application"
                        , "#/config"
                        , "#/keymap"
                        ]
              }
            , Cmd.batch [ Nav.pushUrl model.key "#/update/bootloader" ]
            )

        GoNextStep ->
            goNextStep model

        GoSlaveSetup ->
            let
                ( newModel, newMsg ) =
                    goNextStep model

                currentSetup =
                    newModel.setupRequirement

                newSetup =
                    { currentSetup
                        | role = SPLIT_SLAVE
                        , isLeft = not currentSetup.isLeft
                    }
            in
            ( { newModel | setupRequirement = newSetup }, newMsg )

        UseMsc enabled ->
            let
                currentSetup =
                    model.setupRequirement

                newSetup =
                    { currentSetup | disableMsc = enabled }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        UseLpme enabled ->
            let
                currentSetup =
                    model.setupRequirement

                newRole =
                    if enabled then
                        MASTER_WITH_LPME

                    else if currentSetup.role == MASTER_WITH_LPME then
                        SPLIT_MASTER

                    else
                        currentSetup.role

                newSetup =
                    { currentSetup
                        | role = newRole
                    }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        UpdateBootloader ->
            let
                cmd =
                    E.object
                        [ ( "type", E.string "bootloader" )
                        , ( "name"
                          , E.string
                                (case model.bootloader of
                                    Just bl ->
                                        bl

                                    Nothing ->
                                        Maybe.withDefault "default" (List.head (bootloaderList model))
                                )
                          )
                        , ( "disableMsc", E.bool model.setupRequirement.disableMsc )
                        ]
            in
            ( model
            , updateFirmware cmd
            )

        UpdateApplication ->
            let
                cmd =
                    E.object
                        [ ( "type", E.string "application" )
                        , ( "name"
                          , E.string
                                (case model.application of
                                    Just app ->
                                        app

                                    Nothing ->
                                        Maybe.withDefault "default" (List.head (applicationList model))
                                )
                          )
                        , ( "disableMsc", E.bool model.setupRequirement.disableMsc )
                        ]
            in
            ( model
            , updateFirmware cmd
            )

        UpdateConfig ->
            let
                cmd =
                    E.object <| setupRequirementEncoder model.setupRequirement
            in
            ( model, updateConfig cmd )

        UpdateResultMsg result ->
            ( { model
                | updateProgress =
                    updateProgressEncode result
              }
            , Cmd.none
            )

        IncrementDebounce step ->
            let
                debounce =
                    model.setupRequirement.debounce + step

                currentSetup =
                    model.setupRequirement

                newSetup =
                    if debounce < 1 then
                        { currentSetup | debounce = 1 }

                    else
                        { currentSetup | debounce = debounce }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IncrementAutoSleep step ->
            let
                autoSleep =
                    model.setupRequirement.autoSleep + step

                currentSetup =
                    model.setupRequirement

                newSetup =
                    if autoSleep < 0 then
                        { currentSetup | autoSleep = 0 }

                    else
                        { currentSetup | autoSleep = autoSleep }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IncrementPeriphInterval step ->
            let
                interval =
                    model.setupRequirement.periphInterval + step

                currentSetup =
                    model.setupRequirement

                newSetup =
                    if interval < 10 then
                        { currentSetup | periphInterval = 10 }

                    else if interval > 60 then
                        { currentSetup | periphInterval = 60 }

                    else
                        { currentSetup | periphInterval = interval }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IncrementCentralInterval step ->
            let
                interval =
                    model.setupRequirement.centralInterval + step

                currentSetup =
                    model.setupRequirement

                newSetup =
                    if interval < 10 then
                        { currentSetup | centralInterval = 10 }

                    else if interval > 60 then
                        { currentSetup | centralInterval = 60 }

                    else
                        { currentSetup | centralInterval = interval }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IsSlave bool ->
            let
                currentSetup =
                    model.setupRequirement

                newSetup =
                    if bool then
                        { currentSetup | role = SPLIT_SLAVE }

                    else
                        { currentSetup | role = SPLIT_MASTER }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IsLeft bool ->
            let
                currentSetup =
                    model.setupRequirement

                newSetup =
                    { currentSetup | isLeft = bool }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        IsJis bool ->
            let
                currentSetup =
                    model.setupRequirement

                newSetup =
                    { currentSetup | isJis = bool }
            in
            ( { model | setupRequirement = newSetup }, Cmd.none )

        UploadConfigRequested ->
            ( model
            , Select.file [ "application/json", ".JSN" ] UploadConfigSelected
            )

        UploadConfigSelected file ->
            ( model, Task.perform UploadConfigLoaded (File.toString file) )

        UploadConfigLoaded content ->
            let
                cmd =
                    E.object <|
                        setupRequirementEncoder model.setupRequirement
                            ++ [ ( "uploaded", E.string content ) ]
            in
            ( model, updateConfig cmd )


setupRequirementEncoder : SetupRequirement -> List ( String, E.Value )
setupRequirementEncoder setup =
    [ ( "keyboard", E.string setup.keyboard.name )
    , ( "layout", E.string setup.layout )
    , ( "isSplit", E.bool (isSplit setup.role) )
    , ( "isSlave", E.bool (isSlave setup.role) )
    , ( "useLpme", E.bool (useLpme setup.role) )
    , ( "isLeft", E.bool setup.isLeft )
    , ( "isJis", E.bool setup.isJis )
    , ( "debounce", E.int setup.debounce )
    , ( "centralInterval", E.int setup.centralInterval )
    , ( "periphInterval", E.int setup.periphInterval )
    , ( "autoSleep", E.int setup.autoSleep )
    ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "BLE Micro Pro Web Configurator"
    , body =
        [ Grid.container []
            (Grid.row [] [ Grid.col [] [ navbar model ] ]
                :: (if model.appInfo.webSerialEnabled then
                        []

                    else
                        [ Alert.simpleDanger [] [ text "Web serial is unavailable. Use chrome 89 or later. If you use older versions, set the #enable-experimental-web-platform-features flag in chrome://flags" ] ]
                   )
                ++ (case model.url.fragment of
                        Just "/home" ->
                            viewHome model

                        Just "/keyboard" ->
                            if model.needsHelp then
                                viewKeyboardSelect model

                            else
                                viewHome model

                        Just "/update/bootloader" ->
                            viewUpdateBootloader model

                        Just "/update/application" ->
                            viewUpdateApp model

                        Just "/keymap" ->
                            viewEditKeymap model

                        Just "/config" ->
                            viewEditConfig model

                        Just "/slave" ->
                            viewSlave model

                        _ ->
                            viewHome model
                   )
            )
        ]
    }


viewHome : Model -> List (Html Msg)
viewHome model =
    [ h4 [ Spacing.mt4, align "center" ] [ text "BLE Micro Pro Web Configurator" ]
    , Carousel.config CarouselMsg [ align "center" ]
        |> Carousel.withControls
        |> Carousel.withIndicators
        |> Carousel.slides
            [ Slide.config [] (Slide.image [] "./assets/ble_micro_pro.svg")
            ]
        |> Carousel.view model.carouselState
    , Button.linkButton
        [ Button.outlineInfo
        , Button.block
        , Button.attrs [ href "#/update/bootloader" ]
        ]
        [ text "ブートローダーをアップデートする" ]
    , Button.linkButton
        [ Button.outlineInfo
        , Button.block
        , Button.attrs [ href "#/update/application" ]
        ]
        [ text "アプリケーションをアップデートする" ]
    , Button.linkButton
        [ Button.outlineInfo
        , Button.block
        , Button.attrs [ href "#/config" ]
        ]
        [ text "キーボードごとの設定を書き込む" ]
    , Button.linkButton
        [ Button.outlineInfo
        , Button.block
        , Button.attrs [ href "#/keymap" ]
        ]
        [ text "キーマップを変更する" ]
    , Button.button
        [ Button.primary
        , Button.block
        , Button.onClick StartNavigation
        ]
        [ text "ナビゲーション付きでセットアップを開始する" ]
    , div [ align "center", Spacing.mt3 ]
        [ text <|
            " revision: "
                ++ model.appInfo.revision
        ]
    ]


disableMscCheckbox : Model -> Html Msg
disableMscCheckbox model =
    Checkbox.checkbox
        [ Checkbox.checked model.setupRequirement.disableMsc
        , Checkbox.id "msc"
        , Checkbox.onCheck UseMsc
        ]
        "Disable Mass Storage Class"


useLpmeCheckbox : Model -> Html Msg
useLpmeCheckbox model =
    Checkbox.checkbox
        [ Checkbox.checked (useLpme model.setupRequirement.role)
        , Checkbox.id "lpme"
        , Checkbox.onCheck UseLpme
        , Checkbox.disabled
            (not model.setupRequirement.keyboard.lpme)
        ]
        "Use with LPME-IO"


viewKeyboardSelect : Model -> List (Html Msg)
viewKeyboardSelect model =
    [ text "Select Keyboard"
    , Select.select
        [ Select.id "select-keyboard"
        , Select.onChange SelectKeyboard
        ]
      <|
        (Select.item [] []
            :: List.map
                (\k -> Select.item [] [ text k.name ])
                model.appInfo.keyboards
        )
    , text "Select layout"
    , Select.select
        [ Select.id "select-layout"
        , Select.onChange SelectLayout
        ]
      <|
        List.map
            (\k -> Select.item [] [ text k ])
            (keyboardLayoutList model)
    , disableMscCheckbox model
    , useLpmeCheckbox model
    , Button.button
        [ Button.primary
        , Button.block
        , Button.onClick SetProceduer
        , Button.disabled (model.setupRequirement.keyboard.name == "")
        , Button.attrs [ title "goto next step", Spacing.mt3 ]
        ]
        [ text "Next" ]
    ]


hidden : Bool -> List (Attribute msg)
hidden bool =
    if bool then
        [ style "visibility" "hidden" ]

    else
        []


isDisplay : Bool -> List (Attribute msg)
isDisplay bool =
    if bool then
        []

    else
        [ Display.none ]


updateProgressInfo : Model -> Maybe String -> Html Msg
updateProgressInfo model default =
    case model.updateProgress of
        BootloaderActivated ->
            Alert.simpleInfo
                [ Spacing.mt1 ]
                [ text "Bootloader is activated. Please push the button again." ]

        Error message ->
            Alert.simpleDanger
                [ Spacing.mt1 ]
                [ text (message ++ " Check log for datail.") ]

        Complete ->
            Alert.simpleSuccess
                [ Spacing.mt1 ]
                [ text "Update Succeeded. Go next step." ]

        _ ->
            case default of
                Just x ->
                    Alert.simpleWarning
                        [ Spacing.mt1 ]
                        [ text x ]

                _ ->
                    Alert.simpleWarning
                        [ Spacing.mt1, style "visibility" "hidden" ]
                        [ text "hidden" ]


progressSpinner : Model -> String -> List (Html Msg)
progressSpinner model default =
    case model.updateProgress of
        Updating progress ->
            [ Spinner.spinner [ Spinner.small, Spinner.attrs [ Spacing.mr2 ] ] [], text (String.fromFloat progress ++ "%") ]

        _ ->
            [ text default ]


itemsFromList : List String -> List (Select.Item msg)
itemsFromList list =
    case list of
        [] ->
            [ Select.item [] [] ]

        head :: rest ->
            Select.item [ selected True ] [ text head ]
                :: List.map
                    (\n -> Select.item [] [ text n ])
                    rest


viewUpdateFirmware : Model -> FirmwareType -> List (Html Msg)
viewUpdateFirmware model firmware =
    (case firmware of
        Bootloader ->
            [ text "Select bootloader version"
            , Select.select [ Select.id "bootloader-select", Select.onChange SelectBootloader ] <|
                itemsFromList (bootloaderList model)
            , updateProgressInfo model Nothing
            ]

        Application ->
            [ text "Select application version"
            , Select.select [ Select.id "application-select", Select.onChange SelectApplication ] <|
                itemsFromList (applicationList model)
            , updateProgressInfo model <| Just "From v0.9.4, keycode table is changed. Backup your KEYMAP.JSN for older versions before update firmware."
            ]
    )
        ++ [ disableMscCheckbox model
           , Button.button
                [ Button.primary
                , Button.block
                , Button.onClick
                    (case firmware of
                        Bootloader ->
                            UpdateBootloader

                        Application ->
                            UpdateApplication
                    )
                , Button.attrs [ Spacing.mt3 ]
                , Button.disabled
                    (case model.updateProgress of
                        Updating _ ->
                            True

                        _ ->
                            False
                    )
                ]
                (progressSpinner
                    model
                    "Update"
                )
           , Button.button
                [ Button.outlineSecondary
                , Button.block
                , Button.attrs (isDisplay model.needsHelp)
                , Button.onClick GoNextStep
                ]
                [ if model.updateProgress == Complete then
                    text "Next"

                  else
                    text "Skip"
                ]
           ]


viewUpdateBootloader : Model -> List (Html Msg)
viewUpdateBootloader model =
    viewUpdateFirmware model Bootloader


viewUpdateApp : Model -> List (Html Msg)
viewUpdateApp model =
    viewUpdateFirmware model Application


viewEditConfig : Model -> List (Html Msg)
viewEditConfig model =
    [ text "Select keyboard"
    , Select.select [ Select.onChange SelectKeyboard ] <|
        List.map
            (\n -> Select.item [] [ text n ])
            (filterOrAll
                (if model.needsHelp then
                    model.setupRequirement.keyboard.name

                 else
                    ""
                )
                ([ "", "upload your own" ]
                    ++ List.map
                        (\k -> k.name)
                        model.appInfo.keyboards
                )
            )
    , text "Select layout"
    , Select.select
        [ Select.id "select-layout"
        , Select.onChange SelectLayout
        ]
      <|
        List.map
            (\k -> Select.item [] [ text k ])
            (keyboardLayoutList model)
    , div (hidden (model.setupRequirement.keyboard.name == ""))
        [ useLpmeCheckbox model
        , Checkbox.checkbox
            [ Checkbox.disabled (not model.setupRequirement.keyboard.split)
            , Checkbox.checked (isSlave model.setupRequirement.role)
            , Checkbox.onCheck IsSlave
            , Checkbox.id "is-slave"
            ]
            "Is Slave"
        , Checkbox.checkbox
            [ Checkbox.disabled (not model.setupRequirement.keyboard.split)
            , Checkbox.checked model.setupRequirement.isLeft
            , Checkbox.onCheck IsLeft
            , Checkbox.id "is-left"
            ]
            "Is Left"
        , Checkbox.checkbox
            [ Checkbox.checked model.setupRequirement.isJis
            , Checkbox.onCheck IsJis
            , Checkbox.id "print-jis"
            ]
            "Show keymap.json using JP_XX"
        , lableWithHelp "Debounce" "Matrix scan debounce setting"
        , spinBox model.setupRequirement.debounce
            ""
            (IncrementDebounce 1)
            (IncrementDebounce -1)
        , lableWithHelp "AutoSleep" "Auto sleep timeout setting. Set 0 to disable"
        , spinBox model.setupRequirement.autoSleep
            " min."
            (IncrementAutoSleep 10)
            (IncrementAutoSleep -10)
        , lableWithHelp "Connection inteval (Peripheral)" "Connection interval with PC or master side"
        , spinBox model.setupRequirement.periphInterval
            " ms"
            (IncrementPeriphInterval 5)
            (IncrementPeriphInterval -5)
        , div
            (hidden
                (not (useSlave model.setupRequirement.role))
            )
            [ lableWithHelp
                "Connection inteval (Central)"
                "Connection interval with slave side"
            , spinBox model.setupRequirement.centralInterval
                " ms"
                (IncrementCentralInterval 5)
                (IncrementCentralInterval -5)
            ]
        ]
    , updateProgressInfo model Nothing
    , Button.button
        [ Button.primary
        , Button.block
        , Button.attrs [ Spacing.mt3 ]
        , if model.setupRequirement.keyboard.name == "" then
            Button.onClick UploadConfigRequested

          else
            Button.onClick UpdateConfig
        , Button.disabled
            (case model.updateProgress of
                Updating _ ->
                    True

                _ ->
                    False
            )
        ]
        (progressSpinner
            model
            "Update"
        )
    , Button.button
        [ Button.outlineSecondary
        , Button.block
        , Button.attrs (isDisplay model.needsHelp)
        , Button.onClick GoNextStep
        ]
        [ if model.updateProgress == Complete then
            text "Next"

          else
            text "Skip"
        ]
    ]


spinBoxButton : String -> msg -> Html msg
spinBoxButton str message =
    Button.button
        [ Button.outlineInfo
        , Button.onClick message
        , Button.attrs [ style "width" "60px" ]
        ]
        [ strong [] [ text str ] ]


lableWithHelp : String -> String -> Html msg
lableWithHelp label help =
    div [ Spacing.mt1 ]
        [ text label
        , Button.button
            [ Button.primary
            , Button.disabled True
            , Button.small
            , Button.attrs
                [ title help
                , class "rounded-circle p-0"
                , style "width" "1.5rem"
                , style "height" "1.5rem"
                , style "margin-left" ".2rem"
                ]
            ]
            [ strong [] [ text "?" ] ]
        ]


spinBox : Int -> String -> msg -> msg -> Html msg
spinBox value unit increment decrement =
    div [ Spacing.mb1 ]
        [ spinBoxButton "+" increment
        , node "text" [ Spacing.ml2, Spacing.mr2 ] [ text (String.fromInt value ++ unit) ]
        , spinBoxButton "-" decrement
        ]


viewEditKeymap : Model -> List (Html Msg)
viewEditKeymap model =
    [ a [ href "https://sekigon-gonnoc.github.io/qmk_configurator", target "_blank" ] [ text "Move to QMK Configurator" ]
    ]


viewSlave : Model -> List (Html Msg)
viewSlave model =
    [ text "スレーブ側のBLE Micro Proに差し替えてください"
    , Button.button
        [ Button.outlineSecondary
        , Button.block
        , Button.attrs (isDisplay model.needsHelp)
        , Button.onClick GoSlaveSetup
        ]
        [ text "Next" ]
    ]


filterOrAll : String -> List String -> List String
filterOrAll key list =
    let
        res =
            List.filter (\m -> String.contains key m) list
    in
    if List.isEmpty res then
        list

    else
        res


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.items
            [ Navbar.itemLink [ href "#/home" ] [ text "Home" ]

            -- , Navbar.itemLink [ href "/keyboard" ] [ text "Select Keyboard" ]
            , Navbar.itemLink [ href "#/update/bootloader" ] [ text "Update Bootloader" ]
            , Navbar.itemLink [ href "#/update/application" ] [ text "Update Application" ]
            , Navbar.itemLink [ href "#/config" ] [ text "Edit Config" ]
            , Navbar.itemLink [ href "#/keymap" ] [ text "Edit Keymap" ]
            ]
        |> Navbar.view model.navbarState


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]

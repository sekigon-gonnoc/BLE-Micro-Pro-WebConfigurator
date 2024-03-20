port module App exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
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


port updateEeprom : E.Value -> Cmd msg


port updatePairList : Int -> Cmd msg


port advertiseToDevice : ( Int, Int ) -> Cmd msg


port deletePairing : ( Int, Int ) -> Cmd msg


port updateResult : (E.Value -> msg) -> Sub msg


port updatePairListResult : (E.Value -> msg) -> Sub msg


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
    , bootloader : Maybe String
    , application : Maybe String
    , updateProgress : UpdateProgress
    , device1PairList : PairList
    , device2PairList : PairList
    , filterText : String
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
    , uploadLabel : String -- "upload your own" in the selection list
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
    , role : BlemRole
    , isLeft : Bool
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


type alias Pairing =
    { id : Int
    , pairType : String
    , name : String
    , mac : String
    }


type alias PairList =
    { device : Int, pairs : List Pairing }


flagDecoder : Decoder Flag
flagDecoder =
    D.map6 Flag
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
        (field "uploadLabel" string)


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


updatePairListEncode : E.Value -> PairList
updatePairListEncode json =
    let
        decoder =
            D.map2 PairList (field "device" D.int) (field "pairs" (D.list <| D.map4 Pairing (field "id" D.int) (field "type" D.string) (field "name" D.string) (field "mac" D.string)))
    in
    Result.withDefault { device = 0, pairs = [ { id = 1, pairType = "Error", name = "Error", mac = "" } ] } (D.decodeValue decoder json)


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
                    Flag "" False [] [] [] "(uploadLabel)"
      , needsHelp = False
      , setupRequirement =
            { keyboard = Keyboard "" [] [] "" False False
            , role = SINGLE
            , isLeft = True
            , disableMsc = False
            , debounce = 1
            , centralInterval = 30
            , periphInterval = 30
            , autoSleep = 0
            }
      , setupProcedure = []
      , bootloader = Nothing
      , application = Nothing
      , updateProgress = None
      , device1PairList = { device = 0, pairs = [] }
      , device2PairList = { device = 1, pairs = [] }
      , filterText = ""
      }
    , Cmd.batch
        [ navbarCmd
        ]
    )


applicationList : Model -> List String
applicationList model =
    filterList
        model.setupRequirement.keyboard.firmware
        model.appInfo.applications


bootloaderList : Model -> List String
bootloaderList model =
    model.appInfo.bootloaders



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , updateResult UpdateResultMsg
        , updatePairListResult UpdatePairListMsg
        ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavbarMsg Navbar.State
    | StartNavigation
    | SelectKeyboard String
    | SelectBootloader String
    | SelectApplication String
    | InputKeyboardFilter String
    | SetProceduer
    | GoNextStep
    | GoSlaveSetup
    | UseMsc Bool
    | UseLpme Bool
    | UpdateBootloader
    | UpdateApplication
    | UpdateConfig
    | UpdateEeprom
    | UpdateResultMsg E.Value
    | UpdateDevice1PairList
    | UpdateDevice2PairList
    | UpdatePairListMsg E.Value
    | AdvertiseToDevice Int Int
    | DeletePairing Int Int
    | IncrementDebounce Int
    | IncrementAutoSleep Int
    | IncrementPeriphInterval Int
    | IncrementCentralInterval Int
    | IsSlave Bool
    | IsLeft Bool


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
                    case url.path of
                        "/legacy" ->
                            ( model, Nav.load "legacy" )

                        _ ->
                            ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , updateProgress = None
              }
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        StartNavigation ->
            ( { model | needsHelp = True }
            , Cmd.batch [ Nav.pushUrl model.key "#/keyboard" ]
            )

        SelectKeyboard name ->
            ( updateKeyboard model name, Cmd.none )

        SelectBootloader name ->
            ( { model | bootloader = Just name }, Cmd.none )

        SelectApplication name ->
            ( { model | application = Just name }, Cmd.none )

        InputKeyboardFilter name ->
            let
                keyboard =
                    List.head <| filterList name <| List.map (\k -> k.name) model.appInfo.keyboards

                newModel =
                    updateKeyboard model <| Maybe.withDefault "" keyboard
            in
            ( { newModel | filterText = name }, Cmd.none )

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
                        ]

                    else
                        [ "#/update/application"
                        , "#/config"
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

        UpdateEeprom ->
            let
                cmd =
                    E.object <| setupRequirementEncoder model.setupRequirement
            in
            ( model, updateEeprom cmd )

        UpdateResultMsg result ->
            ( { model
                | updateProgress =
                    updateProgressEncode result
              }
            , Cmd.none
            )

        UpdateDevice1PairList ->
            ( model, updatePairList 0 )

        UpdateDevice2PairList ->
            ( model, updatePairList 1 )

        UpdatePairListMsg result ->
            let
                newPairList =
                    updatePairListEncode result

                newModel =
                    if newPairList.device == 0 then
                        { model | device1PairList = newPairList }

                    else
                        { model | device2PairList = newPairList }
            in
            ( newModel, Cmd.none )

        AdvertiseToDevice dev id ->
            ( model, advertiseToDevice ( dev, id ) )

        DeletePairing dev id ->
            ( model, deletePairing ( dev, id ) )

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


setupRequirementEncoder : SetupRequirement -> List ( String, E.Value )
setupRequirementEncoder setup =
    [ ( "keyboard", E.string setup.keyboard.name )
    , ( "isSplit", E.bool (isSplit setup.role) )
    , ( "isSlave", E.bool (isSlave setup.role) )
    , ( "useLpme", E.bool (useLpme setup.role) )
    , ( "isLeft", E.bool setup.isLeft )
    , ( "debounce", E.int setup.debounce )
    , ( "centralInterval", E.int setup.centralInterval )
    , ( "periphInterval", E.int setup.periphInterval )
    , ( "autoSleep", E.int setup.autoSleep )
    ]


updateKeyboard : Model -> String -> Model
updateKeyboard model name =
    let
        keyboard =
            Maybe.withDefault
                (Keyboard name [] [] "" False False)
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
                , role = newRole
                , isLeft = True
            }
    in
    { model | setupRequirement = newSetup }



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

                        Just "/pairing" ->
                            viewPairing model

                        Just "/slave" ->
                            viewSlave model

                        _ ->
                            viewHome model
                   )
                ++ viewFooter model
            )
        ]
    }


viewFooter : Model -> List (Html Msg)
viewFooter model =
    [ div [ align "center", Spacing.mt3 ]
        [ a [ href "/legacy" ] [ text "For old firmware(<1.0.0)" ]
        ]
    ]


viewHome : Model -> List (Html Msg)
viewHome model =
    [ h4 [ Spacing.mt4, align "center" ] [ text "BLE Micro Pro Web Configurator" ]
    , img [ src "assets/ble_micro_pro.svg", style "display" "block", style "margin" "auto" ] []
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
            (not model.setupRequirement.keyboard.lpme && model.setupRequirement.keyboard.name /= model.appInfo.uploadLabel)
        ]
        "Use with LPME-IO"


viewKeyboardSelect : Model -> List (Html Msg)
viewKeyboardSelect model =
    [ text "Select Keyboard"
    , Input.text [ Input.onInput InputKeyboardFilter, Input.attrs [ Html.Attributes.value model.filterText ], Input.placeholder "Filter text" ]
    , Select.select [ Select.onChange SelectKeyboard, Select.attrs [ Html.Attributes.value model.setupRequirement.keyboard.name ] ] <|
        (Select.item [] []
            :: List.map
                (\k -> Select.item [] [ text k.name ])
                model.appInfo.keyboards
        )
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

        _ :: _ ->
            Select.item [] []
                :: List.map
                    (\n -> Select.item [] [ text n ])
                    list


viewUpdateFirmware : Model -> FirmwareType -> List (Html Msg)
viewUpdateFirmware model firmware =
    (case firmware of
        Bootloader ->
            [ text "Select bootloader version"
            , Select.select [ Select.id "bootloader-select", Select.onChange SelectBootloader, Select.attrs [ Html.Attributes.value <| Maybe.withDefault "" model.bootloader ] ] <|
                itemsFromList (bootloaderList model)
            , updateProgressInfo model Nothing
            ]

        Application ->
            [ text "Select application version"
            , Select.select [ Select.id "application-select", Select.onChange SelectApplication, Select.attrs [ Html.Attributes.value <| Maybe.withDefault "" model.application ] ] <|
                itemsFromList (applicationList model)
            , updateProgressInfo model Nothing
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
    , Input.text [ Input.onInput InputKeyboardFilter, Input.attrs [ Html.Attributes.value model.filterText ], Input.placeholder "Filter text" ]
    , Select.select [ Select.onChange SelectKeyboard, Select.attrs [ Html.Attributes.value model.setupRequirement.keyboard.name ] ] <|
        List.map
            (\n -> Select.item [] [ text n ])
            (filterList
                (if model.needsHelp then
                    model.setupRequirement.keyboard.name

                 else
                    model.filterText
                )
                ([ "", model.appInfo.uploadLabel ]
                    ++ List.map
                        (\k -> k.name)
                        model.appInfo.keyboards
                )
            )
    , div (hidden (model.setupRequirement.keyboard.name == ""))
        [ useLpmeCheckbox model
        , Checkbox.checkbox
            [ Checkbox.disabled (not model.setupRequirement.keyboard.split && model.setupRequirement.keyboard.name /= model.appInfo.uploadLabel)
            , Checkbox.checked (isSlave model.setupRequirement.role)
            , Checkbox.onCheck IsSlave
            , Checkbox.id "is-slave"
            ]
            "Is Slave"
        , Checkbox.checkbox
            [ Checkbox.disabled (not model.setupRequirement.keyboard.split && model.setupRequirement.keyboard.name /= model.appInfo.uploadLabel)
            , Checkbox.checked model.setupRequirement.isLeft
            , Checkbox.onCheck IsLeft
            , Checkbox.id "is-left"
            ]
            "Is Left"
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
        , lableWithHelp "Connection interval (Peripheral)" "Connection interval with PC or master side"
        , spinBox model.setupRequirement.periphInterval
            " ms"
            (IncrementPeriphInterval 5)
            (IncrementPeriphInterval -5)
        , div
            (hidden
                (not (useSlave model.setupRequirement.role))
            )
            [ lableWithHelp
                "Connection interval (Central)"
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
        , Button.onClick UpdateConfig
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
    [ text "Select keyboard"
    , Input.text [ Input.onInput InputKeyboardFilter, Input.attrs [ Html.Attributes.value model.filterText ], Input.placeholder "Filter text" ]
    , Select.select [ Select.onChange SelectKeyboard, Select.attrs [ Html.Attributes.value model.setupRequirement.keyboard.name ] ] <|
        List.map
            (\n -> Select.item [] [ text n ])
            (filterList
                (if model.needsHelp then
                    model.setupRequirement.keyboard.name

                 else
                    model.filterText
                )
                ([ "", model.appInfo.uploadLabel ]
                    ++ List.filterMap
                        (\k ->
                            if List.length k.keymap > 0 then
                                Just k.name

                            else
                                Nothing
                        )
                        model.appInfo.keyboards
                )
            )
    , updateProgressInfo model Nothing
    , Button.button
        [ Button.primary
        , Button.block
        , Button.attrs [ Spacing.mt3 ]
        , Button.onClick UpdateEeprom
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
    , div []
        [ text "Use "
        , a [ href "https://remap-keys.app/", target "_blank" ] [ text "Remap" ]
        , text " or "
        , a [ href "https://vial.rocks/", target "_blank" ] [ text "Vial" ]
        , text " to edit keymap"
        ]
    ]


viewPair : Int -> PairList -> List (Html Msg)
viewPair device pairList =
    List.map
        (\p ->
            div []
                [ h5 [ Spacing.mt2 ] [ text (p.name ++ " " ++ p.pairType) ]
                , Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ Spacing.ml3 ]
                    , Button.onClick (AdvertiseToDevice device p.id)
                    ]
                    [ text "Connect" ]
                , Button.button
                    [ Button.outlineDanger
                    , Button.attrs [ Spacing.ml1 ]
                    , Button.onClick (DeletePairing device p.id)
                    ]
                    [ text "Delete" ]
                ]
        )
        pairList.pairs


viewPairing : Model -> List (Html Msg)
viewPairing model =
    viewPair 0 model.device1PairList
        ++ [ Button.button
                [ Button.primary
                , Button.block
                , Button.attrs [ Spacing.mt3 ]
                , Button.onClick UpdateDevice1PairList
                ]
                [ text "Update Device1 Pair List" ]
           , Button.button
                [ Button.primary
                , Button.block
                , Button.attrs [ Spacing.mt1 ]
                , Button.onClick (AdvertiseToDevice 0 255)
                ]
                [ text "Pair Device1 with New Device" ]
           ]
        ++ viewPair 1 model.device2PairList
        ++ [ Button.button
                [ Button.primary
                , Button.block
                , Button.attrs [ Spacing.mt3 ]
                , Button.onClick UpdateDevice2PairList
                ]
                [ text "Update Device2 Pair List" ]
           , Button.button
                [ Button.primary
                , Button.block
                , Button.attrs [ Spacing.mt1 ]
                , Button.onClick (AdvertiseToDevice 1 255)
                ]
                [ text "Pair Device2 with New Device" ]
           ]
        ++ [ updateProgressInfo model Nothing ]


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


filterList : String -> List String -> List String
filterList key list =
    List.filter (\m -> String.contains key m) list


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.items
            [ makeNavItem model.url.fragment "#/home" "Home"
            , makeNavItem model.url.fragment "#/update/bootloader" "Update Bootloader"
            , makeNavItem model.url.fragment "#/update/application" "Update Application"
            , makeNavItem model.url.fragment "#/config" "Edit config"
            , makeNavItem model.url.fragment "#/keymap" "Write default keymap"
            , makeNavItem model.url.fragment "#/pairing" "Pairing"
            ]
        |> Navbar.view model.navbarState


makeNavItem : Maybe String -> String -> String -> Navbar.Item msg
makeNavItem urlFragment link str =
    if String.concat [ "#", Maybe.withDefault "" urlFragment ] == link then
        Navbar.itemLink [ href link, style "font-weight" "bold" ] [ text str ]

    else
        Navbar.itemLink [ href link ] [ text str ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]

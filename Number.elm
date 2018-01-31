module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Charty.Color as ChartyColor
import Charty.LineChart as LineChart
import Color as Color
import FontAwesome as FontAwesome
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json
import Maybe exposing (withDefault)
import Navigation exposing (Location)
import Random as Random
import Svg as Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, transform, width)
import Svg.Events
import UrlParser exposing ((</>))
import Visualization.Axis as Axis
import Visualization.Scale as Scale
import Visualization.Shape as Shape


type Msg
    = NeueEingabe String
    | Taste Int
    | DenkeZahlAus
    | NeueGeheimeZahl Int
    | MenuMsg Navbar.State
    | UrlChange Location
    | MouseClick


type alias Model =
    { geheimeZahl : Maybe Int
    , bereich : ( Int, Int )
    , eingabe : String
    , zahl : Maybe Int
    , ausgabe : String
    , geschichte : List ( Int, Html Msg )
    , menuState : Navbar.State
    , page : Page
    }


neueModel : Navbar.State -> Model
neueModel navbarState =
    let
        bereich =
            ( 1, 100 )
    in
    { bereich = bereich
    , geheimeZahl = Nothing
    , zahl = Nothing
    , eingabe = ""
    , ausgabe = toAusgabe bereich Start
    , geschichte = []
    , menuState = navbarState
    , page = Zahl
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navCmd ) =
            Navbar.initialState MenuMsg

        ( model, urlCmd ) =
            urlUpdate location (neueModel navbarState)
    in
    ( model, Cmd.batch [ navCmd, denkeZahlAus model ] )


denkeZahlAus : Model -> Cmd Msg
denkeZahlAus model =
    Random.generate NeueGeheimeZahl (Random.int (Tuple.first model.bereich) (Tuple.second model.bereich))


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Json.map tagger Events.keyCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        NeueEingabe txt ->
            ( { model | eingabe = txt }, Cmd.none )

        Taste key ->
            if key == 13 then
                -- Enter
                let
                    ( zahl, fehler ) =
                        case model.eingabe |> String.toInt of
                            Err fehler ->
                                ( Nothing, Just fehler )

                            Ok zahl ->
                                ( Just zahl, Nothing )

                    ergebnis =
                        pruefeEingabe model

                    neueGeschichte =
                        let
                            fuegeHinzu =
                                zahl |> Maybe.map (\x -> [ ( x, toHtml ergebnis ) ]) |> Maybe.withDefault []
                        in
                        case ergebnis of
                            Fehler _ ->
                                []

                            Ausserhalb _ ->
                                []

                            Start ->
                                []

                            Zuklein ->
                                fuegeHinzu

                            Zugross ->
                                fuegeHinzu

                            Treffer ->
                                fuegeHinzu
                in
                ( { model
                    | zahl = zahl
                    , ausgabe = toAusgabe model.bereich ergebnis
                    , eingabe = ""
                    , geschichte = model.geschichte ++ neueGeschichte
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        NeueGeheimeZahl zahl ->
            ( { model | geheimeZahl = Just zahl }, Cmd.none )

        DenkeZahlAus ->
            ( neueModel model.menuState, denkeZahlAus model )

        MenuMsg nav ->
            ( model, Cmd.none )

        UrlChange location ->
            urlUpdate location model

        MouseClick ->
            ( model, Cmd.none )


type Ergebnis
    = Fehler String
    | Zuklein
    | Zugross
    | Treffer
    | Start
    | Ausserhalb Int


fontSize : Int
fontSize =
    14


toHtml : Ergebnis -> Html msg
toHtml ergebnis =
    case ergebnis of
        Fehler _ ->
            FontAwesome.warning Color.red fontSize

        Zuklein ->
            FontAwesome.arrow_up Color.gray fontSize

        Zugross ->
            FontAwesome.arrow_down Color.gray fontSize

        Treffer ->
            FontAwesome.bullseye Color.green fontSize

        Start ->
            FontAwesome.play Color.black fontSize

        Ausserhalb _ ->
            FontAwesome.warning Color.yellow fontSize


toAusgabe : ( Int, Int ) -> Ergebnis -> String
toAusgabe bereich ergebnis =
    case ergebnis of
        Fehler meldung ->
            meldung

        Zuklein ->
            "Zu klein"

        Zugross ->
            "Zu groß"

        Treffer ->
            "Teffer!"

        Start ->
            "Ich habe mir eine Zahl zwischen " ++ toString (Tuple.first bereich) ++ " und " ++ toString (Tuple.second bereich) ++ " ausgedacht!"

        Ausserhalb zahl ->
            "Die Zahl " ++ toString zahl ++ " liegt außerhalb des Zahlenbreiches: zwischen " ++ toString (Tuple.first bereich) ++ " und " ++ toString (Tuple.second bereich)


pruefeEingabe : Model -> Ergebnis
pruefeEingabe model =
    let
        zahl =
            model.eingabe |> String.toInt |> Result.toMaybe

        meldung zahl geheimeZahl =
            if zahl < Tuple.first model.bereich || Tuple.second model.bereich < zahl then
                Ausserhalb zahl
            else if zahl < geheimeZahl then
                Zuklein
            else if zahl > geheimeZahl then
                Zugross
            else
                Treffer
    in
    case ( zahl, model.geheimeZahl ) of
        ( Nothing, Nothing ) ->
            Start

        ( Just z, Just g ) ->
            meldung z g

        ( Nothing, Just _ ) ->
            Fehler "keine Zahl"

        ( Just _, Nothing ) ->
            Fehler "keine geheime Zahl"


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , CDN.fontAwesome
        , viewNavigation model
        , viewMainContent model
        ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    div [ style [ ( "padding-top", "70px" ) ] ]
        [ Grid.container [] <|
            case model.page of
                Zahl ->
                    pageZahlenRaten model

                GettingStarted ->
                    [ text "getting started" ]

                NotFound ->
                    [ text "not found" ]
        ]


pageZahlenRaten : Model -> List (Html Msg)
pageZahlenRaten model =
    let
        gesch =
            List.concatMap (\( n, h ) -> [ text (toString n), h ]) model.geschichte
    in
    [ Grid.row []
        [ Grid.col [ Col.sm6, Col.lg6 ]
            [ Grid.row [] [ Grid.col [] [ text model.ausgabe ] ]
            , Grid.row []
                [ Grid.col []
                    [ Input.text
                        [ Input.onInput NeueEingabe
                        , Input.attrs [ onKeyDown Taste ]
                        , Input.attrs [ value model.eingabe ]
                        , Input.attrs [ placeholder "Dein Versuch" ]
                        ]
                    ]
                ]
            ]
        , Grid.col [ Col.sm6, Col.lg6 ]
            [ Grid.row [] [ Grid.col [ Col.lg12, Col.sm12 ] gesch ]
            , Grid.row [] [ Grid.col [ Col.lg12, Col.sm12 ] [ chart2 model ] ]

            -- , Grid.row [] [ Grid.col [ Col.lg12, Col.sm12 ] [ chart model ] ]
            ]
        ]
    ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    Navbar.config MenuMsg
        -- |> Navbar.withAnimation
        |> Navbar.fixTop
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ FontAwesome.search Color.black 15, text " Menü" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Getting started" ]
            , Navbar.itemLink [ href "#modules" ] [ text "Modules" ]
            ]
        |> Navbar.customItems
            (if model.page == Zahl then
                [ Navbar.customItem (Button.button [ Button.info, Button.onClick DenkeZahlAus ] [ text "Neues Spiel!" ]) ]
             else
                []
            )
        |> Navbar.view model.menuState


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.menuState MenuMsg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- Navi


type Page
    = Zahl
    | GettingStarted
    | NotFound


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Zahl UrlParser.top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        ]


exampleData : List ( Float, Float )
exampleData =
    List.indexedMap (\i x -> ( toFloat i, toFloat x )) [ 100, 1, 11, 30, 40, 11 ]


chart : Model -> Html Msg
chart model =
    let
        data =
            -- exampleData
            model.geschichte |> List.indexedMap (\i ( v, _ ) -> ( toFloat i, toFloat v ))

        dataset =
            [ { label = "input"
              , data = data
              }
            ]

        defaults =
            { drawPoints = True
            , background = "transparent"
            , colorAssignment = ChartyColor.assignDefaults
            , labelPrecision = 0
            , drawLabels = False
            }
    in
    LineChart.view defaults dataset


movePointByPadding : number -> ( number, number ) -> ( number, number )
movePointByPadding padding ( x, y ) =
    ( x + padding, y + padding )


chart2 : Model -> Svg Msg
chart2 model =
    let
        data =
            -- exampleData
            model.geschichte |> List.indexedMap (\i ( v, _ ) -> ( toFloat i, toFloat v ))

        w =
            470

        h =
            370

        padding =
            60

        translate x y =
            Svg.Attributes.transform <| "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        translatePoint ( x, y ) =
            translate x y

        px x =
            toString x ++ "px"

        xScale : Scale.ContinuousScale
        xScale =
            Scale.linear ( 1, 20 ) ( 0, w - 2 * padding )

        xAxisOptions =
            { defaultOptions | orientation = Axis.Bottom, tickCount = 20 }

        defaultOptions =
            Axis.defaultOptions

        yScale : Scale.ContinuousScale
        yScale =
            Scale.linear ( 1, 100 ) ( h - 2 * padding, 0 )

        -- yAxisOptions =
        yAxisOptions =
            { defaultOptions | orientation = Axis.Left, tickCount = 20 }

        transformToLineData ( x, y ) =
            ( Scale.convert xScale (x + 1), Scale.convert yScale y )

        points model =
            List.map transformToLineData data

        line model =
            data
                |> List.map (\x -> transformToLineData x |> Just)
                |> Shape.line Shape.linearCurve
                |> d

        circle : String
        circle =
            Shape.arc
                { innerRadius = 0
                , outerRadius = 3
                , cornerRadius = 0
                , startAngle = 0
                , endAngle = 2 * pi
                , padAngle = 0
                , padRadius = 0
                }
    in
    svg [ Svg.Attributes.width (px w), Svg.Attributes.height (px h) ]
        [ g [ translate (padding - 1) (h - padding) ]
            [ Axis.axis xAxisOptions xScale ]
        , g [ translate (padding - 1) padding ]
            [ Axis.axis yAxisOptions yScale ]
        , g [ translate padding padding ]
            [ Svg.path [ line model, stroke "red", strokeWidth "1px", fill "none" ] []
            ]
        , g [] <| List.map (\point -> Svg.path [ Svg.Events.on "click" (Json.succeed MouseClick), d circle, fill "white", stroke "blue", translatePoint (movePointByPadding padding point) ] []) (points model)
        ]

module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Texture exposing (Texture)
import Color exposing (Color)
import Css
import Css.Global
import Css.Media
import Expression exposing (Expression(..), FunctionName(..))
import Html.Lazy
import Html.Styled exposing (Html, button, div, li, p, styled, text, toUnstyled, ul, span)
import Html.Styled.Attributes exposing (href, target)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy
import Maybe
import Time
import Url
import Task
import Array exposing (Array)
import Array.Extra as Array


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\x y -> UpdateWindowWidth x)
        , Browser.Events.onAnimationFrame AnimationFrame
        ]


type Load a
    = Loading
    | Failure
    | Success a


loadMap2 : (a -> b -> c) -> Load a -> Load b -> Load c
loadMap2 f la lb =
    case (la, lb) of
        (Failure, _) -> Failure
        (_, Failure) -> Failure
        (Success a, Success b) -> Success (f a b)
        _ -> Loading


type alias Model =
    { input : String
    , parsedExpression : Maybe Expression
    , expressionError : Maybe String
    , computedValues : Array Float
    , animatedValues : Array Float
    , lastFrameTime : Int
    , spriteHead : Load Texture
    , spriteTail : Load Texture
    , smoothEnds : Bool
    , extraLong : Bool
    , windowWidth : Int
    , requestFrame : Bool
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialExpression =
            Product (FunctionCall FuncSin VarX) (ConstValue 20.0)
        initialPoints =
            calculatePoints initialExpression True 10
        state =
            { input = "sin x * 20"
            , parsedExpression = Just initialExpression
            , expressionError = Nothing
            , computedValues = initialPoints
            , animatedValues = initialPoints
            , lastFrameTime = 0
            , spriteHead = Loading
            , spriteTail = Loading
            , smoothEnds = True
            , extraLong = False
            , windowWidth = 200
            , requestFrame = True
            }
    in
    ( state
    , Dom.getViewport
      |> Task.perform
        (\viewport -> UpdateWindowWidth <| round viewport.scene.width)
    )


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | InputChanged String
    | AnimationFrame Time.Posix
    | TextureLoadedHead (Maybe Texture)
    | TextureLoadedTail (Maybe Texture)
    | SetSmoothing Bool
    | SetExtraLong Bool
    | UpdateWindowWidth Int


fWeightedAverage : (Float -> Float) -> (Float -> Float) -> ( Float, Float ) -> Float -> Float
fWeightedAverage fa fb ( xl, xr ) x =
    if x <= xl then
        fa x

    else if x >= xr then
        fb x

    else
        let
            k =
                (cos ((xr - x) / (xr - xl) * pi) + 1) / 2
        in
        fa x * (1 - k) + fb x * k


calculatePoints : Expression -> Bool -> Int -> Array Float
calculatePoints expression smooth maxValue =
    let
        smoothRange =
            1.5

        numSamples =
            maxValue * 20

        rawValue : Float -> Float
        rawValue x =
            Expression.evaluate expression x

        x0 =
            rawValue 0

        xl =
            rawValue (toFloat maxValue)

        smoothValue : Float -> Float
        smoothValue x =
            if smooth then
                fWeightedAverage
                    (fWeightedAverage (\_ -> x0) rawValue ( 0, smoothRange ))
                    (\_ -> xl)
                    ( toFloat maxValue - smoothRange, toFloat maxValue )
                    x

            else
                rawValue x
    in
    Array.initialize numSamples
        (\s ->
            smoothValue (toFloat s / toFloat numSamples * toFloat maxValue)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal href ->
                    ( model, Nav.load (Url.toString href) )

                Browser.External href ->
                    ( model, Nav.load href )

        TextureLoadedTail Nothing ->
            ( { model | spriteTail = Failure }, Cmd.none )

        TextureLoadedHead Nothing ->
            ( { model | spriteHead = Failure }, Cmd.none )

        TextureLoadedTail (Just texture) ->
            ( { model | spriteTail = Success texture }, Cmd.none )

        TextureLoadedHead (Just texture) ->
            ( { model | spriteHead = Success texture }, Cmd.none )

        UpdateWindowWidth width ->
            ( { model | windowWidth = width }, Cmd.none )

        InputChanged newInput ->
            let
                newModel =
                    case Expression.parse newInput of
                        Err error ->
                            { model
                                | input = newInput

                                -- TODO: Better error messages
                                , expressionError = Just "error"
                            }

                        Ok expression ->
                            { model
                                | input = newInput
                                , parsedExpression = Just expression
                                , expressionError = Nothing
                            }
                            |> updateComputedValues
            in
            ( newModel, Cmd.none )

        AnimationFrame time ->
            -- Ideally this should be moved to the subscriptions function,
            -- but that was producing some weird behaviour so it's here instead
            if not model.requestFrame
            then
                ( model, Cmd.none )
            else
                -- Attempting to create as few extra objects here as possible
                -- Array.Extra.map2 resorts to conversions to lists, which is
                -- pretty bad in that reagrd
                let
                    newPoints =
                        Array.initialize (Array.length model.computedValues)
                            (\i ->
                                let
                                    target =
                                        Array.get i model.computedValues
                                        |> Maybe.withDefault 0
                                        |> clamp -2000 2000
                                    cur =
                                        Array.get i model.animatedValues
                                        |> Maybe.withDefault target
                                    diff = abs (cur - target)
                                    sign = if cur < target then -1.0 else 1.0
                                    timeGap = toFloat ((Time.posixToMillis time) - model.lastFrameTime) / 100
                                in
                                if diff < 0.2 then
                                    target
                                else
                                    (cur * 9 + target) / 10   
                            )
                in
                ( { model
                    | lastFrameTime = time |> Time.posixToMillis
                    , animatedValues = newPoints
                    , requestFrame = newPoints /= model.computedValues
                  }
                , Cmd.none
                )

        SetSmoothing smooth ->
            ( { model | smoothEnds = smooth } |> updateComputedValues
            , Cmd.none
            )

        SetExtraLong long ->
            ( { model | extraLong = long } |> updateComputedValues
            , Cmd.none
            )


updateComputedValues : Model -> Model
updateComputedValues model =
    let
        maxValue =
            if model.extraLong then 50 else 10
    in
    { model
    | computedValues =
        model.parsedExpression
        |> Maybe.map (\e -> calculatePoints e model.smoothEnds maxValue)
        |> Maybe.withDefault model.computedValues
    , requestFrame = True
    }


path1 points =
    case points of
        [] ->
            Canvas.path ( 0, 0 ) []

        x :: xr ->
            Canvas.path
                x
                (List.map Canvas.lineTo xr)


thiccLine : Float -> Float -> Array Float -> Float -> Float -> Color.Color -> Canvas.Renderable
thiccLine y l points o1 o2 color =
    let
        pointsOnCanvas =
            (++)
                ( points
                  |> Array.indexedMap
                    (\i v -> ( l + v + o1, y + 256 + toFloat i * 2 ))
                  |> Array.toList
                )
                ( points
                  |> Array.indexedMap
                    (\i v -> ( l + v + o2, y + 256 + toFloat i * 2 ))
                  |> Array.toList
                  |> List.reverse
                )
    in
    Canvas.shapes
        [ Canvas.Settings.fill color ]
        [ path1 pointsOnCanvas ]


viewSpriteAt ( x, y ) sprite =
    Canvas.texture
        [ Canvas.Settings.Advanced.transform
            [ Canvas.Settings.Advanced.translate x y
            , Canvas.Settings.Advanced.scale 0.5 0.5
            , Canvas.Settings.Advanced.translate -x -y
            ]
        ]
        ( x, y )
        sprite

viewCanvas points spriteHead spriteTail w =
    let
        o =
            0

        y =
            15

        h =
            520 + (Array.length points) * 2

        x0 =
            Array.get 0 points
                |> Maybe.withDefault 0
                |> (\x -> x - 133 + (w / 2) - o)

        xl =
            Array.get (Array.length points - 1) points
                |> Maybe.withDefault 0
                |> (\x -> x - 133 + (w / 2) - o)
    in
    Canvas.toHtml (floor w, h)
        []
        [ Canvas.shapes
            [ Canvas.Settings.fill Color.white ]
            [ Canvas.rect ( 0, 0 ) w (toFloat h) ]
        , thiccLine y (w / 2 - o) points 0 71 (Color.rgb255 223 188 152)
        , thiccLine y (w / 2 - o) points 0 12 (Color.rgb255 194 141 102)
        , thiccLine y (w / 2 - o) points 42 71 (Color.rgb255 194 141 102)
        , thiccLine y (w / 2 - o) points 58 71 (Color.rgb255 181 122 94)
        , thiccLine y (w / 2 - o) points 0 1 Color.black
        , thiccLine y (w / 2 - o) points 70 71 Color.black
        , viewSpriteAt ( x0, y ) spriteHead
        , viewSpriteAt ( xl, toFloat <| y + 254 + Array.length points * 2 ) spriteTail
        ]


white = Css.rgb 255 255 255
black = Css.rgb 0 0 0
blue = Css.rgb 66 188 245
slateGrey = Css.rgb 47 49 54


darkSection : Css.Style
darkSection =
    Css.batch
        [ Css.backgroundColor slateGrey
        , Css.color white
        , Css.displayFlex
        , Css.flexWrap Css.wrap
        , Css.flexDirection Css.row
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.padding (Css.px 30)
        ]


styleLoadingScreen : Css.Style
styleLoadingScreen =
    Css.batch 
        [ Css.overflow Css.hidden
        , Css.width (Css.vw 100)
        , Css.height (Css.vh 100)
        , Css.backgroundColor slateGrey
        , Css.color white
        , Css.displayFlex
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        ]


viewLoadingScreen : Html Msg
viewLoadingScreen =
    styled div
        [ styleLoadingScreen ]
        []
        [ p
            []
            [ text "Loading..." ]
        , Canvas.toHtmlWith
            { width = 0
            , height = 0
            , textures =
                [ Canvas.Texture.loadFromImageUrl "./public/kita-head.png" TextureLoadedHead
                , Canvas.Texture.loadFromImageUrl "./public/kita-tail.png" TextureLoadedTail
                ]
            }
            []
            []
          |> Html.Styled.fromUnstyled
        ]


viewLoadFailure : Html Msg
viewLoadFailure =
    styled div
        [ styleLoadingScreen ]
        []
        [ p
            []
            [ text "Loading resources failed. Try refreshing the page." ]
        ]


viewHeader : Html Msg
viewHeader =
    let
        links =
            [ ("Made by Paf", "https://twitter.com/ProbsAFox")
            , ("Character is Kita", "https://twitter.com/kitacatter")
            , ("Art by Cianiati", "https://twitter.com/ScribbleServal")
            , ("Source", "https://github.com/DXsmiley/longboi")
            ]
        linksHtml =
            links
            |> List.map
                (\(text, url) ->
                    styled Html.Styled.a
                        [ Css.display Css.inlineBlock
                        , Css.margin2 (Css.px 0) (Css.px 8)
                        , Css.lineHeight (Css.px 30)
                        ]
                        [ href url
                        , target "_blank"
                        ]
                        [ Html.Styled.text text ]
                )
            |> div []
    in
    styled div
        [ darkSection
        , Css.paddingBottom Css.zero
        ]
        []
        [ linksHtml ]


viewEquationComponent : Maybe String -> String -> Html Msg
viewEquationComponent expressionError input =
    let
        contents =
            styled Html.Styled.input
                [ Css.borderRadius (Css.px 20)
                , Css.padding2 (Css.px 5) (Css.px 15)
                , Css.borderStyle Css.solid
                , Css.borderWidth (Css.px 4)
                , Css.fontFamily Css.monospace
                , Css.marginRight (Css.px 0)
                , Css.marginBottom (Css.px 0)
                , Css.maxWidth (Css.px 500)
                , Css.width (Css.pc 100)
                , Css.minWidth (Css.px 0)
                , Css.display Css.inlineBlock
                , Css.flexShrink (Css.num 1)
                , Css.flexGrow (Css.num 1)
                , Css.letterSpacing (Css.px -1)
                -- TODo: More informative errors
                , case expressionError of
                    Nothing -> Css.borderColor white
                    Just error -> Css.borderColor (Css.rgb 245 66 120)
                , Css.focus [ Css.outline Css.none ]
                ]
                [ Html.Styled.Attributes.value input
                , onInput InputChanged
                ]
                []
    in
    styled div
        [ darkSection
        , Css.position Css.sticky
        , Css.top (Css.px 0)
        ]
        []
        [ contents ]


viewToggles : Bool -> Bool -> Html Msg
viewToggles smoothEnds extraLong =
    let
        toggle text_ isOn command =
            styled div
                [ Css.flexShrink (Css.num 0)
                , Css.flexGrow (Css.num 0)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.flexWrap Css.noWrap
                , Css.marginRight (Css.px 10)
                , Css.marginLeft (Css.px 10)
                --, Css.marginBottom (Css.px 20)
                ]
                []
                [ styled div
                    [ if isOn then
                        Css.backgroundColor blue
                      else
                        Css.backgroundColor white
                    , Css.flexShrink (Css.num 0)
                    , Css.flexGrow (Css.num 0)
                    , Css.width (Css.px 30)
                    , Css.height (Css.px 30)
                    , Css.borderRadius (Css.px 15)
                    , Css.borderWidth (Css.px 7)
                    , Css.borderStyle Css.solid
                    , Css.borderColor white
                    , Css.display Css.inlineBlock
                    ]
                    [ onClick (command (not isOn)) ]
                    []
                , styled Html.Styled.span
                    [ Css.marginLeft (Css.px 5)
                    ]
                    []
                    [ text text_ ]
                ]
    in
    styled div
        [ darkSection
        , Css.paddingBottom (Css.px 0)
        , Css.flexDirection Css.row
        ]
        []
        [ toggle "Smooth Ends" smoothEnds SetSmoothing
        , toggle "Extra Long" extraLong SetExtraLong
        ]


viewHelpFooter : Html msg
viewHelpFooter =
    let
        code t =
            Html.Styled.code [] [ text t ]
    in
    styled div
        [ Css.marginTop (Css.px 30)
        , darkSection
        ]
        []
        [ styled div
            [ Css.maxWidth (Css.px 300) ]
            []
            [ p []
                [ text "You can use the variable "
                , code "x"
                , text " and the four operators:"
                ]
            , ul
                []
                [ li [] [ code "+", text " : addition" ]
                , li [] [ code "-", text " : subtraction" ]
                , li [] [ code "*", text " : multiplication" ]
                , li [] [ code "^", text " : exponentiation" ]
                ]
            , p [] [ text "Division is non-continuous and therefore you don't get to use it." ]
            , p [] [ text "The following functions exist" ]
            , ul
                []
                [ li [] [ code "sin" ]
                , li [] [ code "cos" ]
                , li [] [ code "exp" ]
                , li [] [ code "abs" ]
                ]
            , p []
                [ text "The constants "
                , code "pi"
                , text " and "
                , code "e"
                , text " are also available."
                ]
            , p []
                [ text "Values of "
                , code "x"
                , text " range from "
                , code "0"
                , text " to "
                , code "10"
                , text ", or "
                , code "0"
                , text " to "
                , code "50"
                , text " in extra long mode."
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        loadState =
            loadMap2 (\a b -> (a, b))
                model.spriteHead
                model.spriteTail
    in
    let
        body =
            case loadState of
                Failure -> viewLoadFailure
                Loading -> viewLoadingScreen
                Success (spriteHead, spriteTail) ->
                    div
                        []
                        [ viewHeader
                        , Html.Styled.Lazy.lazy2 viewToggles
                            model.smoothEnds
                            model.extraLong
                        , Html.Styled.Lazy.lazy2 viewEquationComponent
                            model.expressionError
                            model.input
                        , styled div
                            [ Css.displayFlex
                            , Css.justifyContent Css.center
                            ]
                            []
                            [ Html.Lazy.lazy4
                                viewCanvas
                                model.animatedValues
                                spriteHead
                                spriteTail
                                (toFloat model.windowWidth)
                              |> Html.Styled.fromUnstyled
                            ]
                        , viewHelpFooter
                        ]
    in
    { title = "Longboi"
    , body =
        [ Css.Global.global
            [ Css.Global.body
                [ Css.margin (Css.px 0)
                , Css.backgroundColor white
                --, Css.overflowX Css.visible
                ]
            , Css.Global.everything
                [ Css.boxSizing Css.borderBox
                , Css.fontFamilies [ "Roboto" ]
                , Css.fontSize (Css.em 1.1)
                ]
            , Css.Global.code
                [ Css.fontFamily Css.monospace
                ]
            , Css.Global.a
                [ Css.link [ Css.color white ]
                , Css.visited [ Css.color white ]
                , Css.active [ Css.color white ]
                , Css.hover [ Css.color blue ]
                ]
            ]
            |> toUnstyled
        , body |> toUnstyled
        ]
    }

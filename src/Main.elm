module Main exposing (..)

-- elm init
-- elm install elm/url
-- elm install rtfeldman/elm-css

import Browser
import Browser.Navigation as Nav
import Browser.Events
import Css
import Css.Global
import Html.Styled exposing (Html, button, div, p, text, toUnstyled, styled, ul, li)
import Html.Styled.Attributes exposing (href, target)
import Html.Styled.Events exposing (onClick, onInput)
import Url
import Parser exposing (Parser, (|=), (|.))
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Texture exposing (Texture)
import Color exposing (Color)
import Time
import Maybe

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
    Browser.Events.onAnimationFrame AnimationFrame


type FunctionName
    = FuncSin
    | FuncCos
    | FuncExp
    | FuncAbs


type Expression
    = VarX
    | ConstE
    | ConstPi
    | ConstValue Float
    | FunctionCall FunctionName Expression
    | Product Expression Expression
    | Sum Expression Expression
    | Difference Expression Expression
    | Exponentiation Expression Expression


evalFunction name arg =
    case name of
        FuncSin -> sin arg
        FuncCos -> cos arg
        FuncExp -> e ^ arg
        FuncAbs -> abs arg


evalExpression : Expression -> Float -> Float
evalExpression expression x =
    case expression of
        VarX -> x
        ConstE -> e
        ConstPi -> pi
        ConstValue v -> v
        FunctionCall name argument -> evalFunction name (evalExpression argument x)
        Product l r -> (evalExpression l x) * (evalExpression r x)
        Sum l r -> (evalExpression l x) + (evalExpression r x)
        Difference l r -> (evalExpression l x) - (evalExpression r x)
        Exponentiation l r -> (evalExpression l x) ^ (evalExpression r x)


type AdditionSymbols
    = Plus
    | Minus


parseSequence : Parser a -> Parser b -> (b -> (a, b) -> b) -> Parser b
parseSequence sep component combiner =
    let
        helper revParts =
            Parser.oneOf
                [ Parser.succeed (\sep1 component1 -> Parser.Loop ((sep1, component1) :: revParts))
                    |. Parser.spaces
                    |= sep
                    |. Parser.spaces
                    |= component
                , Parser.lazy (\() -> Parser.succeed (Parser.Done (List.reverse revParts)))
                ]
    in
    Parser.succeed (List.foldl (\a b -> combiner b a))
        |. Parser.spaces
        |= component
        |. Parser.spaces
        |= Parser.loop [] helper
        |. Parser.spaces


parseFunction : FunctionName -> String -> Parser Expression
parseFunction name keyword =
    Parser.succeed (FunctionCall name)
        |. Parser.keyword keyword
        |. Parser.spaces
        |= Parser.lazy (\() -> parseAtom)
        |. Parser.spaces


parseAtom : Parser Expression
parseAtom =
    Parser.succeed identity
    |. Parser.spaces
    |= Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.lazy (\() -> parseAddition)
            |. Parser.symbol ")"
        , parseFunction FuncSin "sin"
        , parseFunction FuncCos "cos"
        , parseFunction FuncExp "exp"
        , parseFunction FuncAbs "abs"
        , Parser.succeed VarX
            |. Parser.keyword "x"
        , Parser.succeed ConstValue
            |= Parser.float
        , Parser.succeed ConstPi
            |.  Parser.keyword "pi"
        ]
    |. Parser.spaces


parseExponentiation =
    parseSequence
        (Parser.symbol "^")
        parseAtom
        (\l (_, r) -> Exponentiation l r)

parseMultiplication =
    parseSequence
        (Parser.symbol "*")
        parseExponentiation
        (\l (_, r) -> Product l r)

parseAddition =
    parseSequence
        (Parser.oneOf
            [ Parser.symbol "+" |> Parser.map (\_ -> Plus)
            , Parser.symbol "-" |> Parser.map (\_ -> Minus)
            ]
        )
        parseMultiplication
        (\l (op, r) ->
            case op of
                Plus -> Sum l r
                Minus -> Difference l r
        )

parser : Parser Expression
parser =
    parseAddition
        |. Parser.spaces
        |. Parser.end

type Load a
    = Loading
    | Failure
    | Success a

type alias Model =
    { input : String
    , parsedExpression : Maybe Expression
    , expressionError : Maybe String
    , computedValues : List Float
    , animatedValues : List Float
    , lastFrameTime : Int
    , spriteHead : Load Texture
    , spriteTail : Load Texture
    , smoothEnds : Bool
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialExpression =
            Product (FunctionCall FuncSin VarX) (ConstValue 20.0)
    in
    let
        initialPoints =
            calculatePoints initialExpression True
    in
    let
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
            }
    in
    ( state, Cmd.none )


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | InputChanged String
    | AnimationFrame Time.Posix
    | TextureLoadedHead (Maybe Texture)
    | TextureLoadedTail (Maybe Texture)
    | SetSmoothing Bool


--smoothLerp x0 fx0 x1 x =
--    if x <= x0 then fx0 else
--    if x >= x1 then fx1 else
--    fx0 * (x1 - x) / (x1 - x0) + fx1 * (x - x0) / (x1 - x0)


fWeightedAverage : (Float -> Float) -> (Float -> Float) -> (Float, Float) -> Float -> Float
fWeightedAverage fa fb (xl, xr) x =
    if x <= xl then fa x else
    if x >= xr then fb x else
    let k = ((cos ((xr - x) / (xr - xl) * pi)) + 1) / 2 in
    (fa x) * (1 - k) + (fb x) * k


calculatePoints : Expression -> Bool -> List Float
calculatePoints expression smooth =
    let
        smoothRange = 1.5
        maxValue = 10
        numSamples = 200

        rawValue : Float -> Float
        rawValue x =
            evalExpression expression x

        x0 = rawValue 0
        xl = rawValue (toFloat maxValue)

        smoothValue : Float -> Float
        smoothValue x =
            if smooth then
                fWeightedAverage
                    (fWeightedAverage (\_ -> x0) rawValue (0, smoothRange))
                    (\_ -> xl)
                    (toFloat maxValue - smoothRange, toFloat maxValue)
                    x
            else
                rawValue x
    in
    List.range 0 numSamples
    |> List.map
        (\s ->
            smoothValue ((toFloat s) / (toFloat numSamples) * (toFloat maxValue))
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

        InputChanged newInput ->
            let
                newModel = 
                    case Parser.run parser newInput of
                        Err error ->
                            { model
                            | input = newInput
                            , expressionError = Just (Parser.deadEndsToString error)
                            }
                        Ok expression ->
                            let
                                points = calculatePoints expression
                            in
                            { model
                            | input = newInput
                            , parsedExpression = Just expression
                            , expressionError = Nothing
                            , computedValues = calculatePoints expression model.smoothEnds
                            }
            in
            ( newModel, Cmd.none )

        AnimationFrame time ->
            let
                newPoints =
                    List.map2
                        (\cur target ->
                            ( if abs (cur - target) < 0.2
                              then target
                              else (cur * 9 + target) / 10
                            )
                            |> clamp -1000 1000
                        )
                        model.animatedValues
                        model.computedValues
            in
            ( { model
              | lastFrameTime = time |> Time.posixToMillis
              , animatedValues = newPoints
              }
            , Cmd.none
            )

        SetSmoothing smooth ->
            ( { model
              | smoothEnds = smooth
              , computedValues =
                model.parsedExpression
                |> Maybe.map (\e -> calculatePoints e smooth)
                |> Maybe.withDefault model.computedValues
              }
            , Cmd.none
            )


--path1 : List (Float, Float) -> 
path1 points =
    case points of
        [] -> Canvas.path (0, 0) []
        x :: xr ->
            Canvas.path
                x
                (List.map Canvas.lineTo xr)


thiccLine y l points o1 o2 color =
    let
        pointsOnCanvas =
            (++)
                (List.indexedMap
                    (\i v -> (l + v + o1, y + 256 + toFloat i * 2))
                    points
                )
                (List.reverse
                    (List.indexedMap
                        (\i v -> (l + v + o2, y + 256 + toFloat i * 2))
                        points
                    )
                )
    in
    Canvas.shapes
        [ Canvas.Settings.fill color ]
        [ path1 pointsOnCanvas ]

-- dfbc98 -- 223, 188, 152
-- c28d66 -- 194, 141, 102
-- b57a5e -- 181, 122, 94

viewSpriteAt (x, y) sprite =
    case sprite of
        Loading -> Canvas.shapes [] []
        Failure -> Canvas.shapes [] []
        Success sprite1 ->
            Canvas.texture
                [ Canvas.Settings.Advanced.transform
                    [ Canvas.Settings.Advanced.translate x y
                    , Canvas.Settings.Advanced.scale 0.5 0.5
                    , Canvas.Settings.Advanced.translate -x -y
                    ]
                ]
                (x, y)
                sprite1

viewCanvas points spriteHead spriteTail =
    let
        o = 0
        y = 15
        w = 600
        h = 950
        x0 =
            List.head points
            |> Maybe.withDefault 0
            |> (\x -> x - 133 + (w / 2) - o)
        xl =
            List.head (List.reverse points)
            |> Maybe.withDefault 0
            |> (\x -> x - 133 + (w / 2) - o)
    in
    Canvas.toHtmlWith
        { width = w
        , height = h
        , textures =
            [ Canvas.Texture.loadFromImageUrl "./public/kita-head.png" TextureLoadedHead
            , Canvas.Texture.loadFromImageUrl "./public/kita-tail.png" TextureLoadedTail
            ]
        }
        []
        [ Canvas.shapes
            [ Canvas.Settings.fill Color.white ]
            [ Canvas.rect ( 0, 0 ) w h ]
        , thiccLine y (w / 2 - o) points 0 71  (Color.rgb255 223 188 152)
        , thiccLine y (w / 2 - o) points 0 12  (Color.rgb255 194 141 102)
        , thiccLine y (w / 2 - o) points 42 71 (Color.rgb255 194 141 102)
        , thiccLine y (w / 2 - o) points 58 71 (Color.rgb255 181 122 94)
        , thiccLine y (w / 2 - o) points 0 1    Color.black
        , thiccLine y (w / 2 - o) points 70 71  Color.black
        , viewSpriteAt (x0, y) spriteHead
        , viewSpriteAt (xl, toFloat <| y + 254 + (List.length points) * 2) spriteTail
        ]


view : Model -> Browser.Document Msg
view model =
    let
        errorMessages = 
            [ case model.spriteHead of
                Loading -> Just "Loading head sprite"
                Failure -> Just "Failed to load head sprite"
                Success _ -> Nothing
            , case model.spriteTail of
                Loading -> Just "Loading tail sprite"
                Failure -> Just "Failed to load head sprite"
                Success _ -> Nothing
            , case model.expressionError of
                Nothing -> Nothing
                Just error -> Just "Bad expression :("
            ]
            |> List.filterMap identity
    in
    let
        body =
            styled div
                [ Css.margin2 (Css.zero) (Css.auto)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.flexWrap Css.noWrap
                ]
                []
                [ p
                    []
                    [ Html.Styled.a
                        [ href "https://twitter.com/ProbsAFox"
                        , target "_blank"
                        ]
                        [ text "Code by Paf" ]
                    , text " | "
                    , Html.Styled.a
                        [ href "https://twitter.com/kitacatter"
                        , target "_blank"
                        ]
                        [ text "Character is Kita" ]
                    , text " | "
                    , Html.Styled.a
                        [ href "https://twitter.com/ScribbleServal"
                        , target "_blank"
                        ]
                        [ text "Art by Cianiati" ]
                    ]
                , styled div
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    ]
                    []
                    [ styled Html.Styled.input
                        [ Css.borderRadius (Css.px 20)
                        , Css.padding2 (Css.px 5) (Css.px 15)
                        , Css.borderColor (Css.rgb 0 0 0)
                        , Css.borderStyle Css.solid
                        , Css.borderWidth (Css.px 2)
                        , Css.fontFamily Css.monospace
                        , Css.margin2 Css.zero (Css.px 10)
                        ]
                        [ Html.Styled.Attributes.value model.input
                        , onInput InputChanged
                        ]
                        []
                    , styled div
                        [ if model.smoothEnds
                          then Css.backgroundColor (Css.rgb 0 200 40)
                          else Css.backgroundColor (Css.rgba 0 0 0 0)
                        , Css.width (Css.px 30)
                        , Css.height (Css.px 30)
                        , Css.borderRadius (Css.px 10)
                        , Css.borderWidth (Css.px 2)
                        , Css.borderStyle Css.solid
                        , Css.borderColor (Css.rgb 0 0 0)
                        , Css.display Css.inlineBlock
                        ]
                        [ onClick (SetSmoothing (not model.smoothEnds)) ]
                        []
                    , styled Html.Styled.span
                        [ Css.marginLeft (Css.px 5) ]
                        []
                        [ text "Smooth ends" ]
                    ]
                , styled p
                    [ Css.height (Css.px 20)
                    , Css.margin2 (Css.px 5) Css.zero
                    ]
                    []
                    [ List.head errorMessages |> Maybe.withDefault "" |> text ]
                , styled div
                    [ Css.boxShadow5
                        (Css.px 2)
                        (Css.px 8)
                        (Css.px 20)
                        (Css.px 0)
                        (Css.rgb 200 200 200)
                    ]
                    []
                    [ viewCanvas model.animatedValues model.spriteHead model.spriteTail
                      |> Html.Styled.fromUnstyled
                    ]
                , let code t = Html.Styled.code [] [ text t ]in
                  styled div
                    [ Css.maxWidth (Css.px 200)
                    , Css.marginTop (Css.px 30)
                    ]
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
                        , li [] [ code "*", text " : multiplcation" ]
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
                    ]
                ]
    in
    { title = "Longboi"
    , body =
        [ Css.Global.global
            [ Css.Global.body
                [ Css.margin (Css.px 20)
                , Css.backgroundColor (Css.rgb 255 255 255)
                ]
            , Css.Global.everything
                [ Css.boxSizing Css.borderBox
                ]
            ]
            |> toUnstyled
        , body |> toUnstyled
        ]
    }

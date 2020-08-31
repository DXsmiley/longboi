module Expression exposing (Expression(..), FunctionName(..), evaluate, parse)

import Parser exposing ((|.), (|=), Parser)


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


type FoldDirection
    = Left
    | Right


evaluate : Expression -> Float -> Float
evaluate expression x =
    case expression of
        VarX ->
            x

        ConstE ->
            e

        ConstPi ->
            pi

        ConstValue v ->
            v

        FunctionCall name argument ->
            evaluateFunction name (evaluate argument x)

        Product l r ->
            evaluate l x * evaluate r x

        Sum l r ->
            evaluate l x + evaluate r x

        Difference l r ->
            evaluate l x - evaluate r x

        Exponentiation l r ->
            evaluate l x ^ evaluate r x


evaluateFunction name arg =
    case name of
        FuncSin ->
            sin arg

        FuncCos ->
            cos arg

        FuncExp ->
            e ^ arg

        FuncAbs ->
            abs arg


parseSequence : Parser a -> Parser b -> Parser ( b, List ( a, b ) )
parseSequence sep component =
    let
        helper revParts =
            Parser.oneOf
                [ Parser.succeed
                    (\sep1 component1 ->
                        Parser.Loop (( sep1, component1 ) :: revParts)
                    )
                    |. Parser.spaces
                    |= sep
                    |. Parser.spaces
                    |= component
                , Parser.lazy (\() -> Parser.succeed (Parser.Done (List.reverse revParts)))
                ]
    in
    Parser.succeed (\a b -> ( a, b ))
        |. Parser.spaces
        |= component
        |. Parser.spaces
        |= Parser.loop [] helper
        |. Parser.spaces


mergeFromLeft : (b -> a -> b -> b) -> b -> List ( a, b ) -> b
mergeFromLeft f i xs =
    case xs of
        [] ->
            i

        ( a, b ) :: xr ->
            mergeFromLeft f (f i a b) xr


mergeFromRight : (b -> a -> b -> b) -> b -> List ( a, b ) -> b
mergeFromRight f i xs =
    case xs of
        [] ->
            i

        ( a, b ) :: xr ->
            f i a (mergeFromRight f b xr)


parseOperatorSequence : FoldDirection -> List ( a -> a -> a, String ) -> Parser a -> Parser a
parseOperatorSequence foldDirection seps component =
    let
        sepsParser =
            seps
                |> List.map
                    (\( value, string ) -> Parser.succeed value |. Parser.symbol string)
                |> Parser.oneOf

        mergeFunction =
            case foldDirection of
                Left ->
                    mergeFromLeft

                Right ->
                    mergeFromRight
    in
    parseSequence
        sepsParser
        component
        |> Parser.map
            (\( i, xs ) -> mergeFunction (\l op r -> op l r) i xs)


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
                |. Parser.keyword "pi"
            ]
        |. Parser.spaces


parseExponentiation =
    parseOperatorSequence
        Right
        [ ( Exponentiation, "^" ) ]
        parseAtom


parseMultiplication =
    parseOperatorSequence
        Left
        [ ( Product, "*" ) ]
        parseExponentiation


parseAddition =
    parseOperatorSequence
        Left
        [ ( Sum, "+" )
        , ( Difference, "-" )
        ]
        parseMultiplication


parser : Parser Expression
parser =
    parseAddition
        |. Parser.spaces
        |. Parser.end


parse string =
    Parser.run parser string

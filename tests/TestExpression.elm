module TestExpression exposing (..)

import Expect exposing (Expectation)
import Expression
import Fuzz exposing (Fuzzer, int, list, string)
import Result
import Test exposing (..)


parseAndEvaluate string result =
    Expression.parse string
        |> Result.map (\e -> Expression.evaluate e 0)
        |> Expect.equal (Ok result)


testAddition =
    describe "Addition"
        [ test "Addition evaluated left to right" <|
            \_ -> parseAndEvaluate "2 + 3 - 4" 1
        , test "Operand order is respected" <|
            \_ -> parseAndEvaluate "3 - 2" 1
        ]


testExponentiation =
    describe "Exponentiation"
        [ test "Powers evaluated right to left" <|
            \_ -> parseAndEvaluate "3 ^ 3 ^ 3" 7625597484987
        , test "Brackets are respected (right first)" <|
            \_ -> parseAndEvaluate "3 ^ (3 ^ 3)" 7625597484987
        , test "Brackets are respected (left first)" <|
            \_ -> parseAndEvaluate "(3 ^ 3) ^ 3" 19683
        , test "Operand order is respected" <|
            \_ -> parseAndEvaluate "2 ^ 3" 8
        ]

module Bogus exposing (suite)

import Expect
import Test exposing (Test, describe, test)

add1 : Int -> Int
add1 x = 1 + x

suite : Test
suite =
    describe "basic test"
        [ test "1+1 = 2" <|
            \_ -> 1 + 1
                |> Expect.equal 2
        , test "pipin' 1 |> add1 |> equal 2" <|
            \_ -> 1
                |> add1
                |> Expect.equal 3
    ]

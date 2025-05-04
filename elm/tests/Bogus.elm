module Bogus exposing (suite)

import Expect
import Test exposing (Test, describe, test)

add1 : Int -> Int
add1 x = 1 + x
times2 : Int -> Int
times2 x = 2 * x

add1thentimes2 : Int -> Int
add1thentimes2 = add1 >> times2
times2thenadd1 : Int -> Int
times2thenadd1 = add1 << times2

foo: () -> Expect.Expectation
foo _ = 2 |> add1 |> Expect.equal 3
bar : Int -> () -> Expect.Expectation
bar x = \_ -> x |> add1 |> Expect.equal (x+1)

divide12By : Int -> Maybe Int
divide12By n = case n of
    0 -> Nothing
    p -> 12 // p |> Just

suite : Test
suite =
    describe "basic test"
        [ test "1+1 = 2" <|
            \_ -> 1 + 1
                |> Expect.equal 2
        , test "pipin' 1 |> add1 |> equal 2" <|
            \_ -> 1
                |> add1
                |> Expect.equal 2
        , test "no closure foo" foo
        , test "no closure bar" <| bar 5
        , test "composition order >>" <| \_ -> 3 |> add1thentimes2 |> Expect.equal 8
        , test "composition order <<" <| \_ -> 3 |> times2thenadd1 |> Expect.equal 7
        -- , test "composition order explicit <<" <| \_ -> 3 |> add1 << times2 |> Expect.equal 7
        -- , test "composition order explicit >>" <| \_ -> 3 |> add1 >> times2 |> Expect.equal 8
        , test "composition order explicit >> again" <| (\_ -> 3) >> add1 >> times2 >> Expect.equal 8
        , test "composition order pipin  |>"   <| \_ -> 3 |> add1 |> times2 |> Expect.equal 8
        , test "composition order reverse"   <| \_ -> Expect.equal 8 <| times2 <| add1 <| 3
        , test "monadic bind 1" <| \_ -> 3          |>                  divide12By |> Expect.equal (Just 4)
        , test "monadic bind 2" <| \_ -> 0          |>                  divide12By |> Expect.equal Nothing
        -- , test "monadic bind 3" <| \_ -> Nothing    |> Maybe.andThen    divide12By |> Expect.equal Nothing
        -- , test "monadic bind 4" <| \_ -> (Just 3)   |> Maybe.andThen    divide12By |> Expect.equal (Just 4)
        -- , test "monadic bind 5" <| \_ -> (Just 0)   |> Maybe.andThen    divide12By |> Expect.equal Nothing
    ]


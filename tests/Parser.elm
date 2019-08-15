module Parser exposing (endLoopOutsideLoop, loop, noop, simple, unclosedLoop)

import Brainfuck.Parser exposing (Statement(..), parse)
import Expect
import Test exposing (..)


simple : Test
simple =
    test "simple statements" <|
        \() ->
            Expect.equal
                (parse "<>+-.,")
                [ Left, Right, Incr, Decr, Output, Input ]


noop : Test
noop =
    test "parser should ignore unknown chars" <|
        \() ->
            Expect.equal
                (parse "qwertyasdf")
                []


loop : Test
loop =
    test "loop" <|
        \() ->
            Expect.equal
                (parse ",[.[-],]")
                [ Input, Loop [ Output, Loop [ Decr ], Input ] ]


unclosedLoop : Test
unclosedLoop =
    test "unclosed loop should close itself" <|
        \() ->
            Expect.equal
                (parse "[.")
                [ Loop [ Output ] ]


endLoopOutsideLoop : Test
endLoopOutsideLoop =
    test "] outside a loop should be a no-op" <|
        \() ->
            Expect.equal
                (parse "]")
                []

module Tests.Parser exposing (..)

import Test exposing (..)
import Expect
import Brainfuck.Parser exposing (parse, Statement(..))


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

%%%-------------------------------------------------------------------
%%% @author Samuel Otter <samuel@otter.se>
%%% @copyright (C) 2014, Samuel Otter
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ftree_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/ftree.hrl").

measure(_) -> 1.

lpush_test_() ->
    Measure = fun measure/1,
    Empty   = ftree:new(),
    Single  = #ft_single{element = 1},
    Many    = ftree:from_list(Measure, lists:seq(1, 100)),

    [ ?_assertEqual(ftree:first(ftree:lpush(Measure, x, Empty)), x)
    , ?_assertEqual(ftree:first(ftree:lpush(Measure, x, Single)), x)
    , ?_assertEqual(ftree:first(ftree:lpush(Measure, x, Many)), x)
    ].

lpop_test_() -> 
    Measure = fun measure/1,
    
    Empty  = ftree:new(),
    Single = ftree:rpush(Measure, 1, Empty),
    Deep2  = ftree:rpush(Measure, 2, Single),
    Deep3  = ftree:rpush(Measure, 3, Deep2),
    Deep4  = ftree:rpush(Measure, 4, Deep3),
    Deep5  = ftree:rpush(Measure, 5, Deep4),
    Deep6  = ftree:rpush(Measure, 6, Deep5),
    Deep7  = ftree:rpush(Measure, 7, Deep6),
    Deep8  = ftree:rpush(Measure, 8, Deep7),
    Deep9  = ftree:rpush(Measure, 9, Deep8),
    Deep10 = ftree:rpush(Measure, 10, Deep9),
    
    [ ?_assertEqual(Empty, ftree:lpop(Measure, Empty))
    , ?_assertEqual({1, Empty}, ftree:lpop(Measure, Single))
    , ?_assertEqual({1, Single}, ftree:lpop(Measure, Deep2))
    , ?_assertEqual({1, Deep2}, ftree:lpop(Measure, Deep3))
    ].

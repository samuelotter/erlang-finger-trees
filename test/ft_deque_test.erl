%%%-------------------------------------------------------------------
%%% @author Samuel Otter <samuel.otter@gmail.com>
%%% @copyright (C) 2014, Samuel Otter
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ft_deque_test).

-include_lib("eunit/include/eunit.hrl").

measure_test_() ->
    [ ?_assertEqual(1, ft_deque:measure(x))
    , ?_assertEqual(2, ft_deque:measure([x, y]))
    , ?_assertEqual(3, ft_deque:measure([x, y, z]))
    ].

length_test_() ->
    Deque0 = ft_deque:new(),
    Deque1 = ft_deque:rpush(1, Deque0),
    Deque2 = ft_deque:rpush(2, Deque1),
    Deque3 = ft_deque:rpush(3, Deque2),

    [ ?_assertEqual(0, ft_deque:length(Deque0))
    , ?_assertEqual(1, ft_deque:length(Deque1))
    , ?_assertEqual(2, ft_deque:length(Deque2))
    , ?_assertEqual(3, ft_deque:length(Deque3))
    ].
    

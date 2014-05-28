-module(ft_deque_proper).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../src/ft_types.hrl").

-type ft_deep(V) :: #ft_deep{value  :: V,
                             left   :: [any()],
                             middle :: ftree(V),
                             right  :: list()} |
                    #ft_deep{value  :: V,
                             left   :: list(),
                             middle :: ftree(V),
                             right  :: [any()]}.

-type ftree(V) :: #ft_single{} |
                  #ft_empty{} |
                  ft_deep(V).


-type ft_deque() :: {ft_deque, ftree(integer())}.

prop_popl_after_pushl_gives_same_value() ->
    ?FORALL({X, T}, {any(), ft_deque()},
            begin
                {Y, _} = ft_deque:popl(ft_deque:lpush(X, T)),
                X == Y
            end).

prop_length_equals_number_of_elements() ->
    ?FORALL({L}, {list()},
            begin
                T = ft_deque:from_list(L),
                erlang:length(L) == ft_deque:length(T)
            end).

passes_all_props_test_() ->
    [ ?_assertMatch(true,
                    proper:quickcheck(prop_popl_after_pushl_gives_same_value()))
    ].

%%% ============================================================================
%%% @author Samuel Otter <>
%%% @copyright (C) 2014, Samuel Otter
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by Samuel Otter <>
%%% ============================================================================
-module(ft_deque).

-behaviour(ftree).

-compile({no_auto_import, [length/1]}).

-export([ append/2
        , first/1
        , foldl/3
        , last/1
        , length/1
        , lpush/2
        , value_of/1
        , merge/1
        , new/0
        , nth/2
        , rpush/2
        , seq/2
        , zero/0
        ]).

-include("ftree.hrl").

-record(ft_deque, {root = ftree:new()}).

-type ft_deque() :: #ft_deque{}.

zero() ->
    0.

value_of(List) when is_list(List) ->
    erlang:length(List);
value_of(Elem) ->
    1.
merge(List) ->
    lists:sum(List).

%% @doc Returns the number of elements in Deque.
-spec length(ft_deque()) -> integer().
length(#ft_deque{root = Root}) ->
    ftree:value(?MODULE, Root).

%% @doc Creates a new empty deque.
new() -> #ft_deque{}.

%% @doc Prepends an element to Deque.
lpush(Elem, Deque = #ft_deque{root = Root}) ->
    Deque#ft_deque{root = ftree:lpush(?MODULE, Elem, Root)}.

%% @doc Appends a element to Deque.
rpush(Elem, Deque = #ft_deque{root = Root}) ->
    Deque#ft_deque{root = ftree:rpush(?MODULE, Elem, Root)}.

%% @doc Calls Fun(X, Acc) on each element in Deque, starting with the
%%      leftmost one. The result of each call is passed on in the Acc
%%      argument to Fun. Acc0 is passed to the first call to Fun. The
%%      result of the final call to Fun/2 is returned.
foldl(Fun, Acc0, #ft_deque{root = Root}) ->
    ftree:foldl(Fun, Acc0, Root).

%% @doc Returns the first element in Deque.
first(#ft_deque{root = Root}) ->
    ftree:first(Root).

%% @doc Returns the last element in Deque.
last(#ft_deque{root = Root}) ->
    ftree:last(Root).

%% @doc Appends one tree to another.
append(#ft_deque{root = Root1},
       #ft_deque{root = Root2}) ->
    ftree:append(?MODULE, Root1, [], Root2).

%% @doc Generates a sequence of integers and adds them to a new tree.
seq(From, To) -> #ft_deque{root = ftree:seq(?MODULE, From, To)}.

%% @doc Returns the nth element from the beginning of Deque.
%%
%% Uses 1-based indexing for consistency with lists.
nth(1, #ft_single{element = Element}) ->
    Element;
nth(N, #ft_deep{left = Left, middle = Middle, right = Right}) ->
    LenLeft = ftree:value(?MODULE, Left),
    LenMiddle = ftree:value(?MODULE, Middle),
    if
        N =< LenLeft -> nth(N, Left);
        N =< LenMiddle -> nth(N - LenLeft, Middle);
        true -> nth(N - LenLeft - LenMiddle, Right)
    end;
nth(N, #ft_node2{a = A, b = B}) ->
    ALen = ftree:value(?MODULE, A),
    if
        N =< ALen -> nth(N, A);
        true -> nth(N - ALen, B)
    end;
nth(N, #ft_node3{a = A, b = B, c = C}) ->
    ALen = ftree:value(?MODULE, A),
    BLen = ftree:value(?MODULE, B),
    if
        N =< ALen -> nth(N, A);
        N - ALen =< BLen -> nth(N - ALen, B);
        true -> nth(N - ALen - BLen, C)
    end;
nth(N, [Elem | Tail]) ->
    Len = ftree:value(?MODULE, Elem),
    if
        N =< Len -> nth(N, Elem);
        N > Len -> nth(N - Len, Tail)
    end;
nth(N, #ft_deque{root = Root}) -> nth(N, Root);
nth(_N, Elem) -> Elem.

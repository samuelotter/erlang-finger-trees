-module(deque).
-compile({no_auto_import,[length/1]}).

-export([new/0, length/1, lpush/2, rpush/2, foldl/3, first/1, last/1, append/2, seq/2]).

-include("ftree.hrl").

-record(deque, {root = ftree:new()}).

%% @doc Returns the number of elements.
length(#deque{root = Root}) -> length(Root);
length(#ft_empty{}) -> 0;
length(#ft_single{data = Elem}) -> length(Elem);
length(#ft_deep{value=Value}) -> Value;
length(#ft_node2{value = Value}) -> Value; 
length(#ft_node3{value = Value}) -> Value; 
length([]) -> 0;
length([Head | Tail]) -> length(Head) + length(Tail);
length(_Elem) -> 1.

%% @doc Creates a new empty deque.
new() -> #deque{}.

%% @doc Prepends an element to the tree.
lpush(Elem, Tree = #deque{root = Root}) -> 
	Tree#deque{root = ftree:lpush(fun length/1, Elem, Root)}.

%% @doc Appends a element to the tree.
rpush(Elem, Tree = #deque{root = Root}) -> 
	Tree#deque{root = ftree:rpush(fun length/1, Elem, Root)}.

%% @doc 
foldl(Fun, Acc, #deque{root = Root}) -> ftree:foldl(Fun, Acc, Root).
	
first(#deque{root = Root}) -> ftree:first(Root).

last(#deque{root = Root}) -> ftree:last(Root).

%% @doc Appends one tree to another.
%% @todo The function should check that the same measuring function is used by both trees.
append(#deque{root = Root1}, 
	   #deque{root = Root2}) -> ftree:append(fun length/1, Root1, [], Root2).

%% @doc Generates a sequence of integers and adds them to a new tree.
seq(From, To) -> #deque{root = ftree:seq(fun length/1, From, To)}.


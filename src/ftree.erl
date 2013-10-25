%% ftree.erl
%% @author Samuel Otter
%% @copyright 2013, Samuel Otter
%%
%% @doc An implementation of 2-3 Finger Trees. 
%%

-module(ftree).
-export([new/0, lpush/3, rpush/3, foldl/3, to_list/1, from_list/2, first/1, last/1, append/3, seq/3]).
-compile({no_auto_import,[length/1]}).

-include("ftree.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Creates a new empty finger-tree.
new() -> #ft_empty{}.

%% @doc Inserts a new element to the left (at the front).
lpush(_Measure, Elem, #ft_empty{}) -> 
	#ft_single{element = Elem};
lpush(Measure, Elem, #ft_single{element = Existing}) -> 
	#ft_deep{value = Measure([Elem, Existing]),
			 left = [ Elem ], 
			 middle = #ft_empty{}, 
			 right = [ Existing ] };
lpush(Measure, Elem, Tree = #ft_deep{left = [L0, L1, L2, L3], 
					   		middle = Middle,
					   		right = Right}) -> 
	NewLeft = [Elem, L0],
	Node = #ft_node3{value = Measure([L1, L2, L3]), a = L1, b = L2, c = L3},
	NewMiddle = lpush(Measure, Node, Middle),
	Tree#ft_deep{value = Measure([NewLeft, NewMiddle, Right]),
				 left = NewLeft, 
	  		     middle = NewMiddle};
lpush(Measure, Elem, Deep = #ft_deep{left = Left, middle = Middle, right = Right}) -> 
	Deep#ft_deep{value = Measure([Elem, Left, Middle, Right]), left = [Elem | Left]}.


%% @doc Inserts a new element to the right (at the back). 
rpush(_Measure, Elem, #ft_empty{}) -> 
	#ft_single{element = Elem};
rpush(Measure, NewElem, #ft_single{element=Elem}) ->
	#ft_deep{value = Measure([Elem, NewElem]),
			 left = [Elem],
			 right = [NewElem]};
rpush(Measure, Elem, Tree = #ft_deep{left=Left,
						 right=[R0, R1, R2, R3],
						 middle = Middle}) -> 
	NewMiddle = rpush(Measure, #ft_node3{value = Measure([R1, R2, R3]), a = R1, b = R2, c = R3}, Middle),
	NewRight = [Elem | R0],
	Tree#ft_deep{value = Measure([Left, NewMiddle, NewRight]),
	             middle = NewMiddle,
	             right = NewRight};
rpush(Measure, Elem, Tree = #ft_deep{left = Left, middle = Middle, right = Right}) -> 
	Tree#ft_deep{value = Measure([Left, Middle, Right, Elem]), right = [Elem | Right]}.


%% @doc Pushes a list of elements at the front.
lpushlist(Measure, List, Tree) -> lists:foldr(fun (X, T) -> lpush(Measure, X, T) end, Tree, List).

%% @doc Pushes a list of elements at the back.
rpushlist(Measure, List, Tree) -> lists:foldl(fun (X, T) -> rpush(Measure, X, T) end, Tree, List).


%% @doc Calls `Fun(Elem, Acc)' on each element in the Tree, starting with the first one. The result from each call is
%%  	passed to the next in the second argument. The result of the last call to Fun is returned from the function.
foldl(Fun, Acc, #ft_node2{a = A, b = B}) -> 
	foldl(Fun, foldl(Fun, Acc, A), B); 
foldl(Fun, Acc, #ft_node3{a = A, b = B, c = C}) -> 
	foldl(Fun, foldl(Fun, foldl(Fun, Acc, A), B), C); 
foldl(_Fun, Acc, #ft_empty{}) -> 
	Acc;
foldl(Fun, Acc, #ft_single{element = Elem}) -> 
	Fun(Elem, Acc);
foldl(Fun, Acc0, #ft_deep{left = Left, middle = Middle, right = Right}) -> 
	Acc1 = lists:foldl(fun (Elem, Result) -> foldl(Fun, Result, Elem) end, Acc0, Left),
	Acc2 = foldl(Fun, Acc1, Middle),
	lists:foldr(fun (Elem, Result) -> foldl(Fun, Result, Elem) end, Acc2, Right);
foldl(Fun, Acc, X) -> 
	Fun(X, Acc).

%% @doc Calls `Fun(Elem, Acc)' on each element in the Tree, starting with the last one. The result from each call is
%%  	passed to the next in the second argument. The result of the last call to Fun is returned from the function.
foldr(Fun, Acc0, #ft_node2{a = A, b = B}) -> 
	foldr(Fun, foldr(Fun, Acc0, B), A);
foldr(Fun, Acc0, #ft_node3{a = A, b = B, c = C}) ->
	foldr(Fun, foldr(Fun, foldr(Fun, Acc0, C), B), A);
foldr(Fun, Acc0, #ft_empty{}) ->
	Acc0;
foldr(Fun, Acc0, #ft_single{element = Elem}) ->
	Fun(Elem, Acc0);
foldr(Fun, Acc0, #ft_deep{left = Left, middle = Middle, right = Right}) ->
	Acc1 = lists:foldl(fun (Elem, Acc) -> foldr(Fun, Acc, Elem) end, Acc0, Right),
	Acc2 = foldr(Fun, Acc1, Middle),
	lists:foldr(fun (Elem, Acc) -> foldr(Fun, Acc, Elem) end, Acc2, Left);
foldr(Fun, Acc0, Elem) ->
	Fun(Elem, Acc0).

%% @doc Returns a list with all the elements.
to_list(Tree) -> lists:reverse(foldl(fun (V, Acc) -> [V | Acc] end, [], Tree)).

%% @doc Creates a new tree from a list.
from_list(Measure, List) -> 
	lists:foldr(fun (X, Tree) -> lpush(Measure, X, Tree) end, new(), List).

%% @doc Returns the first element.
first(#ft_single{element = First}) -> First;
first(#ft_deep{left = [First | _]}) -> First.

%% @doc Returns the last element.
last(#ft_single{element = Last}) -> Last;
last(#ft_deep{right = [Last | _]}) -> Last.

%% @doc Groups a list of elements into nodes.
to_nodes(Measure, [A, B]) -> #ft_node2{value = Measure([A | B]), a = A, b = B};
to_nodes(Measure, [A, B, C]) ->  #ft_node3{value = Measure([A, B, C]), a = A, b = B, c = C};
to_nodes(Measure, [A, B, C, D]) -> 
	[#ft_node2{value = Measure([A, B]), a = A, b = B}, 
	 #ft_node2{value = Measure([C, D]), a = C, b = D}];
to_nodes(Measure, [A, B, C | Tail]) -> 
	[#ft_node3{value = Measure([A, B, C]), a = A, b = B, c = C} | [to_nodes(Measure, Tail)]].

%% @doc Appends a tree to another.
append(Measure, #ft_empty{}, Elems, Tree) -> lpushlist(Measure, Elems, Tree);
append(Measure, Tree, Elems, #ft_empty{}) -> rpushlist(Measure, Elems, Tree);
append(Measure, #ft_single{element = Elem}, Elems, Tree) -> 
	lpush(Measure, Elem, lpushlist(Measure, Elems, Tree));
append(Measure, Tree, Elems, #ft_single{element = Elem}) -> 
	rpush(Measure, Elem, rpushlist(Measure, Elems, Tree));
append(Measure, #ft_deep{left = L1, middle = M1, right = R1}, 
	   Elems, 
	   #ft_deep{left = L2, middle = M2, right = R2}) ->
	NewMiddle = append(Measure, M1, to_nodes(Measure, R1 ++ Elems ++ L2), M2),
	#ft_deep{value = Measure([L1, NewMiddle, R2]),
			 left = L1,
			 middle = NewMiddle,
			 right = R2}.

append(Measure, T1, T2) -> append(Measure, T1, [], T2).

seq(Measure, Tree, From, From) -> lpush(Measure, From, Tree);
seq(Measure, Tree, From, To) -> seq(Measure, lpush(Measure, To, Tree), From, To - 1).

seq(Measure, From, To) -> seq(Measure, new(), From, To).

%%
%% == Unit tests =======================================================================================================
%% 

-ifdef(TEST).

lpush_test_() ->
	Empty = new(),
	Single = #ft_single{element = 1},
	Many = from_list(lists:seq(1, 100)),

	[?_assert(first(lpush(x, Empty)) =:= x),
	 ?_assert(first(lpush(x, Single)) =:= x),
	 ?_assert(first(lpush(x, Many)) =:= x)].

-endif.

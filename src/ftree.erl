%% ftree.erl
%% @author Samuel Otter
%% @copyright 2013, Samuel Otter
%%
%% @doc An implementation of 2-3 Finger Trees.
%%

-module(ftree).
-export([ append/3
	, append/4
	, first/1
	, foldl/3
	, foldr/3
	, from_list/2
	, value/2
	, last/1
	, lpop/2
	, lpush/3
	, new/0
	, rpush/3
	, seq/3
	, split/4
	, to_list/1
	]).
-compile({no_auto_import,[length/1]}).

-include("ftree.hrl").

%% @doc Creates a new empty finger-tree.
new() -> #ft_empty{}.

%% @doc Inserts a new element to the left (at the front).
lpush(_Measure, Elem, #ft_empty{}) ->
    #ft_single{element = Elem};
lpush(Measure, Elem, #ft_single{element = Existing}) ->
    #ft_deep{value  = Measure([Elem, Existing]),
	     left   = [Elem],
	     middle = #ft_empty{},
	     right  = [Existing]};
lpush(Measure, Elem, Tree = #ft_deep{left   = [L0, L1, L2, L3],
				     middle = Middle,
				     right  = Right}) ->
    NewLeft   = [Elem, L0],
    Node      = #ft_node3{value = Measure([L1, L2, L3]), a = L1, b = L2, c = L3},
    NewMiddle = lpush(Measure, Node, Middle),
    Tree#ft_deep{value  = Measure([NewLeft, NewMiddle, Right]),
		 left   = NewLeft,
		 middle = NewMiddle};
lpush(Measure, Elem, Deep = #ft_deep{left = Left, middle = Middle, right = Right}) ->
    Deep#ft_deep{value = Measure([Elem, Left, Middle, Right]), left = [Elem | Left]}.


%% @doc Inserts a new element to the right (at the back).
rpush(_Measure, Elem, #ft_empty{}) ->
    #ft_single{element = Elem};
rpush(Measure, NewElem, #ft_single{element=Elem}) ->
    #ft_deep{value = Measure([Elem, NewElem]),
	     left  = [Elem],
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

%% @doc Removes and returns the first element.
lpop(_Measure, #ft_empty{}) -> #ft_empty{};
lpop(_Measure, #ft_single{element = Elem}) -> {Elem, #ft_empty{}};
lpop(Measure, Node = #ft_deep{left = [Elem | LTail], middle = Middle, right = Right}) ->
    Tail = case LTail of
	       [] ->
		   case lpop(Measure, Middle) of
		       #ft_empty{} -> from_list(Measure, Right);
		       {A, Middle2} -> deep(Measure, [A], Middle2, Right)
		   end;
	       _ -> Node#ft_deep{left = LTail}
	   end,
    {Elem, Tail}.

%% @doc Removes and returns the last element.
rpop(_Measure, #ft_empty{}) -> #ft_empty{};
rpop(_Measure, #ft_single{element = Elem}) -> {Elem, #ft_empty{}};
rpop(Measure, Node = #ft_deep{left = Left, middle = Middle, right = [Elem | RTail]}) ->
    Tail = case RTail of
	       [] ->
		   case rpop(Measure, Middle) of
		       #ft_empty{} -> from_list(Measure, Left);
		       {A, Middle2} -> deep(Measure, Left, Middle2, [A])
		   end;
	       _ -> Node#ft_deep{left = RTail}
	   end,
    {Elem, Tail}.

%% @doc Pushes a list of elements at the front.
lpushlist(Measure, List, Tree) -> 
    lists:foldr(fun (X, T) -> lpush(Measure, X, T) end, Tree, List).

%% @doc Pushes a list of elements at the back.
rpushlist(Measure, List, Tree) -> 
    lists:foldl(fun (X, T) -> rpush(Measure, X, T) end, Tree, List).

%% @doc Calls `Fun(Elem, Acc)' on each element in the Tree, starting with the first one. The result from each call is
%%	passed to the next in the second argument. The result of the last call to Fun is returned from the function.
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
%%	passed to the next in the second argument. The result of the last call to Fun is returned from the function.
foldr(Fun, Acc0, #ft_node2{a = A, b = B}) ->
    foldr(Fun, foldr(Fun, Acc0, B), A);
foldr(Fun, Acc0, #ft_node3{a = A, b = B, c = C}) ->
    foldr(Fun, foldr(Fun, foldr(Fun, Acc0, C), B), A);
foldr(_Fun, Acc0, #ft_empty{}) ->
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
to_list(Tree) -> 
    lists:reverse(foldl(fun (V, Acc) -> [V | Acc] end, [], Tree)).

%% @doc Creates a new tree from a list.
from_list(Measure, List) ->
    lists:foldr(fun (X, Tree) -> lpush(Measure, X, Tree) end, new(), List).

%% @doc Creates a new deep node.
deep(Measure, Left, Middle, Right) ->
    #ft_deep{value = Measure([Left, Middle, Right]), left = Left, middle = Middle, right = Right}.
 
deepl(Measure, [], Middle, Right) ->
    case lpop(Measure, Middle) of
	#ft_empty{} -> from_list(Measure, Right);
	{A, NewMiddle} -> deep(Measure, [A], NewMiddle, Right)
    end;
deepl(Measure, Left, Middle, Right) ->
    deep(Measure, Left, Middle, Right).

deepr(Measure, Left, Middle, []) ->
    case rpop(Measure, Middle) of
	#ft_empty{} -> from_list(Measure, Left);
	{A, NewMiddle} -> deep(Measure, Left, NewMiddle, [A])
    end;
deepr(Measure, Left, Middle, Right) ->
    deep(Measure, Left, Middle, Right).

split(_Measure, _Predicate, _I, [Elem]) ->
    {[], Elem, []};
split(Measure, Predicate, I, [Elem | Elems]) ->
    case Predicate(Measure([I, Elem])) of
	true -> {[], Elem, [Elems]};
	false ->
	    I2 = Measure([I, Elem]),
	    {L, X, R} = split(Measure, Predicate, I2, Elems),
	    {[Elem | L], X, R}
    end;
split(_Measure, _Predicate, _I, #ft_single{element = Elem}) ->
    {#ft_empty{}, Elem, #ft_empty{}};
split(Measure, Predicate, I, #ft_deep{left = Left, middle = Middle, right = Right}) ->
    VPR = Measure([I, Left]),
    VM = Measure([VPR, Middle]),
    case Predicate(VPR) of
	true ->
	    {L, X, R} = split(Measure, Predicate, I, Left),
	    {from_list(Measure, L), X, deepl(Measure, R, Middle, Right)};
	false ->
	    case Predicate(VM) of
		true ->
		    {ML, MX, MR} = split(Measure, Predicate, VPR, Middle),
		    {L, X, R} = split(Measure, Predicate, Measure([VPR, ML]), [MX]),
		    {deepr(Measure, Left, ML, L), X, deepl(Measure, R, MR, Right)};
		false ->
		    {L, X, R} = split(Measure, Predicate, I, Right),
		    {deepr(Measure, Left, Middle, L), X, from_list(Measure, R)}
	    end
    end.

%% @doc Returns the first element.
first(#ft_single{element = First})  -> First;
first(#ft_deep{left = [First | _]}) -> First.

%% @doc Returns the last element.
last(#ft_single{element = Last})   -> Last;
last(#ft_deep{right = [Last | _]}) -> Last.

%% @doc Groups a list of elements into nodes.
to_nodes(Measure, [A, B]) -> 
    #ft_node2{value = Measure([A | B]), a = A, b = B};
to_nodes(Measure, [A, B, C]) ->  
    #ft_node3{value = Measure([A, B, C]), a = A, b = B, c = C};
to_nodes(Measure, [A, B, C, D]) ->
    [#ft_node2{value = Measure([A, B]), a = A, b = B},
     #ft_node2{value = Measure([C, D]), a = C, b = D}];
to_nodes(Measure, [A, B, C | Tail]) ->
    [#ft_node3{value = Measure([A, B, C]), a = A, b = B, c = C} | [to_nodes(Measure, Tail)]].

%% @doc Appends a tree to another.
append(Measure, #ft_empty{}, Elems, Tree) ->
    lpushlist(Measure, Elems, Tree);
append(Measure, Tree, Elems, #ft_empty{}) ->
    rpushlist(Measure, Elems, Tree);
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

append(Measure, T1, T2) ->
    append(Measure, T1, [], T2).

%% @doc Generates a tree with an integer sequence.
seq(Measure, Tree, From, From) ->
    lpush(Measure, From, Tree);
seq(Measure, Tree, From, To) ->
    seq(Measure, lpush(Measure, To, Tree), From, To - 1).

%% @doc Generates a tree with an integer sequence.
seq(Measure, From, To) ->
    seq(Measure, new(), From, To).

%% @doc Returns the measurement of a node.
value(Measure, #ft_empty{}) ->
    Measure(0, []);
value(Measure, #ft_single{element=Elem}) ->
    Measure(0, Elem);
value(Measure, #ft_deep{ value=Value
		       , left=Left
		       , middle=Middle
		       , right=Right}) -> 
    Measure(Value, [Left, Middle, Right]);
value(Measure, #ft_node2{value=Value, a=A, b=B}) ->
    Measure(Value, [A, B]); 
value(Measure, #ft_node3{value=Value, a=A, b=B, c=C}) ->
    Measure(Value, [A, B, C]).

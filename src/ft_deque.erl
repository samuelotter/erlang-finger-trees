-module(ft_deque).
-compile({no_auto_import, [length/1]}).

-export([ append/2 
	, first/1
	, foldl/3
	, last/1
	, length/1
	, lpush/2
	, measure/1
	, new/0
	, nth/2
	, rpush/2
	, seq/2
	]).

-include("ftree.hrl").

-record(deque, {root = ftree:new()}).

measure([])            -> 0;
measure([Head | Tail]) -> measure(Head) + measure(Tail);
measure(_Elem)         -> 1.

measure(Value, [])            -> Value;
measure(Value, [Head | Tail]) -> Value + measure(Head) + measure(Tail);
measure(Value, _Elem)          -> Value + 1.

%% @doc Returns the number of elements in Deque.
length(#deque{root = Root}) ->
    ftree:value(fun measure/2, Root).

%% @doc Creates a new empty deque.
new() -> #deque{}.

%% @doc Prepends an element to Deque.
lpush(Elem, Deque = #deque{root = Root}) -> 
    Deque#deque{root = ftree:lpush(fun measure/1, Elem, Root)}.

%% @doc Appends a element to Deque.
rpush(Elem, Deque = #deque{root = Root}) -> 
    Deque#deque{root = ftree:rpush(fun measure/1, Elem, Root)}.

%% @doc Calls Fun(X, Acc) on each element in Deque, starting with the leftmost one. The result of each call is passed 
%%      on in the Acc argument to Fun. Acc0 is passed to the first call to Fun. The result of the final call to Fun/2 
%% 		is returned.  
foldl(Fun, Acc0, #deque{root = Root}) ->
    ftree:foldl(Fun, Acc0, Root).
	
%% @doc Returns the first element in Deque.
first(#deque{root = Root}) ->
    ftree:first(Root).

%% @doc Returns the last element in Deque.
last(#deque{root = Root}) ->
    ftree:last(Root).

%% @doc Appends one tree to another.
append(#deque{root = Root1}, 
       #deque{root = Root2}) ->
    ftree:append(fun measure/1, Root1, [], Root2).

%% @doc Generates a sequence of integers and adds them to a new tree.
seq(From, To) -> #deque{root = ftree:seq(fun measure/1, From, To)}.

%% @doc Returns the nth element from the beginning of Deque.
%%
%% Uses 1-based indexing for consistency with lists.
nth(1, #ft_single{element = Element}) -> 
    Element;
nth(N, #ft_deep{left = Left, middle = Middle, right = Right}) -> 
    LenLeft = ftree:value(fun measure/1, Left),
    LenMiddle = ftree:value(fun measure/1, Middle),
    if 
	N =< LenLeft -> nth(N, Left);
	N =< LenMiddle -> nth(N - LenLeft, Middle);
	true -> nth(N - LenLeft - LenMiddle, Right)
    end;
nth(N, #ft_node2{a = A, b = B}) ->
    ALen = ftree:value(fun measure/1, A),
    if 
	N =< ALen -> nth(N, A);
	true -> nth(N - ALen, B)
    end;
nth(N, #ft_node3{a = A, b = B, c = C}) ->
    ALen = ftree:value(fun measure/1, A),
    BLen = ftree:value(fun measure/1, B),
    if 
	N =< ALen -> nth(N, A);
	N - ALen =< BLen -> nth(N - ALen, B);
	true -> nth(N - ALen - BLen, C)
    end;
nth(N, [Elem | Tail]) -> 
    Len = ftree:value(fun measure/1, Elem),
    if 
	N =< Len -> nth(N, Elem);
	N > Len -> nth(N - Len, Tail)
    end; 
nth(N, #deque{root = Root}) -> nth(N, Root);
nth(_N, Elem) -> Elem.


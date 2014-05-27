%%==============================================================================
%% ftree.erl
%% @author Samuel Otter
%% @copyright 2013, Samuel Otter
%%
%% @doc An implementation of 2-3 Finger Trees.
%%==============================================================================

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

-type ftree(E)   :: #ft_deep{} |
                       %% left :: list(E),
                       %%       middle :: ftree(E),
                       %%      right :: list(E)} |
                    #ft_node3{a :: E, b :: E, c :: E} |
                    #ft_node2{a :: E, b :: E} |
                    #ft_empty{} |
                    #ft_single{element :: E}.
-type ftree()    :: ftree(any()).

-callback value_of(Elements :: list()) ->
    any().
-callback merge(Values :: list()) ->
    any().

-type callback() :: atom.

%% @doc Creates a new empty finger-tree.
-spec new() -> ftree().
new() -> #ft_empty{}.

%% @doc Inserts a new element to the left (at the front).
-spec lpush(callback(), E, ftree(E)) -> ftree(E).
lpush(_Callback, Elem, #ft_empty{}) ->
    #ft_single{element = Elem};
lpush(Callback, Elem, #ft_single{element = Existing}) ->
    #ft_deep{value  = measure(Callback, [Elem, Existing]),
             left   = [Elem],
             middle = #ft_empty{},
             right  = [Existing]};
lpush(Callback, Elem, Tree = #ft_deep{ left   = [L0, L1, L2, L3],
                                       middle = Middle,
                                       right  = Right}) ->
    NewLeft   = [Elem, L0],
    Node      = #ft_node3{ value = measure(Callback, [L1, L2, L3])
                         , a = L1
                         , b = L2
                         , c = L3},
    NewMiddle = lpush(Callback, Node, Middle),
    Tree#ft_deep{value  = measure(Callback, NewLeft, NewMiddle, Right),
                 left   = NewLeft,
                 middle = NewMiddle};
lpush(Callback, Elem, Deep = #ft_deep{ left   = Left
                                     , middle = Middle
                                     , right  = Right}) ->
    Deep#ft_deep{ value = measure(Callback, Elem, Left, Middle, Right)
                , left  = [Elem | Left]}.


%% @doc Inserts a new element to the right (at the back).
-spec rpush(callback(), E, ftree(E)) -> ftree(E).
rpush(_Callback, Elem, #ft_empty{}) ->
    #ft_single{element = Elem};
rpush(Callback, NewElem, #ft_single{element=Elem}) ->
    #ft_deep{value = measure(Callback, [Elem, NewElem]),
             left  = [Elem],
             right = [NewElem]};
rpush(Callback, Elem, Tree = #ft_deep{left   = Left,
                                      right  = [R0, R1, R2, R3],
                                      middle = Middle}) ->
    NewMiddle = rpush(Callback,
                      #ft_node3{ value = value(Callback, [R1, R2, R3])
                               , a = R1
                               , b = R2
                               , c = R3},
                      Middle),
    NewRight = [Elem | R0],
    Tree#ft_deep{value = measure(Callback, Left, NewMiddle, NewRight),
                 middle = NewMiddle,
                 right = NewRight};
rpush(Callback, Elem, Tree = #ft_deep{ left = Left
                                     , middle = Middle
                                     , right = Right}) ->
    Tree#ft_deep{ value = measure(Callback, Elem, Left, Middle, Right)
                , right = [Elem | Right]}.

%% @doc Removes and returns the first element.
-spec lpop(callback(), ftree(E)) -> ftree(E).
lpop(_Callback, #ft_empty{}) -> #ft_empty{};
lpop(_Callback, #ft_single{element = Elem}) -> {Elem, #ft_empty{}};
lpop(Callback, Node = #ft_deep{ left = [Elem | LTail]
                              , middle = Middle
                              , right = Right}) ->
    Tail = case LTail of
               [] ->
                   case lpop(Callback, Middle) of
                       #ft_empty{} -> from_list(Callback, Right);
                       {A, Middle2} -> deep(Callback, [A], Middle2, Right)
                   end;
               _ -> Node#ft_deep{left = LTail}
           end,
    {Elem, Tail}.

%% @doc Removes and returns the last element.
-spec rpop(callback(), ftree(E)) -> ftree(E).
rpop(_Callback, #ft_empty{}) -> #ft_empty{};
rpop(_Callback, #ft_single{element = Elem}) -> {Elem, #ft_empty{}};
rpop(Callback, Node = #ft_deep{ left = Left
                              , middle = Middle
                              , right = [Elem | RTail]}) ->
    Tail = case RTail of
               [] ->
                   case rpop(Callback, Middle) of
                       #ft_empty{} -> from_list(Callback, Left);
                       {A, Middle2} -> deep(Callback, Left, Middle2, [A])
                   end;
               _ -> Node#ft_deep{left = RTail}
           end,
    {Elem, Tail}.

%% @doc Pushes a list of elements at the front.
lpushlist(Callback, List, Tree) ->
    lists:foldr(fun (X, T) -> lpush(Callback, X, T) end, Tree, List).

%% @doc Pushes a list of elements at the back.
rpushlist(Callback, List, Tree) ->
    lists:foldl(fun (X, T) -> rpush(Callback, X, T) end, Tree, List).

%% @doc Calls `Fun(Elem, Acc)' on each element in the Tree, starting
%%      with the first one. The result from each call is passed to the
%%      next in the second argument. The result of the last call to
%%      Fun is returned from the function.
-spec foldl(fun((E, A) -> A), A, ftree(E)) -> A.
foldl(Fun, Acc, #ft_node2{a = A, b = B}) ->
    foldl(Fun, foldl(Fun, Acc, A), B);
foldl(Fun, Acc, #ft_node3{a = A, b = B, c = C}) ->
    foldl(Fun, foldl(Fun, foldl(Fun, Acc, A), B), C);
foldl(_Fun, Acc, #ft_empty{}) ->
    Acc;
foldl(Fun, Acc, #ft_single{element = Elem}) ->
    Fun(Elem, Acc);
foldl(Fun, Acc0, #ft_deep{left = Left, middle = Middle, right = Right}) ->
    Acc1 = lists:foldl(fun (Elem, Result) -> foldl(Fun, Result, Elem) end,
                       Acc0,
                       Left),
    Acc2 = foldl(Fun, Acc1, Middle),
    lists:foldr(fun (Elem, Result) -> foldl(Fun, Result, Elem) end,
                Acc2,
                Right);
foldl(Fun, Acc, X) ->
    Fun(X, Acc).

%% @doc Calls `Fun(Elem, Acc)' on each element in the Tree, starting
%%      with the last one. The result from each call is passed to the
%%      next in the second argument. The result of the last call to
%%      Fun is returned from the function.
-spec foldr(fun((E, A) -> A), A, ftree(E)) -> A.
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
-spec to_list(ftree(E)) -> list(E).
to_list(Tree) ->
    lists:reverse(foldl(fun (V, Acc) -> [V | Acc] end, [], Tree)).

%% @doc Creates a new tree from a list.
-spec from_list(callback(), list(E)) -> ftree(E).
from_list(Callback, List) ->
    lists:foldr(fun (X, Tree) -> lpush(Callback, X, Tree) end, new(), List).

%% @doc Creates a new deep node.
deep(Callback, Left, Middle, Right) ->
    #ft_deep{ value = measure(Callback, [Left, Middle, Right])
            , left = Left
            , middle = Middle
            , right = Right}.

deepl(Callback, [], Middle, Right) ->
    case lpop(Callback, Middle) of
        #ft_empty{} -> from_list(Callback, Right);
        {A, NewMiddle} -> deep(Callback, [A], NewMiddle, Right)
    end;
deepl(Callback, Left, Middle, Right) ->
    deep(Callback, Left, Middle, Right).

deepr(Callback, Left, Middle, []) ->
    case rpop(Callback, Middle) of
        #ft_empty{} -> from_list(Callback, Left);
        {A, NewMiddle} -> deep(Callback, Left, NewMiddle, [A])
    end;
deepr(Callback, Left, Middle, Right) ->
    deep(Callback, Left, Middle, Right).

split(_Callback, _Predicate, _I, [Elem]) ->
    {[], Elem, []};
split(Callback, Predicate, I, [Elem | Elems]) ->
    case Predicate(measure(Callback, [I, Elem])) of
        true -> {[], Elem, [Elems]};
        false ->
            I2 = measure(Callback, [I, Elem]),
            {L, X, R} = split(Callback, Predicate, I2, Elems),
            {[Elem | L], X, R}
    end;
split(_Callback, _Predicate, _I, #ft_single{element = Elem}) ->
    {#ft_empty{}, Elem, #ft_empty{}};
split(Callback, Predicate, I, #ft_deep{left = Left, middle = Middle, right = Right}) ->
    VPR = measure(Callback, [I, Left]),
    VM = measure(Callback, [VPR, Middle]),
    case Predicate(VPR) of
        true ->
            {L, X, R} = split(Callback, Predicate, I, Left),
            {from_list(Callback, L), X, deepl(Callback, R, Middle, Right)};
        false ->
            case Predicate(VM) of
                true ->
                    {ML, MX, MR} = split(Callback, Predicate, VPR, Middle),
                    {L, X, R} = split(Callback, Predicate, Callback([VPR, ML]), [MX]),
                    {deepr(Callback, Left, ML, L), X, deepl(Callback, R, MR, Right)};
                false ->
                    {L, X, R} = split(Callback, Predicate, I, Right),
                    {deepr(Callback, Left, Middle, L), X, from_list(Callback, R)}
            end
    end.

%% @doc Returns the first element.
-spec first(ftree(E)) -> E.
first(#ft_single{element = First})  -> First;
first(#ft_deep{left = [First | _]}) -> First.

%% @doc Returns the last element.
-spec last(ftree(E)) -> E.
last(#ft_single{element = Last})   -> Last;
last(#ft_deep{right = [Last | _]}) -> Last.

%% @doc Groups a list of elements into nodes.
to_nodes(Callback, [A, B]) ->
    #ft_node2{ value = measure(Callback, [A | B])
             , a = A
             , b = B};
to_nodes(Callback, [A, B, C]) ->
    #ft_node3{ value = measure(Callback, [A, B, C])
             , a = A
             , b = B
             , c = C};
to_nodes(Callback, [A, B, C, D]) ->
    [ #ft_node2{ value = measure(Callback, [A, B])
              , a = A
              , b = B}
    , #ft_node2{ value = measure(Callback, [C, D])
              , a = C
              , b = D}
    ];
to_nodes(Callback, [A, B, C | Tail]) ->
    [ #ft_node3{ value = measure(Callback, [A, B, C])
               , a = A
               , b = B
               , c = C}
    | [to_nodes(Callback, Tail)]
    ].

%% @doc Appends a tree to another.
-spec append(callback(), ftree(E), list(E), ftree(E)) -> ftree(E).
append(Callback, #ft_empty{}, Elems, Tree) ->
    lpushlist(Callback, Elems, Tree);
append(Callback, Tree, Elems, #ft_empty{}) ->
    rpushlist(Callback, Elems, Tree);
append(Callback, #ft_single{element = Elem}, Elems, Tree) ->
    lpush(Callback, Elem, lpushlist(Callback, Elems, Tree));
append(Callback, Tree, Elems, #ft_single{element = Elem}) ->
    rpush(Callback, Elem, rpushlist(Callback, Elems, Tree));
append(Callback, #ft_deep{left = L1, middle = M1, right = R1},
       Elems,
       #ft_deep{left = L2, middle = M2, right = R2}) ->
    NewMiddle = append(Callback, M1, to_nodes(Callback, R1 ++ Elems ++ L2), M2),
    #ft_deep{value = measure(Callback, [L1, NewMiddle, R2]),
             left = L1,
             middle = NewMiddle,
             right = R2}.

-spec append(callback(), ftree(E), ftree(E)) -> ftree(E).
append(Callback, T1, T2) ->
    append(Callback, T1, [], T2).

%% @doc Generates a tree with an integer sequence.
-spec seq(callback(), ftree(integer()), integer(), integer()) ->
                 ftree(integer()).
seq(Callback, Tree, From, From) ->
    lpush(Callback, From, Tree);
seq(Callback, Tree0, From, To) ->
    Tree = lpush(Callback, To, Tree0),
    seq(Callback, Tree, From, To - 1).

%% @doc Generates a tree with an integer sequence.
-spec seq(callback(), integer(), integer()) -> ftree(integer()).
seq(Callback, From, To) ->
    seq(Callback, new(), From, To).

%% @doc Returns the measurement of a node.
-spec value(callback(), ftree()) -> any().
value(Callback, #ft_empty{}) ->
    Callback:zero();
value(Callback, #ft_single{element=Elem}) ->
    value(Callback, Elem);
value(_Callback, #ft_deep{value=Value}) ->
    Value;
value(_Callback, #ft_node2{value=Value}) ->
    Value;
value(_Callback, #ft_node3{value=Value}) ->
    Value;
value(Callback, Element) ->
    Callback:value_of(Element).

measure(Callback, Elements) ->
    Values = [value(Callback, Elem) || Elem <- Elements],
    Callback:merge(Values).

measure(Callback, Left, Middle, Right) ->
    LeftValues = [value(Callback, E) || E <- Left],
    MiddleValue = value(Callback, Middle),
    RightValues = [value(Callback, E) || E <- Right],
    Values = LeftValues ++ [MiddleValue] ++ RightValues,
    Callback:merge(Values).

measure(Callback, Elem, Left, Middle, Right) ->
    LeftValues = [value(Callback, E) || E <- Left],
    MiddleValue = value(Callback, Middle),
    RightValues = [value(Callback, E) || E <- Right],
    ElemValue = value(Callback, Elem),
    Values = [ElemValue] ++ LeftValues ++ [MiddleValue] ++ RightValues,
    Callback:merge(Values).

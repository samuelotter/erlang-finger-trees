-ifndef(_FT_TYPES_HRL_).
-define(_FT_TYPES_HRL_, true).

-record(ft_empty, {}).
-record(ft_single, {element}).
-record(ft_deep, {value,
                  left = [] :: list(),
                  middle = #ft_empty{},
                  right = [] :: list()}).
-record(ft_node2, {value, a :: any(), b :: any()}).
-record(ft_node3, {value, a :: any(), b :: any(), c :: any()}).

-endif.

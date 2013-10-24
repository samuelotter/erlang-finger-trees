-record(ft_empty, {}).
-record(ft_single, { data }).
-record(ft_deep, { value = 0, left = [], middle = #ft_empty{}, right = []}).
-record(ft_node2, { value = 0, a, b}).
-record(ft_node3, { value = 0, a, b, c}).


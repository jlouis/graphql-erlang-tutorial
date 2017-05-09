%% tag::scalarRepr[]
-module(sw_core_scalar).

-export([input/2, output/2]).

input(_Type, Val) ->
    {ok, Val}.

output(_Type, Val) ->
    {ok, Val}.
%% end::scalarRepr[]

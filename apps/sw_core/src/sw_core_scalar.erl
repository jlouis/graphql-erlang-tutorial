%% tag::scalarRepr[]
-module(sw_core_scalar).

-export([input/2, output/2]).

input(<<"DateTime">>, Input) ->
    try iso8601:parse(Input) of
        DateTime -> {ok, DateTime}
    catch
        error:badarg ->
            {error, bad_date}
    end;
input(_Type, Val) ->
    {ok, Val}.

output(<<"DateTime">>, DateTime) ->
    {ok, iso8601:format(DateTime)};
output(_Type, Val) ->
    {ok, Val}.
%% end::scalarRepr[]

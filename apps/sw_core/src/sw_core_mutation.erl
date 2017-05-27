-module(sw_core_mutation).

-export([execute/4]).

execute(Ctx, _, Field, #{ <<"input">> := Input}) ->
    with_client_mutation(Ctx, Field, Input).

with_client_mutation(Ctx, Field, Input) ->
    {CM, Rest} = maps:take(<<"clientMutationId">>, Input),
    case execute_mutation(Ctx, Field, Rest) of
        {ok, Payload} ->
            {ok, Payload#{ <<"clientMutationId">> => CM }};
        {error, Reason} ->
            {error, Reason}
    end.

execute_mutation(Ctx, <<"introduceFaction">>, Input) ->
    {ok, Faction} = sw_core_faction:introduce(Ctx, Input),
    {ok, #{ <<"faction">> => Faction }};
execute_mutation(Ctx, <<"introduceStarship">>, Input) ->
    {ok, Faction, Starship} = sw_core_starship:introduce(Ctx, Input),
    {ok, #{ <<"faction">> => Faction,
            <<"starship">> => Starship }}.

    

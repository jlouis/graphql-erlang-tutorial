-module(sw_core_query).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::execute[]
execute(_Ctx, _DummyObj, <<"node">>, #{ <<"id">> := ID }) ->
    load_node(any, ID);
execute(_Ctx, _DummyObj, <<"starship">>, #{ <<"id">> := ID }) ->
    load_node(['Starship'], ID).
%% end::execute[]


%% tag::loadNode[]
load_node(Types, ID) when is_binary(ID) ->
    case sw_core_id:decode(ID) of
        {ok, Decoded} ->
            load_node_(Types, Decoded);
        {error, Reason} ->
            {error, Reason}
    end.

load_node_(any, {Type, MID}) ->
    sw_core_db:load(Type, MID);
load_node_(TypeList, {Type, MID}) ->
    case lists:member(Type, TypeList) of
        true ->
            sw_core_db:load(Type, MID);
        false ->
            {error, wrong_type}
    end.
%% end::loadNode[]

-module(sw_core_type).

-include("sw_core_db.hrl").
-export([execute/1]).

%% tag::resolveType[]
execute(#starship{}) -> {ok, 'Starship'};
execute(#planet{}) -> {ok, 'Planet'};
execute(#film{}) -> {ok, 'Film'};
execute(_Otherwise) -> {error, unknown_type}.
%% end::resolveType[]
    

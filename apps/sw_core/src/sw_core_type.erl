-module(sw_core_type).

-include("sw_core_db.hrl").
-export([execute/1]).

%% tag::resolveType[]
execute(#film{}) -> {ok, 'Film'};
execute(#person{}) -> {ok, 'Person'};
execute(#planet{}) -> {ok, 'Planet'};
execute(#species{}) -> {ok, 'Species'};
execute(#starship{}) -> {ok, 'Starship'};
execute(#transport{}) -> {ok, 'Transport'};
execute(#vehicle{}) -> {ok, 'Vehicle'};
execute(#faction{}) -> {ok, 'Faction'};
execute(_Otherwise) -> {error, unknown_type}.
%% end::resolveType[]
    

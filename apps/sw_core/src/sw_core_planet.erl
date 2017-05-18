-module(sw_core_planet).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::planetExecute[]
execute(_Ctx, Planet, Field, _Args) ->
    case Field of
        <<"id">> -> {ok, sw_core_id:encode({planet, Planet#planet.id})};
        <<"edited">> -> {ok, Planet#planet.edited};
        <<"climate">> -> {ok, Planet#planet.climate};
        <<"surfaceWater">> -> {ok, Planet#planet.surface_water};
        <<"name">> -> {ok, Planet#planet.name};
        <<"diameter">> -> {ok, Planet#planet.diameter};
        <<"rotationPeriod">> -> {ok, Planet#planet.rotation_period};
%% end::planetExecute[]
        <<"created">> -> {ok, Planet#planet.created};
        <<"terrain">> -> {ok, Planet#planet.terrain};
        <<"gravity">> -> {ok, Planet#planet.gravity};
        <<"orbitalPeriod">> -> {ok, Planet#planet.orbital_period};
        <<"population">> -> {ok, Planet#planet.population}
    end.

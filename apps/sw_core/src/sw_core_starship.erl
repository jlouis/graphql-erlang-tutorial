-module(sw_core_starship).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::starshipExecute[]
execute(_Ctx, Starship, Field, _Args) ->
    case Field of
        <<"id">> ->
            {ok, sw_core_id:encode({starship, Starship#starship.id})};
        <<"cargoCapacity">>    -> {ok, Starship#starship.cargo_capacity};
        <<"class">>            -> {ok, Starship#starship.class};
        <<"consumables">>      -> {ok, Starship#starship.consumables};
        <<"cost">>             -> {ok, Starship#starship.cost};
%% end::starshipExecute[]
        <<"model">>            -> {ok, Starship#starship.model};
        <<"created">>          -> {ok, Starship#starship.created};
        <<"crew">>             -> {ok, Starship#starship.crew};
        <<"edited">>           -> {ok, Starship#starship.edited};
        <<"hyperdriveRating">> -> {ok, Starship#starship.hyperdrive_rating};
        <<"length">>           -> {ok, Starship#starship.length};
        <<"manufacturers">>    -> {ok, Starship#starship.manufacturers};
        <<"maxSpeed">>         -> {ok, Starship#starship.max_speed};
        <<"mglt">>             -> {ok, Starship#starship.mglt};
        <<"name">>             -> {ok, Starship#starship.name};
        <<"passengers">>       -> {ok, Starship#starship.passengers}
    end.

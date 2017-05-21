-module(sw_core_starship).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::starshipExecute[]
execute(_Ctx, #{ starship := Starship,
                 transport := Transport }, Field, _Args) ->
    case Field of
        <<"id">> ->
            {ok, sw_core_id:encode({'Starship', Starship#starship.id})};
        <<"name">> -> {ok, Transport#transport.name};
        <<"model">> -> {ok, Transport#transport.model};
        <<"starshipClass">> -> {ok, Starship#starship.starship_class};
        <<"costInCredits">> -> {ok, Transport#transport.cost};
        <<"length">> -> {ok, Transport#transport.length};
        <<"crew">> -> {ok, Transport#transport.crew};
        <<"passengers">> -> {ok, Transport#transport.passengers};
        <<"maxAtmospheringSpeed">> ->
            {ok, Transport#transport.max_atmosphering_speed};
        <<"hyperdriveRating">> ->
            {ok, Starship#starship.hyperdrive_rating};
        <<"MGLT">> ->
            {ok, Starship#starship.mglt};
        <<"cargoCapacity">> -> {ok, Transport#transport.cargo_capacity};
        <<"consumables">> -> {ok, Transport#transport.consumables};
        <<"created">> -> {ok, Transport#transport.created};
        <<"edited">> ->  {ok, Transport#transport.edited}
%% end::starshipExecute[]
    end.

-module(sw_core_vehicle).
-include("sw_core_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([execute/4]).

%% tag::starshipExecute[]
execute(_Ctx, #{ vehicle := #vehicle { id = VehicleId } = Vehicle,
                 transport := Transport }, Field, Args) ->
    case Field of
        <<"id">> ->
            {ok, sw_core_id:encode({'Vehicle', Vehicle#vehicle.id})};
        <<"name">> -> {ok, Transport#transport.name};
        <<"model">> -> {ok, Transport#transport.model};
        <<"vehicleClass">> -> {ok, Vehicle#vehicle.vehicle_class};
        <<"costInCredits">> -> {ok, Transport#transport.cost};
        <<"length">> -> {ok, Transport#transport.length};
        <<"crew">> -> {ok, Transport#transport.crew};
        <<"passengers">> -> {ok, Transport#transport.passengers};
        <<"manufacturers">> -> {ok, [{ok, M} || M <- Transport#transport.manufacturers]};
        <<"maxAtmospheringSpeed">> ->
            {ok, Transport#transport.max_atmosphering_speed};
        <<"cargoCapacity">> -> {ok, Transport#transport.cargo_capacity};
        <<"consumables">> -> {ok, Transport#transport.consumables};
        <<"created">> -> {ok, Transport#transport.created};
        <<"edited">> ->  {ok, Transport#transport.edited};
%% end::starshipExecute[]
        <<"pilotConnection">> ->
            #vehicle { pilots = Pilots } = Vehicle,
            Txn = fun() ->
                          [mnesia:read(person, P) || P <- Pilots]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"filmConnection">> ->
            Txn = fun() ->
                          QH = qlc:q([F || F <- mnesia:table(film),
                                           lists:member(VehicleId, F#film.vehicles)]),
                          qlc:e(QH)
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(Records, Args)
    end.

-module(sw_core_starship).
-include("sw_core_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([execute/4]).
-export([introduce/2]).

%% tag::starshipExecute[]
execute(_Ctx, #{ starship := #starship { id = StarshipId } = Starship,
                 transport := Transport }, Field, Args) ->
    case Field of
        <<"id">> ->
            {ok, sw_core_id:encode({'Starship', Starship#starship.id})};
        <<"name">> -> {ok, Transport#transport.name};
        <<"model">> -> {ok, Transport#transport.model};
        <<"starshipClass">> -> {ok, Starship#starship.starship_class};
        <<"costInCredits">> -> {ok, floatify(Transport#transport.cost)};
        <<"length">> -> {ok, Transport#transport.length};
        <<"crew">> -> {ok, Transport#transport.crew};
        <<"passengers">> ->
            Passengers = Transport#transport.passengers,
            {ok, Passengers};
        <<"manufacturers">> -> {ok, [{ok, M} || M <- Transport#transport.manufacturers]};
        <<"maxAtmospheringSpeed">> ->
            {ok, Transport#transport.max_atmosphering_speed};
        <<"hyperdriveRating">> ->
            {ok, Starship#starship.hyperdrive_rating};
        <<"MGLT">> ->
            {ok, Starship#starship.mglt};
        <<"cargoCapacity">> ->
            Capacity = Transport#transport.cargo_capacity,
            {ok, floatify(Capacity)};
        <<"consumables">> -> {ok, Transport#transport.consumables};
        <<"created">> -> {ok, Transport#transport.created};
        <<"edited">> ->  {ok, Transport#transport.edited};
%% end::starshipExecute[]
        <<"pilotConnection">> ->
            #starship { pilots = Pilots } = Starship,
            Txn = fun() ->
                          [mnesia:read(person, P) || P <- Pilots]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"filmConnection">> ->
            Txn = fun() ->
                          QH = qlc:q([F || F <- mnesia:table(film),
                                           lists:member(StarshipId, F#film.starships)]),
                          qlc:e(QH)
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(Records, Args)
    end.

%% tag::introduce[]
introduce(_Ctx, #{ <<"name">> := Name,
                   <<"model">> := Model,
                   <<"starshipClass">> := Class,
                   <<"manufacturers">> := Manufacturers,
                   <<"costInCredits">> := Cost,
                   <<"length">> := Length,
                   <<"crew">> := Crew,

                   <<"faction">> := FactionInput }) ->
    ID = sw_core_db:nextval(transport),
    Transport = #transport { id = ID,
                             name = Name,
                             created = current_time(),
                             edited = current_time(),
                             crew = Crew,
                             model = Model,
                             cost = Cost,
                             length = Length,
                             manufacturers = Manufacturers },
    Starship = #starship { id = ID,
                           starship_class = Class },
    {ok, {'Faction', FactionID}} = sw_core_id:decode(FactionInput),
    case sw_core_db:load('Faction', FactionID) of
        {ok, #faction { id = FactionRef } = Faction} ->
            Txn = fun() ->
                          ok = mnesia:write(Starship),
                          ok = mnesia:write(Transport#transport {
                                              faction = FactionRef
                                             }),
                          ok
                  end,
            {atomic, ok} = mnesia:transaction(Txn),
            {ok, Faction, #{ starship => Starship,
                             transport => Transport#transport {
                                            faction = FactionRef
                                           }}};
        {error, Reason} ->
            {error, Reason}
    end.
                           
%% end::introduce[]
floatify(nan) -> null;
floatify(I) -> float(I).

current_time() ->
    calendar:universal_time().


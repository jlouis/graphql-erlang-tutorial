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
            Result = case Transport#transport.passengers of
                         undefined -> null;
                         P -> P
                     end,
            {ok, Result};
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
        <<"consumables">> -> {ok,
                              case Transport#transport.consumables of
                                  undefined -> null;
                                  V -> V
                              end};
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
    ID = sw_core_db:nextval(transport), % <1>
    Transport = #transport { id = ID,
                             name = Name,
                             created = current_time(),
                             edited = current_time(),
                             crew = Crew,
                             model = Model,
                             cost = Cost,
                             length = Length,
                             passengers = undefined,
                             consumables = undefined,
                             max_atmosphering_speed = 0,
                             cargo_capacity = nan,
                             manufacturers = Manufacturers },
    Starship = #starship { id = ID,
                           pilots = [],
                           mglt = 0,
                           hyperdrive_rating = 0.0,
                           starship_class = Class }, % <2>
    {ok, {'Faction', FactionID}} =
        sw_core_id:decode(FactionInput), % <3>
    case sw_core_db:load('Faction', FactionID) of % <4>
        {ok, #faction { id = FactionRef } = Faction} ->
            Txn = fun() ->
                          ok = mnesia:write(Starship),
                          ok = mnesia:write(Transport#transport {
                                              faction = FactionRef
                                             }), % <5>
                          ok
                  end,
            {atomic, ok} = mnesia:transaction(Txn),
            {ok, Faction, #{ starship => Starship,
                             transport => Transport#transport {
                                            faction = FactionRef
                                           }}}; % <6>
        {error, Reason} ->
            {error, Reason}
    end.
%% end::introduce[]


floatify(nan) -> null;
floatify(I) -> float(I).

current_time() ->
    calendar:universal_time().


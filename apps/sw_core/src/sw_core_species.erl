-module(sw_core_species).
-include("sw_core_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([execute/4]).

%% tag::execute[]
execute(_Ctx, #species { id = Id } = Species, Field, Args) ->
    case Field of
        <<"id">> -> {ok, sw_core_id:encode({'Species', Id})};
        <<"eyeColors">> ->
            {ok,
             [{ok, EC} || EC <- Species#species.eye_colors]};
%% end::execute[]
        <<"classification">> -> {ok, Species#species.classification};
        <<"designation">> -> {ok, Species#species.designation};
        <<"averageHeight">> -> {ok, Species#species.average_height};
        <<"averageLifespan">> -> {ok, Species#species.average_lifespan};
        <<"hairColors">> ->
            {ok,
             [{ok, EC} || EC <- Species#species.hair_colors]};
        <<"skinColors">> ->
            {ok,
             [{ok, EC} || EC <- Species#species.skin_colors]};
        <<"language">> -> {ok, Species#species.language};
        <<"homeworld">> -> {ok, Species#species.homeworld};
        <<"personConnection">> ->
            Txn = fun() ->
                          QH = qlc:q([P || P <- mnesia:table(person),
                                           lists:member(P#person.id,
                                                        Species#species.people)]),
                          qlc:e(QH)
                  end,
            {atomic, People} = mnesia:transaction(Txn),
            sw_core_paginate:select(People, Args);
        <<"filmConnection">> ->
            Txn = fun() ->
                          QH = qlc:q([F || P <- mnesia:table(person),
                                           lists:member(P#person.id,
                                                        Species#species.people),
                                           F <- mnesia:table(film),
                                           lists:member(P#person.id,
                                                        F#film.characters)]),
                          qlc:e(qlc:sort(QH, [{unique, true}]))
                  end,
            {atomic, Films} = mnesia:transaction(Txn),
            sw_core_paginate:select(Films, Args);
        <<"created">> -> {ok, Species#species.created};
        <<"edited">> -> {ok, Species#species.edited}
    end.

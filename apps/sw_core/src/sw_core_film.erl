-module(sw_core_film).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::filmExecute[]
execute(_Ctx, #film{} = Film, Field, Args) ->
    case Field of
        %% Plain Fields
        <<"id">> -> {ok, sw_core_id:encode({'Film', Film#film.id})};
        <<"edited">> -> {ok, Film#film.edited};
        <<"created">> -> {ok, Film#film.created};
        <<"producers">> -> {ok, [{ok, P} || P <- Film#film.producer]};
        <<"title">> -> {ok, Film#film.title};
        <<"episodeID">> -> {ok, Film#film.episode_id};
        <<"director">> -> {ok, Film#film.director};
        <<"releaseDate">> -> {ok, Film#film.release_date};
        <<"openingCrawl">> -> {ok, Film#film.opening_crawl};
        <<"planetConnection">> ->
            #film { planets = Planets } = Film,
            Txn = fun() ->
                          [mnesia:read(planet, P) || P <- Planets]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"characterConnection">> ->
            #film { characters = Characters } = Film,
            Txn = fun() ->
                          [mnesia:read(person, P) || P <- Characters]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"speciesConnection">> ->
            #film { species = Species } = Film,
            Txn = fun() ->
                          [mnesia:read(species, S) || S <- Species]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"starshipConnection">> ->
            #film { starships = Starships } = Film,
            Txn = fun() ->
                          [mnesia:read(species, S) || S <- Starships]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args);
        <<"vehicleConnection">> ->
            #film { vehicles = Vehicles } = Film,
            Txn = fun() ->
                          [mnesia:read(species, V) || V <- Vehicles]
                  end,
            {atomic, Records} = mnesia:transaction(Txn),
            sw_core_paginate:select(lists:append(Records), Args)
    end.
%% end::filmExecute[]


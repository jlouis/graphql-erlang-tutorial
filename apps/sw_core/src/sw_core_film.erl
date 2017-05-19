-module(sw_core_film).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::filmExecute[]
execute(_Ctx, Film, Field, Args) ->
    case Field of
        %% Plain Fields
        <<"id">> -> {ok, sw_core_id:encode({'Film', Film#film.id})};
        <<"edited">> -> {ok, Film#film.edited};
        <<"created">> -> {ok, Film#film.created};
        <<"producer">> -> {ok, Film#film.producer};
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
            sw_core_paginate:select(lists:append(Records), Args)
    end.
%% end::filmExecute[]


-module(sw_core_film).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::filmExecute[]
execute(_Ctx, Film, Field, _Args) ->
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
        <<"openingCrawl">> -> {ok, Film#film.opening_crawl}
    end.
%% end::filmExecute[]

-module(sw_core_starship).
-include("sw_core_db.hrl").

-export([execute/4]).

execute(_Ctx, Starship, Field, _Args) ->
    case Field of
        <<"id">> ->
            {ok, sw_core_id:encode({starship, Starship#starship.id})};
        <<"hyperdriveRating">> -> {ok, Starship#starship.hyperdrive_rating};
        <<"mglt">>             -> {ok, Starship#starship.mglt}
    end.

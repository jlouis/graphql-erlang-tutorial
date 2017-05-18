-module(sw_core_person).
-include("sw_core_db.hrl").

-export([execute/4]).

%% tag::planetExecute[]
execute(_Ctx, #person { id = PersonId } = Person, Field, _Args) ->
    case Field of
        <<"id">> -> {ok, sw_core_id:encode({'Person', PersonId})};
        <<"edited">> -> {ok, Person#person.edited};
        <<"name">> -> {ok, Person#person.name};
        <<"created">> -> {ok, Person#person.created};
        <<"gender">> -> {ok, Person#person.gender};
        <<"skinColor">> -> {ok, Person#person.skin_color};
        <<"hairColor">> -> {ok, Person#person.hair_color};
        <<"height">> -> {ok, Person#person.height};
        <<"eyeColor">> -> {ok, Person#person.eye_color};
        <<"mass">> -> {ok, Person#person.mass};
        %% TBD
        <<"homeworld">> -> {ok, Person#person.homeworld};
        <<"birthYear">> -> {ok, Person#person.birth_year}
    end.

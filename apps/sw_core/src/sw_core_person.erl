-module(sw_core_person).
-include("sw_core_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

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
        <<"mass">> -> {ok, floatify(Person#person.mass)};
        <<"homeworld">> ->
            Txn = fun() ->
                          mnesia:read(planet, Person#person.homeworld)
                  end,
            {atomic, [Planet]} = mnesia:transaction(Txn),
            {ok, Planet};
        <<"species">> ->
            Txn = fun() ->
                          QH = qlc:q([S || S <- mnesia:table(species),
                                           lists:member(PersonId, S#species.people)]),
                          qlc:e(QH)
                  end,
            case mnesia:transaction(Txn) of
                {atomic, [Species]} -> {ok, Species};
                {atomic, []} -> {ok, null}
            end;
        <<"birthYear">> -> {ok, Person#person.birth_year}
    end.

floatify(nan) -> null;
floatify(I) -> float(I).

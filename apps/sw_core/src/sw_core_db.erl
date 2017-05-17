-module(sw_core_db).

-include("sw_core_db.hrl").
-export([load/2]).

-export([create_schema/0]).

%% tag::recordOf[]
record_of('Starship') -> starship.
%% end::recordOf[]

%% tag::load[]
load(Type, ID) ->
    MType = record_of(Type),
    F = fun() ->
                [Obj] = mnesia:read(MType, ID, read),
                Obj
        end,
    txn(F).

%% @doc txn/1 turns a mnesia transaction into a GraphQL friendly return
%% @end
txn(F) ->
    case mnesia:transaction(F) of
        {atomic, Res} -> {ok, Res};
        {aborted, Reason} -> {error, Reason}
    end.
%% end::load[]

%% @doc Create the backend schema for the SW system
%% @end
%% tag::createSchema[]
create_schema() ->
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    ok = create_tables(),
    ok = populate_tables(),
    mnesia:backup("FALLBACK.BUP"),
    mnesia:install_fallback("FALLBACK.BUP"),
    application:stop(mnesia).
%% end::createSchema[]
%% tag::createTables[]
create_tables() ->
    {atomic, ok} =
        mnesia:create_table(
          starship,
          [{disc_copies, [node()]},
           {type, set},
           {attributes, record_info(fields, starship)}]),
    {atomic, ok} =
        mnesia:create_table(
          species,
          [{disc_copies, [node()]},
           {type, set},
           {attributes, record_info(fields, species)}]),
%% end::createTables[]
    ok.

populate_tables() ->
    populate_starships(),
    populate_species().

%% tag::populateStarships[]
populate_starships() ->
    {ok, Data} = file:read_file("fixtures/transport.json"),
    Transports = jsx:decode(Data, [return_maps]),
    populate_starships(Transports).

populate_starships(Ts) ->
    Starships = [json_to_starship(T) || T <- Ts],
    F = fun() ->
                [mnesia:write(SS) || SS <- Starships],
                ok
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
%% end::populateStarships[]

populate_species() ->
    {ok, Data} = file:read_file("fixtures/species.json"),
    Species = jsx:decode(Data, [return_maps]),
    populate_species(Species).

populate_species(Terms) ->
    Species = [json_to_species(T) || T <- Terms],
    F = fun() ->
                [mnesia:write(SS) || SS <- Species],
                ok
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

%% tag::jsonToStarship[]
json_to_starship(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"consumables">> := Consumables,
         <<"name">> := Name,
         <<"created">> := Created,
         <<"cargo_capacity">> := CargoCapacity,
         <<"passengers">> := Passengers,
         <<"max_atmosphering_speed">> := MaxAtmosSpeed,
         <<"crew">> := Crew,
         <<"length">> := Length,
         <<"model">> := Model,
         <<"cost_in_credits">> := Cost,
         <<"manufacturer">> := Manufacturer }}) ->
    #starship {
       id = ID,
       cargo_capacity = CargoCapacity,
       consumables = Consumables,
       cost = Cost,
       created = Created,
       crew = Crew,
       edited = Edited,
       length = Length,
       manufacturers = [Manufacturer],
       max_speed = MaxAtmosSpeed,
       model = Model,
       name = Name,
       passengers = Passengers }.
%% end::jsonToStarship[]

json_to_species(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"classification">> := Classification,
         <<"name">> := Name,
         <<"created">> := Created,
         <<"eye_colors">> := EyeColors,
         <<"people">> := People,
         <<"skin_colors">> := SkinColors,
         <<"language">> := Language,
         <<"hair_colors">> := HairColors,
         <<"homeworld">> := HomeWorld,
         <<"average_lifespan">> := LifeSpan,
         <<"average_height">> := Height }}) ->
    #species {
       id = ID,
       edited = Edited,
       created = Created,
       classification = Classification,
       name = Name,
       eye_colors = commasplit(EyeColors),
       people = People,
       skin_colors = commasplit(SkinColors),
       language = Language,
       hair_colors = commasplit(HairColors),
       homeworld = HomeWorld,
       average_lifespan = integer_like(LifeSpan),
       average_height = integer_like(Height) }.


%% --- INTERNAL HELPERS ------------------------
commasplit(String) ->
    binary:split(String, <<", ">>, [global]).

integer_like(<<"indefinite">>) -> infinity;
integer_like(<<"n/a">>)        -> nan;
integer_like(<<"unknown">>)    -> nan;
integer_like(String)           -> binary_to_integer(String).


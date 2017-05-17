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
    {atomic, ok} =
        mnesia:create_table(
          film,
          [{disc_copies, [node()]},
           {type, set},
           {attributes, record_info(fields, film)}]),
    ok.

%% tag::populatingTables[]
populate(File, Fun) ->
    {ok, Data} = file:read_file(File),
    Terms = jsx:decode(Data, [return_maps]),
    Fun(Terms).

populate_tables() ->
    populate("fixtures/transport.json", fun populate_starships/1),
    populate("fixtures/species.json", fun populate_species/1),
    populate("fixtures/films.json", fun populate_films/1).
%% end::populatingTables

%% tag::populateStarships[]
populate_starships(Ts) ->
    Starships = [json_to_starship(T) || T <- Ts],
    Txn = fun() ->
                [mnesia:write(S) || S <- Starships],
                ok
        end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.
%% end::populateStarships[]

populate_films(Terms) ->
    Films = [json_to_film(T) || T <- Terms],
    Txn = fun() ->
                  [mnesia:write(F) || F <- Films],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.

populate_species(Terms) ->
    Species = [json_to_species(T) || T <- Terms],
    Txn = fun() ->
                [mnesia:write(S) || S <- Species],
                ok
        end,
    {atomic, ok} = mnesia:transaction(Txn),
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

json_to_film(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"created">> := Created,
         <<"vehicles">> := Vehicles,
         <<"planets">> := Planets,
         <<"producer">> := Producer,
         <<"title">> := Title,
         <<"episode_id">> := EpisodeId,
         <<"director">> := Director,
         <<"opening_crawl">> := OpeningCrawl,
         <<"characters">> := Characters
        }}) ->
    #film {
       id = ID,
       edited = Edited,
       created = Created,
       vehicles = Vehicles,
       planets = Planets,
       producer = Producer,
       title = Title,
       episode_id = EpisodeId,
       director = Director,
       opening_crawl = OpeningCrawl,
       characters = Characters
      }.
  
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


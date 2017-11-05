-module(sw_core_db).

-include("sw_core_db.hrl").
-export([load/2, nextval/1]).
-export([wait_for_tables/0]).

-export([create_schema/0, create_fixture/2]).

%% tag::recordOf[]
record_of('Film') -> film;
record_of('Person') -> person;
record_of('Planet') -> planet;
record_of('Species') -> species;
record_of('Starship') -> starship;
record_of('Transport') -> transport;
record_of('Vehicle') -> vehicle;
record_of('Faction') -> faction.
%% end::recordOf[]

tables() ->
    [starship, transport, film, faction,
     species, person, planet, vehicle].

wait_for_tables() ->
    mnesia:wait_for_tables([sequences | tables()], 5000).

load('Vehicle', ID) ->
    F = fun() ->
                [Transport] = mnesia:read(transport, ID, read),
                [Vehicle]  = mnesia:read(vehicle, ID, read),
                #{ vehicle => Vehicle,
                   transport => Transport }
        end,
    txn(F);
%% tag::loadStarship[]
load('Starship', ID) ->
    F = fun() ->
                [Transport] = mnesia:read(transport, ID, read),
                [Starship]  = mnesia:read(starship, ID, read),
                #{ starship => Starship,
                   transport => Transport }
        end,
    txn(F);
%% end::loadStarship[]
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
        {atomic, Res} ->
            {ok, Res};

        {aborted, {{badmatch,[]}, _}} ->
            {error, not_found};

        {aborted, Reason} ->
            {error, Reason}
    end.
%% end::load[]

%% @doc Create the backend schema for the SW system
%% @end
%% tag::createSchema[]
create_schema() ->
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    ok = create_fixture(disc_copies, "fixtures"),
    mnesia:backup("FALLBACK.BUP"),
    mnesia:install_fallback("FALLBACK.BUP"),
    application:stop(mnesia).

create_fixture(Type, BaseDir) ->
    ok = create_tables(Type),
    ok = populate_tables(BaseDir),
    ok.
%% end::createSchema[]

%% tag::createTables[]
create_tables(Type) ->
    {atomic, ok} =
        mnesia:create_table(
          starship,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, starship)}]),
    {atomic, ok} =
        mnesia:create_table(
          species,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, species)}]),
%% end::createTables[]
    {atomic, ok} =
        mnesia:create_table(
          film,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, film)}]),
    {atomic, ok} =
        mnesia:create_table(
          person,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, person)}]),
    {atomic, ok} =
        mnesia:create_table(
          planet,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, planet)}]),
    {atomic, ok} =
        mnesia:create_table(
          transport,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, transport)}]),
    {atomic, ok} =
        mnesia:create_table(
          vehicle,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, vehicle)}]),
    {atomic, ok} =
        mnesia:create_table(
          faction,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, faction)}]),
    {atomic, ok} =
        mnesia:create_table(
          sequences,
          [{Type, [node()]},
           {type, set},
           {attributes, record_info(fields, sequences)}]),
    ok.

%% tag::populatingTables[]
populate(File, Fun) ->
    {ok, Data} = file:read_file(File),
    Terms = jsx:decode(Data, [return_maps]),
    Fun(Terms).

populate_tables(BaseDir) ->
    populate(filename:join(BaseDir, "transport.json"),
             fun populate_transports/1),
    populate(filename:join(BaseDir, "starships.json"),
             fun populate_starships/1),
    populate(filename:join(BaseDir, "species.json"),
             fun populate_species/1),
    populate(filename:join(BaseDir, "films.json"),
             fun populate_films/1),
    populate(filename:join(BaseDir, "people.json"),
             fun populate_people/1),
    populate(filename:join(BaseDir, "planets.json"),
             fun populate_planets/1),
    populate(filename:join(BaseDir, "vehicles.json"),
             fun populate_vehicles/1),
    setup_sequences(),
    ok.
%% end::populatingTables[]

%% tag::populateTransports[]
populate_transports(Ts) ->
    Transports = [json_to_transport(T) || T <- Ts],
    Txn = fun() ->
                [mnesia:write(S) || S <- Transports],
                ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.
%% end::populateTransports[]

setup_sequences() ->
    Counters = [#sequences { key = K, value = 1000 } || K <- tables()],
    Txn = fun() ->
                  [mnesia:write(C) || C <- Counters],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.

nextval(Key) ->
    mnesia:dirty_update_counter(sequences, Key, 1).

populate_starships(Terms) ->
    Starships = [json_to_starship(T) || T <- Terms],
    Txn = fun() ->
                  [mnesia:write(F) || F <- Starships],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.

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

populate_people(Terms) ->
    People = [json_to_person(P) || P <- Terms],
    Txn = fun() ->
                  [mnesia:write(P) || P <- People],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.

%% tag::populate_planets[]
populate_planets(Terms) ->
    Planets = [json_to_planet(P) || P <- Terms],
    Txn = fun() ->
                  [mnesia:write(P) || P <- Planets],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.
%% end::populate_planets[]

populate_vehicles(Terms) ->
    Vehicles = [json_to_vehicle(V) || V <- Terms],
    Txn = fun() ->
                  [mnesia:write(V) || V <- Vehicles],
                  ok
          end,
    {atomic, ok} = mnesia:transaction(Txn),
    ok.

%% tag::jsonToTransport[]
json_to_transport(
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
    #transport {
       id = ID,
       cargo_capacity = number_like(CargoCapacity),
       consumables = Consumables,
       cost = number_like(Cost),
       created = datetime(Created),
       crew = Crew,
       edited = datetime(Edited),
       length = number_like(Length),
       manufacturers = commasplit(Manufacturer),
       max_atmosphering_speed = number_like(MaxAtmosSpeed),
       model = Model,
       name = Name,
       passengers = Passengers }.
%% end::jsonToTransport[]

json_to_starship(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"pilots">> := Pilots,
         <<"MGLT">> := MGLT,
         <<"starship_class">> := Class,
         <<"hyperdrive_rating">> := HyperRating
        }}) ->
    #starship {
       id = ID,
       pilots = Pilots,
       mglt = number_like(MGLT),
       starship_class = Class,
       hyperdrive_rating = number_like(HyperRating)
      }.

json_to_film(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"created">> := Created,
         <<"vehicles">> := Vehicles,
         <<"starships">> := Starships,
         <<"species">> := Species,
         <<"planets">> := Planets,
         <<"producer">> := Producers,
         <<"title">> := Title,
         <<"episode_id">> := EpisodeId,
         <<"director">> := Director,
         <<"opening_crawl">> := OpeningCrawl,
         <<"characters">> := Characters,
         <<"release_date">> := ReleaseDate
        }}) ->
    #film {
       id = ID,
       edited = datetime(Edited),
       created = datetime(Created),
       vehicles = Vehicles,
       planets = Planets,
       starships = Starships,
       species = Species,
       producers = commasplit(Producers),
       title = Title,
       episode = episode(EpisodeId),
       episode_id = number_like(EpisodeId),
       director = Director,
       opening_crawl = OpeningCrawl,
       characters = Characters,
       release_date = datetime(ReleaseDate)
      }.

json_to_species(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"classification">> := Classification,
         <<"name">> := Name,
         <<"created">> := Created,
         <<"designation">> := Designation,
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
       edited = datetime(Edited),
       created = datetime(Created),
       classification = Classification,
       designation = Designation,
       name = Name,
       eye_colors = commasplit(EyeColors),
       people = People,
       skin_colors = commasplit(SkinColors),
       language = Language,
       hair_colors = commasplit(HairColors),
       homeworld = HomeWorld,
       average_lifespan = number_like(LifeSpan),
       average_height = number_like(Height) }.

%% tag::json_to_planet[]
json_to_planet(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"climate">> := Climate,
         <<"surface_water">> := SWater,
         <<"name">> := Name,
         <<"diameter">> := Diameter,
         <<"rotation_period">> := RotationPeriod,
         <<"created">> := Created,
         <<"terrain">> := Terrains,
         <<"gravity">> := Gravity,
         <<"orbital_period">> := OrbPeriod,
         <<"population">> := Population
        }}) ->
    #planet {
       id = ID,
       edited = datetime(Edited),
       climate = Climate,
       surface_water = number_like(SWater),
       name = Name,
       diameter = number_like(Diameter),
       rotation_period = number_like(RotationPeriod),
       created = datetime(Created),
       terrains = commasplit(Terrains),
       gravity = Gravity,
       orbital_period = number_like(OrbPeriod),
       population = number_like(Population)
}.
%% end::json_to_planet[]

json_to_person(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"edited">> := Edited,
         <<"name">> := Name,
         <<"created">> := Created,
         <<"gender">> := Gender,
         <<"skin_color">> := SkinColor,
         <<"hair_color">> := HairColor,
         <<"height">> := Height,
         <<"eye_color">> := EyeColor,
         <<"mass">> := Mass,
         <<"homeworld">> := HomeWorld,
         <<"birth_year">> := BirthYear
        }}) ->
    #person {
       id = ID,
       edited = datetime(Edited),
       name = Name,
       created = datetime(Created),
       gender = Gender,
       skin_color = SkinColor,
       hair_color = HairColor,
       height = number_like(Height),
       eye_color = EyeColor,
       mass = number_like(Mass),
       homeworld = HomeWorld,
       birth_year = BirthYear
      }.

json_to_vehicle(
  #{ <<"pk">> := ID,
     <<"fields">> := #{
         <<"vehicle_class">> := Class,
         <<"pilots">> := Pilots
        }}) ->
    #vehicle {
       id = ID,
       vehicle_class = Class,
       pilots = Pilots
      }.

%% --- INTERNAL HELPERS ------------------------
commasplit(String) ->
    binary:split(String, <<", ">>, [global]).

number_like(<<"indefinite">>)              -> infinity;
number_like(<<"none">>)                    -> nan;
number_like(<<"n/a">>)                     -> nan;
number_like(<<"unknown">>)                 -> nan;
number_like(F) when is_float(F)            -> F;
number_like(I) when is_integer(I)          -> I;
number_like(String) when is_binary(String) ->
    case binary:match(String, <<".">>) of
        nomatch ->
            binary_to_integer(binary:replace(String, <<",">>, <<>>));
        _Found ->
            binary_to_float(String)
    end.

datetime(DT) ->
    {ok, Val} = sw_core_scalar:input(<<"DateTime">>, DT),
    Val.

%% Compute the episode for injection
episode(1) -> 'PHANTOM';
episode(2) -> 'CLONES';
episode(3) -> 'SITH';
episode(4) -> 'NEWHOPE';
episode(5) -> 'EMPIRE';
episode(6) -> 'JEDI'.
    
    

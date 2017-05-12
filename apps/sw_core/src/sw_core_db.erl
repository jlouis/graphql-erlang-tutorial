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
%% end::createTables[]
    ok.

populate_tables() ->
    populate_starships().

%% tag::populateStarships[]
populate_starships() ->
    {ok, Data} = file:read_file("fixtures/transports.json"),
    {ok, Transports} = jsx:decode(Data, [return_maps]),
    populate_starships(Transports).

populate_starships(Ts) ->
    Starships = [json_to_starship(T) || T <- Ts],
    F = fun() ->
                mnesia:insert(Starships),
                ok
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
%% end::populateStarships[]

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

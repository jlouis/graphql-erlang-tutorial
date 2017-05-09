-module(sw_core_db).

-include("sw_core_db.hrl").
-export([create_schema/0]).


%% @doc Create the backend schema for the SW system
%% @end
%% tag::createSchema[]
create_schema() ->
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    ok = create_tables(),
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


-module(sw_core_db).

-include("sw_core_db.hrl").
-export([create_schema/0]).


%% @doc Create the backend schema for the SW system
%% @end
create_schema() ->
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    ok = create_tables(),
    mnesia:backup("b.log"),
    mnesia:install_fallback("b.log"),
    application:stop(mnesia).

create_tables() ->
    {atomic, ok} =
        mnesia:create_table(
          ship,
          [{disc_copies, [node()]},
           {type, set},
           {attributes, record_info(fields, ship)}]),
    ok.

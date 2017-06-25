-module(sw_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(graphql),
    ok = init_mnesia(Config),
    {ok, _} = application:ensure_all_started(sw_core),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    [{setup, [], [live]}].

all() -> [
          {group, setup}
         ].

%% -- Mnesia initialization ----------------------
init_mnesia(Config) ->
    {ok, _} = application:ensure_all_started(mnesia),
    FixtureDir = ?config(data_dir, Config),
    ok = sw_core_db:create_fixture(ram_copies, FixtureDir),
    ok.

%% -- BASIC --------------------------------------
live(Config) ->
    Running = [element(1, Runners)
               || Runners <- proplists:get_value(running, application:info())],
    true = lists:member(sw_core, Running),
    ok.

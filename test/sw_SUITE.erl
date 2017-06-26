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
    [{setup, [], [live]},
     {tour_queries, [], [system_tour_queries]}].

all() -> [
          {group, setup},
          {group, tour_queries}
         ].

%% -- Mnesia initialization ----------------------
init_mnesia(Config) ->
    {ok, _} = application:ensure_all_started(mnesia),
    FixtureDir = ?config(data_dir, Config),
    ok = sw_core_db:create_fixture(ram_copies, FixtureDir),
    ok.

%% -- SETUP --------------------------------------
live(Config) ->
    Running = [element(1, Runners)
               || Runners <- proplists:get_value(running, application:info())],
    true = lists:member(sw_core, Running),
    ok.

%% -- TOUR ---------------------------------------
system_tour_queries(Config) ->
    ok = run_query(Config, "first"),
    ok = run_query(Config, "advanced"),
    ok = run_query(Config, "mutation", <<"IntroduceFaction">>),
    ok = run_query(Config, "faction"),
    ok.

%% -- INTERNALS ----------------------------------
run_query(Config, Name) ->
    run_query(Config, Name, undefined).

run_query(Config, Name, OpName) ->
    DataDir = ?config(data_dir, Config),
    Query = filename:join(DataDir, Name ++ ".query"),
    Result = filename:join(DataDir, Name ++ ".result"),
    Vars = input(Config, Name),

    {ok, QueryDoc} = file:read_file(Query),
    {ok, ExpectedJson} = file:read_file(Result),
    Expected = canonicalize_json(ExpectedJson),

    {ok, AST} = graphql:parse(QueryDoc),
    Elaborated = graphql:elaborate(AST),
    {ok, #{ fun_env := FunEnv, ast := AST2}} =
        graphql:type_check(Elaborated),
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
    Ctx = #{
      params => Coerced,
      operation_name => OpName
     },
    Response = graphql:execute(Ctx, AST2),
    Expected = jsx:encode(Response),
    ok.

input(Config, Name) -> 
    DataDir = ?config(data_dir, Config),
    case file:read_file(
           filename:join(DataDir, Name ++ ".input")) of
        {error, enoent} ->
            #{};
        {ok, InputData} ->
            jsx:decode(InputData, [return_maps])
    end.

canonicalize_json(Input) ->
    jsx:encode(jsx:decode(Input)).

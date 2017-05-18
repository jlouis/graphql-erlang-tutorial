%%%-------------------------------------------------------------------
%% @doc sw_core public API
%% @end
%%%-------------------------------------------------------------------

-module(sw_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = sw_core_sup:start_link(),
    ok = load_schema(),
    ok = sw_core_db:wait_for_tables(),
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%% tag::schemaMapping[]
mapping_rules() ->
    #{
       scalars => #{ default => sw_core_scalar },
       interfaces => #{ default => sw_core_type },
       unions => #{ default => sw_core_type },
       objects => #{
         'Planet' => sw_core_planet,
         'Starship' => sw_core_starship,
         'Query' => sw_core_query,
         'Mutation' => sw_core_mutation,
         default => sw_core_object }
     }.
%% end::schemaMapping[]

%% tag::loadSchema[]
load_schema() ->
    {ok, SchemaFile} = application:get_env(sw_core, schema_file),
    PrivDir = code:priv_dir(sw_core),
    {ok, SchemaData} = file:read_file(
                         filename:join(PrivDir, SchemaFile)),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.
%% end::loadSchema[]

%% tag::setupRoot[]
setup_root() ->
    Root = {root,
            #{ query => 'Query',
               mutation => 'Mutation',
               interfaces => ['Node']
             }},
    ok = graphql:insert_schema_definition(Root),
    ok.
%% end::setupRoot[]
      

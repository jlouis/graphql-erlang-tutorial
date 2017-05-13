%% tag::exports[]
-module(sw_web_graphql_handler).

%% Cowboy Handler Interface
-export([init/3]).

%% REST callbacks
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2
]).

%% Data input/output callbacks
-export([
    from_graphql/2,
    from_json/2,
    to_json/2,
    to_html/2
]).
%% end::exports[]

%% -- API ---------------------------------------------------
%% tag::init[]
init(_Transport, _Req, _Options) ->
    {upgrade, protocol, cowboy_rest}.
%% end::init[]

%% tag::rest_init[]
rest_init(Req, {priv_file, _, _} = PrivFile) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req2,
     #{ method => Method,
        priv_file => PrivFile }}.
%% end::rest_init[]

%% tag::allowed_methods[]
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.
%% end::allowed_methods[]

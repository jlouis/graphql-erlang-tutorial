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

%% tag::content_types_accepted[]
content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.
%% end:content_types_accepted[]

%% tag::content_types_provided[]
content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json},
        {{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.
%% end::content_types_provided[]

%% tag::charsets_provided[]
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
%% end::charsets_provided[]

%% tag::resource_exists[]
resource_exists(Req, #{ method := <<"GET">> } = State) ->
    {true, Req, State};
resource_exists(Req, #{ method := <<"POST">> } = State) ->
    {false, Req, State}.
%% end::resource_exists[]

%% tag::to_html[]
to_html(Req, #{ index_location :=
                    {priv_file, App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    {ok, Data} = file:read_file(Filename),
    {Data, Req, State}.
%% end::to_html[]

gather(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Bindings, Req3} = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    try jsx:decode(Body, [return_maps]) of
        JSON ->
            gather(Req3, State, JSON, Params)
    catch
        error:badarg ->
            {error, invalid_json_body}
    end.

gather(Req, State, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, QueryDocument, Vars, Opations};
        {error, Reason} ->
            {error, Reason}
    end.

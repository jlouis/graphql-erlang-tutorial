-module(sw_core_paginate).

-export([select/2]).

-define(DEFAULT_FIRST, 5).

select(Elements, Args) ->
    try
        {ok, select_(Elements, Args)}
    catch
        throw:Err ->
            {error, Err}
    end.

%% tag::paginate[]
select_(Elements,
         #{ <<"first">> := F,
            <<"last">> := L,
            <<"after">> := After,
            <<"before">> := Before }) ->
    {First, Last} = defaults(F, L), % <1>
    Count = length(Elements), % <2>

    %% applyCursorsToEdges <3>
    Positions = lists:seq(1, Count),
    Sliced = apply_cursors_to_edges(After, Before,
                                    lists:zip(Elements, Positions)),
    Window = edges_to_return(First, Last, Sliced), % <4>
    Edges = format(Window), 

    %% Build PageInfo <5>
    PageInfo = #{
      <<"hasNextPage">> => has_next(Sliced, First),
      <<"hasPreviousPage">> => has_previous(Sliced, Last)
     },

    %% Return result <6>
    #{
       <<"totalCount">> => Count,
       <<"edges">> => Edges,
       <<"pageInfo">> => PageInfo
     }.
%% end::paginate[]

defaults(null, null) -> {?DEFAULT_FIRST, null};
defaults(F, L) -> {F, L}.

%% tag::pageInfo[]
has_previous(_Sliced, null) -> false;
has_previous(Sliced, Last) -> length(Sliced) > Last.
    
has_next(_Sliced, null) -> false;
has_next(Sliced, First) -> length(Sliced) > First.
%% end::pageInfo[]
    
format([]) -> [];
format([{Elem, Pos}|Xs]) ->
    X = #{ <<"node">> => Elem,
           <<"cursor">> => pack_cursor(Pos)},
    [{ok, X} | format(Xs)].

%% tag::edgesToReturn[]
edges_to_return(First, null, Window) ->
    Sz = length(Window),
    case Sz - First of
        K when K =< 0 -> Window;
        K when K > 0 ->
            {Res, _} = lists:split(First, Window),
            Res
    end;
edges_to_return(null, Last, Window) ->
    lists:reverse(
      edges_to_return(Last, null, lists:reverse(Window))).
%% end::edgesToReturn[]

%% tag::applyCursorsToEdges[]
apply_cursors_to_edges(null, null, Elements) ->
    Elements;
apply_cursors_to_edges(null, Before, Elements) ->
    Pos = unpack_cursor(Before),
    {Res,_} = lists:split(Pos, Elements),
    apply_cursors_to_edges(null, null, Res);
apply_cursors_to_edges(After, Before, Elements) ->
    Pos = unpack_cursor(After),
    {_, Res} = lists:split(Pos, Elements),
    apply_cursors_to_edges(null, Before, Res).
%% end::applyCursorsToEdges[]

%% tag::packCursor[]
pack_cursor(Pos) ->
    base64:encode(integer_to_binary(Pos)).
%% end::packCursor[]

unpack_cursor(Cursor) ->
  try
      P = base64:decode(Cursor),
      binary_to_integer(P)
  catch
      _:_ ->
          throw(bad_cursor_decode)
  end.

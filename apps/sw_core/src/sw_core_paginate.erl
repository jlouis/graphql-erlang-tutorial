-module(sw_core_paginate).

-export([select/2]).

-define(DEFAULT_FIRST, 5).

select(Elements, Window) ->
    try
        {ok, select_(Elements, Window)}
    catch
        throw:Err ->
            {error, Err}
    end.

defaults(null, null) -> {?DEFAULT_FIRST, null};
defaults(F, L) -> {F, L}.

select_(Elements,
         #{ <<"first">> := F,
            <<"last">> := L,
            <<"after">> := After,
            <<"before">> := Before }) ->
    {First, Last} = defaults(F, L),
    Count = length(Elements),
    Positions = lists:seq(1, Count),
    Sliced = apply_cursors_to_edges(After, Before, lists:zip(Elements, Positions)),
    Window = edges_to_return(First, Last, Sliced),
    Edges = format(Window),

    PageInfo = #{
      <<"hasNextPage">> => has_next(Sliced, First),
      <<"hasPreviousPage">> => has_previous(Sliced, Last)
     },
    #{
       <<"totalCount">> => Count,
       <<"edges">> => Edges,
       <<"pageInfo">> => PageInfo
     }.

has_previous(_Sliced, null) -> false;
has_previous(Sliced, Last) -> length(Sliced) > Last.
    
has_next(_Sliced, null) -> false;
has_next(Sliced, First) -> length(Sliced) > First.
    
format([]) -> [];
format([{Elem, Pos}|Xs]) ->
    X = #{ <<"node">> => Elem,
           <<"cursor">> => pack_cursor(Pos)},
    [{ok, X} | format(Xs)].
    
edges_to_return(First, null, Window) ->
    Sz = length(Window),
    case Sz - First of
        0 -> Window;
        K when K < 0 -> Window;
        K when K > 0 -> lists:split(First, Window)
    end;
edges_to_return(null, Last, Window) ->
    %% Simple "conjugate"
    lists:reverse(
      edges_to_return(Last, null, lists:reverse(Window))).

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

pack_cursor(Pos) ->
    base64:encode(integer_to_binary(Pos)).

unpack_cursor(Cursor) ->
  try
      P = base64:decode(Cursor),
      binary_to_integer(P)
  catch
      _:_ ->
          throw(bad_cursor_decode)
  end.

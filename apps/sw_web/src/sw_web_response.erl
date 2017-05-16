-module(sw_web_response).
-export([term_to_json/1]).

term_to_json(Term) ->
    jsx:encode(fixup(Term)).

%% Ground types
fixup(Term) when is_number(Term) -> Term;
fixup(Term) when is_atom(Term) -> Term;
fixup(Term) when is_binary(Term) -> Term;
%% Compound types
fixup(Term) when is_list(Term) ->
    [fixup(T) || T <- Term];
fixup(Term) when is_map(Term) ->
    maps:map(fun(_K, V) ->
                     fixup(V)
             end, Term);
fixup(Term) ->
    %% Every other term is transformed into a binary value
    iolist_to_binary(
      io_lib:format("~p", [Term])).


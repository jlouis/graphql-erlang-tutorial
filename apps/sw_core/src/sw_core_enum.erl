%% tag::coreEnum[]
-module(sw_core_enum).

-export([input/2, output/2]).

input(<<"Episode">>, <<"PHANTOM">>) -> {ok, 'PHANTOM'};
input(<<"Episode">>, <<"CLONES">>)  -> {ok, 'CLONES'};
input(<<"Episode">>, <<"SITH">>)    -> {ok, 'SITH'};
input(<<"Episode">>, <<"NEWHOPE">>) -> {ok, 'NEWHOPE'};
input(<<"Episode">>, <<"EMPIRE">>)  -> {ok, 'EMPIRE'};
input(<<"Episode">>, <<"JEDI">>)    -> {ok, 'JEDI'}.

output(<<"Episode">>, Episode) ->
    {ok, atom_to_binary(Episode, utf8)}.
%% end::coreEnum[]

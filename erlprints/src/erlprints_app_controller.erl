-module(erlprints_app_controller).
-export([hook/1]).

hook(A) ->
    {ewc, A}.



-module(epostmark).

-export([send_email/1]).
-export([send_email/2]).

authentication_header() ->
    {ok, PK} = application:get_env(epostmark, postmark_key),
    {<<"X-Postmark-Server-Token">>, PK}.

send_email(Json) ->
    send_email(Json, []).

send_email(Json, Headers) ->
    Response = lhttpc:request(
        "https://api.postmarkapp.com/email",
        "POST",
        [
            authentication_header(),
            {<<"Content-Type">>, <<"application/json">>},
            {<<"Accept">>, <<"application/json">>}
        ] ++ Headers,
        Json,
        5000
    ),
    handle_response(Response).

handle_response({ok, {{200, _}, _, JsonResponse}}) ->
    {ok, jiffy:decode(JsonResponse)};

handle_response({ok, {{422, "Unprocessable Entity"}, _, JsonResponse}}) ->
    Jterm = jiffy:decode(JsonResponse),
    case get_json_value([<<"ErrorCode">>], Jterm) of
        300 ->
            {error, {illegal_to_address, JsonResponse}};
        _ ->
            {error, JsonResponse}
    end.

get_json_value(_, undefined) ->
    undefined;
get_json_value([], Obj) ->
    Obj;
get_json_value([Key | Rest], {Obj}) ->
    get_json_value(Rest, proplists:get_value(Key, Obj));
get_json_value([Index | _Rest], []) when is_integer(Index) ->
    undefined;
get_json_value([Index | Rest], Obj) when is_integer(Index), is_list(Obj) ->
    get_json_value(Rest, lists:nth(Index + 1, Obj));
get_json_value([Key | Rest], Obj) when is_list(Obj) ->
    get_json_value(Rest, proplists:get_all_values(Key, Obj)).

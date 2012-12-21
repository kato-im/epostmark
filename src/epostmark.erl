-module(epostmark).

-export([send_email/1]).

-record(postmark_success, {obj}).

authentication_header() ->
    {ok, PK} = application:get_env(epostmark, postmark_key),
    {<<"X-Postmark-Server-Token">>, PK}.

send_email(Json) ->
    Res = lhttpc:request(
        "https://api.postmarkapp.com/email",
        "POST",
        [
            authentication_header(),
            {<<"Content-Type">>, <<"application/json">>},
            {<<"Accept">>, <<"application/json">>}
        ],
        Json,
        5000
    ),
    {ok, {{200, _}, _, JsonResponse}} = Res,
    {ok, #postmark_success{obj = jiffy:decode(JsonResponse)}}.

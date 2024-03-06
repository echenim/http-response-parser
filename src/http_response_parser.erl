-module(http_response_parser).

-export([http_parser/1, bench/0, test/0]).

%% Constants for Test Responses
-define(RESP1,
    <<"HTTP/1.1 200 OK\r\nServer: AdGear\r\nContent-Length: 12\r\nDate: Wed, 21 Dec 2016 18:29:13 GMT\r\nConnection: close\r\n\r\nhello world!">>
).
-define(RESP2,
    <<"HTTP/1.1 204 No Content\r\nServer: AdGear\r\nDate: Wed, 15 Feb 2017 01:47:43 GMT\r\nContent-Length: 0\r\nConnection: Keep-Alive\r\n\r\n">>
).
-define(RESP3,
    <<"HTTP/1.1 500 Internal Server Error\r\nServer: AdGear\r\nDate: Fri, 03 Jun 2016 14:34:26 GMT\r\nContent-Length: 0\r\n\r\n">>
).

-type status() :: pos_integer().
-type reason() :: binary().
-type headers() :: [{binary(), binary()}].
-type body() :: binary().

%% Public API
-spec http_parser(binary()) -> {status(), reason(), headers(), body()}.
http_parser(_Bin) ->
    {StatusLine, HeadersBodyBin} = split_status_line(_Bin),
    {Status, Reason} = parse_status_line(StatusLine),
    {HeadersBin, Body} = split_headers_body(HeadersBodyBin),
    Headers = parse_headers(HeadersBin),
    {Status, Reason, Headers, Body}.

bench() ->
    N = 10000,
    Results = [timer:tc(fun() -> http_parser(?RESP1) end) || _ <- lists:seq(1, N)],
    Average = lists:foldr(fun({X, _}, Acc) -> Acc + X end, 0, Results) / N,
    io:format("Average parsing time: ~p us~n", [Average]).

test() ->
    {200, <<"OK">>,
        [
            {<<"connection">>, <<"close">>},
            {<<"date">>, <<"Wed, 21 Dec 2016 18:29:13 GMT">>},
            {<<"content-length">>, <<"12">>},
            {<<"server">>, <<"AdGear">>}
        ],
        <<"hello world!">>} = http_parser(?RESP1),
    {204, <<"No Content">>,
        [
            {<<"connection">>, <<"Keep-Alive">>},
            {<<"content-length">>, <<"0">>},
            {<<"date">>, <<"Wed, 15 Feb 2017 01:47:43 GMT">>},
            {<<"server">>, <<"AdGear">>}
        ],
        <<>>} = http_parser(?RESP2),
    {500, <<"Internal Server Error">>,
        [
            {<<"content-length">>, <<"0">>},
            {<<"date">>, <<"Fri, 03 Jun 2016 14:34:26 GMT">>},
            {<<"server">>, <<"AdGear">>}
        ],
        <<>>} = http_parser(?RESP3),
    ok.

%% Internal Helper Functions

split_status_line(Bin) ->
    [StatusLine, Rest] = binary:split(Bin, <<"\r\n\r\n">>, [global]),
    {StatusLine, Rest}.

parse_status_line(StatusLine) ->
    [_, StatusCodeBin, ReasonBin | _] = binary:split(StatusLine, <<" ">>, [global]),
    {binary_to_integer(StatusCodeBin), ReasonBin}.

split_headers_body(Bin) ->
    Parts = binary:split(Bin, <<"\r\n\r\n">>, [global]),
    case Parts of
        [HeadersBin, Body] ->
            % Normal case, headers and body are found
            {HeadersBin, Body};
        [HeadersBin] ->
            % Only headers are found, no body
            {HeadersBin, <<>>};
        _ ->
            % Fallback scenario, treat everything as body
            {<<>>, Bin}
    end.

parse_headers(HeadersBin) ->
    HeadersLines = binary:split(HeadersBin, <<"\r\n">>, [global]),
    lists:map(
        fun(HeaderLine) ->
            [Key, Value] = binary:split(HeaderLine, <<": ">>, [global]),
            {string:to_lower(binary_to_list(Key)), Value}
        end,
        HeadersLines
    ).

%% Possibly more helper functions below for detailed parsing tasks

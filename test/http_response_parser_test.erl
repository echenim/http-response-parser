-module(http_response_parser_test).
-include_lib("eunit/include/eunit.hrl").

%% Test cases for http_parser/1
http_parser_test_() ->
    [
        % {"Test parsing HTTP 200 response", fun() ->
        %     Expected =
        %         {200, <<"OK">>,
        %             [
        %                 {<<"connection">>, <<"close">>},
        %                 {<<"date">>, <<"Wed, 21 Dec 2016 18:29:13 GMT">>},
        %                 {<<"content-length">>, <<"12">>},
        %                 {<<"server">>, <<"AdGear">>}
        %             ],
        %             <<"hello world!">>},
        %     ?assertEqual(Expected, http_response_parser:http_parser(?RESP1))
        % end},

        % {"Test parsing HTTP 204 response", fun() ->
        %     Expected =
        %         {204, <<"No Content">>,
        %             [
        %                 {<<"connection">>, <<"Keep-Alive">>},
        %                 {<<"content-length">>, <<"0">>},
        %                 {<<"date">>, <<"Wed, 15 Feb 2017 01:47:43 GMT">>},
        %                 {<<"server">>, <<"AdGear">>}
        %             ],
        %             <<>>},
        %     ?assertEqual(Expected, http_response_parser:http_parser(?RESP2))
        % end},

        % {"Test parsing HTTP 500 response", fun() ->
        %     Expected =
        %         {500, <<"Internal Server Error">>,
        %             [
        %                 {<<"content-length">>, <<"0">>},
        %                 {<<"date">>, <<"Fri, 03 Jun 2016 14:34:26 GMT">>},
        %                 {<<"server">>, <<"AdGear">>}
        %             ],
        %             <<>>},
        %     ?assertEqual(Expected, http_response_parser:http_parser(?RESP3))
        % end}
    ].

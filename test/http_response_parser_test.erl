-module(http_response_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_status_line_success_test_() ->
    [
        % Test case for a valid status line with a typical 200 OK response.
        ?_assertEqual(
            {200, <<"OK">>},
            http_response_parser:parse_status_line(<<"HTTP/1.1 200 OK">>)
        ),
        % Test case for a different status code and reason phrase.
        ?_assertEqual(
            {404, <<"Not Found">>},
            http_response_parser:parse_status_line(<<"HTTP/1.1 404 Not Found">>)
        )
    ].

parse_status_line_invalid_http_version_test_() ->
    % Test case for an invalid HTTP version.
    ?_assertEqual(
        {error, <<"Invalid HTTP version or status code">>},
        http_response_parser:parse_status_line(<<"HTTP/1.0 200 OK">>)
    ).

parse_status_line_status_code_out_of_range_test_() ->
    [
        % Test case for a status code below the valid range.
        ?_assertEqual(
            {error, <<"Invalid HTTP version or status code">>},
            http_response_parser:parse_status_line(<<"HTTP/1.1 99 Some Error">>)
        ),
        % Test case for a status code above the valid range.
        ?_assertEqual(
            {error, <<"Invalid HTTP version or status code">>},
            http_response_parser:parse_status_line(<<"HTTP/1.1 600 Another Error">>)
        )
    ].

parse_status_line_malformed_test_() ->
    % Test case for a malformed status line.
    ?_assertEqual(
        {error, <<"Malformed status line">>},
        http_response_parser:parse_status_line(<<"HTTP/1.1">>)
    ).

parse_headers_test_() ->
    [
        %% Test parsing of a single, well-formed header.
        ?_assertEqual(
            [{"content-type", <<"text/html">>}],
            http_response_parser:parse_headers(<<"Content-Type: text/html\r\n">>)
        ),

        %% Test parsing multiple well-formed headers.
        ?_assertEqual(
            [{"server", <<"AdGear">>}, {"content-length", <<"123">>}],
            http_response_parser:parse_headers(<<"Server: AdGear\r\nContent-Length: 123\r\n">>)
        ),

        %% Test parsing headers with extra spaces around the colon.
        ?_assertEqual(
            [{"connection", <<"keep-alive">>}],
            http_response_parser:parse_headers(<<"Connection : keep-alive\r\n">>)
        ),

        %% Test that headers without a value are excluded.
        ?_assertEqual(
            [],
            http_response_parser:parse_headers(<<"X-Custom-Header:\r\n">>)
        ),

        %% Test parsing headers with only key and no value.
        ?_assertEqual(
            [],
            http_response_parser:parse_headers(<<"X-Empty-Header: \r\n">>)
        ),

        %% Test that malformed headers (no colon) are excluded.
        ?_assertEqual(
            [],
            http_response_parser:parse_headers(<<"MalformedHeader\r\n">>)
        ),

        %% Test handling of mixed valid and invalid headers.
        ?_assertEqual(
            [{"user-agent", <<"curl/7.58.0">>}],
            http_response_parser:parse_headers(
                <<"User-Agent: curl/7.58.0\r\nInvalidHeader\r\nAnother: Valid\r\n">>
            )
        )
    ].

-module(http_response_parser).
-export([http_parser/1, bench/0, test/0]).
-ifdef(TEST).
-export([parse_headers/1, parse_status_line/1]).
-endif.

%% Constants defined for test responses to simulate different HTTP response scenarios.
%% These binaries include status lines, headers, and bodies formatted according to the HTTP protocol.
-define(RESP1,
    <<"HTTP/1.1 200 OK \r\nServer: AdGear\r\nContent-Length: 12\r\nDate: Wed, 21 Dec 2016 18:29:13 GMT\r\nConnection: close\r\n\r\nhello world 1!">>
).
-define(RESP2,
    <<"HTTP/1.1 204 No-Content \r\nServer: AdGear\r\nDate: Wed, 15 Feb 2017 01:47:43 GMT\r\nContent-Length: 0\r\nConnection: Keep-Alive\r\n\r\n">>
).
-define(RESP3,
    <<"HTTP/1.1 500 Internal-Server-Error \r\nServer: AdGear\r\nDate: Fri, 03 Jun 2016 14:34:26 GMT\r\nContent-Length: 0\r\n\r\n">>
).

%% Type specifications for clarity and error checking.
-type status() :: pos_integer().
-type reason() :: ok | no_content.
-type headers() :: [{binary(), binary()}].
-type body() :: binary().
-type http_response() :: {status(), reason(), headers(), body()} | {error, binary()}.

%% Public API

%% http_parser/1 takes a binary HTTP response and parses it into structured data.
%% This function leverages Erlang's pattern matching and binary processing capabilities
%% for efficient parsing of HTTP response components.
-spec http_parser(binary()) -> http_response().

%% @spec http_parser(Binary :: binary()) -> {status(), reason(), headers(), body()} | {error, binary()}.
%% @desc Parses a binary HTTP response into a structured format.
%%       The function handles parsing in a sequential manner, dealing with each part of the HTTP response.
%%
%%       First, it attempts to split the input binary into the headers (including the status line) and the body.
%%       If the input does not follow the expected format, indicating a malformed HTTP response,
%%       it returns an error with a descriptive reason.
%%
%%       If the initial split is successful, it proceeds to parse the status line to extract the status code and reason phrase.
%%       Any error in parsing the status line results in an immediate return of an error tuple.
%%
%%       Assuming the status line is successfully parsed, the function then attempts to parse the headers section
%%       into a list of key-value pairs. If the headers cannot be correctly parsed due to formatting issues,
%%       an error is returned.
%%
%%       Upon successful parsing of both the status line and headers, the function assembles and returns a tuple
%%       containing the status code, reason phrase, headers list, and the body binary.
%%
%%       This approach ensures that parsing errors at any stage are caught and reported, making debugging easier.
%%       It also cleanly separates the parsing logic for different parts of the HTTP response, enhancing readability and maintainability.
http_parser(Binary) ->
    %% Attempt to split the input binary into headers and body.
    case split_status_and_body(Binary) of
        {error, Reason} ->
            %% Return an error tuple if the input is malformed and cannot be split.
            {error, Reason};
        {HeadersBinary, BodyBinary} ->
            %% Attempt to parse the status line from the headers binary.
            case parse_status_line(HeadersBinary) of
                {error, Reason} ->
                    %% Return an error tuple if the status line is malformed.
                    {error, Reason};
                {Status, Reason} ->
                    %% Attempt to parse the headers if the status line is successfully parsed.
                    case parse_headers(HeadersBinary) of
                        %% Return an error if headers are malformed.
                        {error, Reason} ->
                            {error, Reason};
                        Headers ->
                            %% Return the structured HTTP response if all parsing steps succeed.
                            {Status, Reason, Headers, BodyBinary}
                    end
            end
    end.

%% Benchmarks the http_parser function by running it a large number of times (NumIterations)
%% and calculating the average execution time. This function helps in performance tuning and optimization.
bench() ->
    NumIterations = 10000,
    %% Measure execution time for parsing a predefined response.
    TimingResults = [
        timer:tc(fun() -> http_parser(?RESP1) end)
     || _ <- lists:seq(1, NumIterations)
    ],
    %% Calculate the average parsing time.
    TotalTime = lists:foldr(fun({Time, _}, Accum) -> Accum + Time end, 0, TimingResults),
    AverageTime = TotalTime / NumIterations,
    io:format("Average parsing time: ~p us~n", [AverageTime]).

%% Test function to validate the parser with predefined responses and expected outcomes.
%% This ensures the parser's correctness across different types of HTTP responses.
test() ->
    %% Tests are structured to compare the parser's output directly against expected results.
    %% This straightforward approach facilitates easy understanding and maintenance of test cases.
    %% Each predefined response is parsed, and its output is compared to the expected tuple structure.
    %% Asserting true = Condition for each test verifies the parser's accuracy.
    ExpectedResult1 =
        {200, <<"OK">>,
            [
                {"server", <<"AdGear">>},
                {"content-length", <<"12">>},
                {"date", <<"Wed, 21 Dec 2016 18:29:13 GMT">>},
                {"connection", <<"close">>}
            ],
            <<"hello world 1!">>},
    ExpectedResult2 =
        {204, <<"No-Content">>,
            [
                {"server", <<"AdGear">>},
                {"date", <<"Wed, 15 Feb 2017 01:47:43 GMT">>},
                {"content-length", <<"0">>},
                {"connection", <<"Keep-Alive">>}
            ],
            <<>>},
    ExpectedResult3 =
        {500, <<"Internal-Server-Error">>,
            [
                {"server", <<"AdGear">>},
                {"date", <<"Fri, 03 Jun 2016 14:34:26 GMT">>},
                {"content-length", <<"0">>}
            ],
            <<>>},
    true = http_parser(?RESP1) == ExpectedResult1,
    true = http_parser(?RESP2) == ExpectedResult2,
    true = http_parser(?RESP3) == ExpectedResult3,
    ok.

%% Internal Helper Functions

%% Splits the input binary into headers and body parts based on the HTTP protocol's structure.
split_status_and_body(Binary) ->
    case binary:split(Binary, <<"\r\n\r\n">>, [global]) of
        % Successfully found delimiter, return headers and body
        [Headers, Body] -> {Headers, Body};
        % Delimiter not found, return error
        _ -> {error, <<"Malformed HTTP response: Missing header-body delimiter">>}
    end.

%% @doc Parses the status line of an HTTP response.
%%
%% The status line is expected to contain the HTTP version, status code, and reason phrase,
%% separated by spaces. This function attempts to extract these components and validate them.
%%
%% @param StatusLine A binary string containing the status line to be parsed.
%% @return A tuple of `{StatusCode, ReasonBinary}` if the status line is correctly formatted and contains
%% a valid HTTP version and status code. The `StatusCode` is an integer, and `ReasonBinary` is the binary
%% reason phrase.
%% If the status line is malformed or contains invalid data, an error tuple is returned, indicating
%% the nature of the error: either `{error, <<"Malformed status line">>}` for a general format error,
%% or `{error, <<"Invalid HTTP version or status code">>}` if the version or status code is not recognized.
parse_status_line(StatusLine) ->
    case binary:split(StatusLine, <<" ">>, [global]) of
        [HttpVersion, StatusCodeBinary, ReasonBinary | _] ->
            % Validate the HTTP version and status code range.
            case {HttpVersion, binary_to_integer(StatusCodeBinary)} of
                {<<"HTTP/1.1">>, StatusCode} when StatusCode >= 100, StatusCode =< 599 ->
                    % Return the status code and reason phrase if valid.
                    {StatusCode, ReasonBinary};
                _ ->
                    % Return an error if the HTTP version is not "HTTP/1.1" or the status code is out of range.
                    {error, <<"Invalid HTTP version or status code">>}
            end;
        _ ->
            % Return an error if the status line does not match the expected format.
            {error, <<"Malformed status line">>}
    end.

parse_headers(HeadersBinary) ->
    %% The function `parse_headers` takes a binary representation of the HTTP headers
    %% and splits it into individual header lines. It then processes each line to extract
    %% the header name and value, converting the header name to lowercase for normalization.
    %% Headers that do not conform to the expected "Key: Value" format are excluded from the
    %% result, ensuring that only valid headers are returned. The function makes use of the
    %% `lists:filtermap/2` to efficiently filter out invalid headers and map over the remaining
    %% ones to transform them into a list of {Key, Value} tuples, where both Key and Value are binaries.
    HeadersLines = binary:split(HeadersBinary, <<"\r\n">>, [global]),
    lists:filtermap(
        fun(HeaderLine) ->
            case binary:split(HeaderLine, <<": ">>, [global]) of
                [Key, Value] when Value =/= <<>> ->
                    {true, {string:to_lower(binary_to_list(Key)), Value}};
                _ ->
                    false
            end
        end,
        HeadersLines
    ).

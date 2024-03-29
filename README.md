# HTTP 1.1 Response Parser 

## Overview

The `http_response_parser` module transforms binary strings of HTTP responses into structured Erlang terms. This document details the module's design, including its public interfaces, internal mechanics, and typing definitions, providing clarity on its operational framework and usability.

## Table of Contents

1. [Public APIs](#public-apis)
2. [Type Specifications](#type-specifications)
3. [Detailed Function Descriptions](#detailed-function-descriptions)
    - [http_parser/1](#http_parser1)
    - [bench/0](#bench0)
    - [test/0](#test0)
4. [Internal Helper Functions](#internal-helper-functions)
    - [split_status_and_body/1](#split_status_and_body1)
    - [parse_status_line/1](#parse_status_line1)
    - [parse_headers/1](#parse_headers1)
5. [Flow Diagram](#flow-diagram)
6. [Conclusion](#conclusion)

## Public APIs

### http_parser/1

Parses a binary HTTP response into a structured tuple comprising status code, reason phrase, headers, and body.

### bench/0

Benchmarks the parsing function, assessing performance across numerous iterations to facilitate optimization.

### test/0

Executes predefined tests to validate the parser's accuracy and robustness in handling different HTTP responses.

## Type Specifications

- `status()`: A positive integer that represents the HTTP status code.
- `reason()`: An atom that denotes the reason phrase of the response, e.g., `ok` or `no_content`.
- `headers()`: A list of tuples, each containing a binary key and a binary value, representing HTTP headers.
- `body()`: A binary string that embodies the HTTP response body.
- `http_response()`: A tuple that encapsulates a parsed HTTP response or an error indication.

## Detailed Function Descriptions

### http_parser/1

**Purpose:** This function ingests a binary HTTP response and parses it into a well-structured Erlang term, leveraging Erlang's pattern matching and binary processing features for efficient component extraction.

**Workflow:**

1. Divides the binary into headers and body.
2. Isolates the status code and reason phrase from the status line.
3. Processes headers into key-value pairs.
4. Compiles a tuple containing the status code, reason phrase, headers, and body.
5. Returns an error tuple with a descriptive message in case of any parsing failure.

### bench/0

**Objective:** To measure and average the execution time of the `http_parser` function, aiding in performance optimization.

**Implementation:** Repeatedly parses a sample response, computing the average execution time over all iterations.

### test/0

**Goal:** To ensure the parser's output accuracy and reliability across various HTTP response scenarios.

**Method:** Utilizes predefined responses and expected results to verify that the parser's output matches the anticipated outcomes.

## Internal Helper Functions

### split_status_and_body/1

**Functionality:** Separates the headers and body of an HTTP response according to the protocol's specified delimiters.

**Method:** Applies binary splitting techniques to segment the response, signaling an error for any malformed input.

### parse_status_line/1

**Objective:** To extract the HTTP version, status code, and reason phrase from the status line of the response.

**Approach:** Validates and segments the status line, ensuring conformity to format and data integrity, and returns error tuples for any irregularities.

### parse_headers/1

**Purpose:** To transform the header section of an HTTP response into a standardized list of key-value pairs.

**Execution:** Splits the header lines and further each line into key and value pairs, filtering and converting valid headers into a structured list.

## Flow Diagram
This diagram illustrates the sequence of operations performed by the HTTP 1.1 Response Parser. Starting with the client sending a binary HTTP response to the parser, the parser then sequentially splits the binary into headers and body, parses the status line to extract the status code and reason phrase, parses the headers into a list of key-value pairs, and finally, returns the structured response back to the client.

```mermaid
sequenceDiagram
    participant Client
    participant Parser
    participant split_status_and_body
    participant parse_status_line
    participant parse_headers

    Client->>+Parser: HTTP Response (Binary)
    Parser->>+split_status_and_body: Split binary into headers and body
    split_status_and_body-->>-Parser: Headers Binary, Body Binary

    Parser->>+parse_status_line: Parse status line from Headers Binary
    parse_status_line-->>-Parser: Status, Reason

    Parser->>+parse_headers: Parse headers into key-value pairs
    parse_headers-->>-Parser: Headers List

    Parser->>Client: {Status, Reason, Headers List, Body Binary}

```
## Flow Chart 
```mermaid
flowchart TD
    A[Start] --> B{Is HTTP Response?}
    B -- Yes --> C[Parse Status Line]
    B -- No --> D[Return Error: Not a HTTP Response]
    C --> E{Is Status Line Valid?}
    E -- Yes --> F[Parse Headers]
    E -- No --> G[Return Error: Invalid Status Line]
    F --> H{Are Headers Valid?}
    H -- Yes --> I[Split Headers and Body]
    H -- No --> J[Return Error: Invalid Headers]
    I --> K{Is Body Present?}
    K -- Yes --> L[Parse Body]
    K -- No --> M[No Body to Parse]
    L --> N[Assemble HTTP Response Object]
    M --> N[Assemble HTTP Response Object]
    N --> O[End]

```

## Conclusion

The `http_response_parser` module offers an eloquently designed solution for parsing HTTP responses in Erlang, showcasing a sophisticated API and internal logic. It simplifies the management and interpretation of binary HTTP responses, highlighting the module's efficacy and performance in processing HTTP communications.

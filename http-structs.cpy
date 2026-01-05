*> HTTP request and response data structures
*> Structure for parsing HTTP requests
       01 HTTP-REQUEST.
*> HTTP method (GET, POST, PUT, etc.) - up to 10 characters
          05 REQUEST-METHOD    PIC X(10).
*> Requested URL path - up to 512 characters
          05 REQUEST-PATH      PIC X(512).
*> Raw HTTP request data from client - 8KB maximum
          05 REQUEST-BUFFER    PIC X(8192).
          
*> Structure for building HTTP responses
       01 HTTP-RESPONSE.
*> Complete HTTP response (headers + content) - 64KB maximum
          05 RESPONSE-BUFFER   PIC X(65536).
*> Actual length of response data (binary for efficiency)
          05 RESPONSE-LEN      PIC 9(8) COMP-5.
          
*> Utility fields for HTTP header construction
*> HTTP status line (e.g., "HTTP/1.1 200 OK")
       01 STATUS-LINE          PIC X(50).
*> Content-Type header value (e.g., "text/html")
       01 CONTENT-TYPE-HDR     PIC X(100).
*> Content-Length header value (e.g., "1024")
       01 CONTENT-LENGTH-HDR   PIC X(50).

*> MCP (Model Context Protocol) specific structures
*> JSON-RPC request structure
       01 MCP-REQUEST.
          05 MCP-REQ-METHOD    PIC X(30).
          05 MCP-REQ-ID        PIC 9(9) COMP VALUE 0.
          05 MCP-REQ-PARAMS    PIC X(2048).

*> JSON-RPC response structure
       01 MCP-RESPONSE.
          05 MCP-RESP-ID       PIC 9(9) COMP VALUE 0.
          05 MCP-RESP-STATUS   PIC X(10).
          05 MCP-RESP-BODY     PIC X(8192).
          05 MCP-RESP-LEN      PIC 9(8) COMP-5 VALUE 0.

*> MCP server capabilities
       01 MCP-CAPABILITIES.
          05 CAP-TOOLS         PIC X(10) VALUE "true".
          05 CAP-LOGGING       PIC X(10) VALUE "false".
          05 CAP-RESOURCES     PIC X(10) VALUE "false".

*> Session tracking
       01 MCP-SESSION.
          05 SESSION-ID        PIC X(36) VALUE SPACES.
          05 SESSION-INITIALIZED PIC X VALUE "N".

*> MCP utility fields
       01 MCP-UTIL.
          05 MCP-REQUEST-LEN   PIC 9(4) COMP VALUE 0.
          05 MCP-IS-MCP-CALL   PIC X VALUE "N".
          05 MCP-CRLF          PIC XX VALUE X"0D0A".

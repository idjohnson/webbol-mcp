*> HTTP request parser and response generator module
IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-HANDLER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> Include HTTP request/response data structures
       COPY "http-structs.cpy".
*> Include file handling data structures
       COPY "file-structs.cpy".

*> General purpose index for string operations (binary for efficiency)
       01 WS-INDEX             PIC 9(4) COMP.
*> Position of space character in HTTP request parsing
       01 WS-SPACE-POS         PIC 9(4) COMP.
*> Length of extracted path from HTTP request
       01 WS-PATH-LEN          PIC 9(4) COMP.
*> Return code from called modules (0=success, 1=error)
       01 WS-RETURN-CODE       PIC 9.
*> String representation of file size for Content-Length header
       01 WS-SIZE-STR          PIC X(10).
*> HTTP line terminator sequence (carriage return + line feed)
       01 WS-CRLF              PIC XX VALUE X"0D0A".
*> Decoded path after URL decoding (converts %20 to space, etc.)
       01 WS-DECODED-PATH      PIC X(512).
       
*> Parameters passed from calling program
       LINKAGE SECTION.
*> HTTP request data received from client (8KB max)
       01 LS-REQUEST-BUF       PIC X(8192).
*> Buffer for building HTTP response (64KB max)
       01 LS-RESPONSE-BUF      PIC X(65536).
*> Actual length of generated response
       01 LS-RESPONSE-LEN      PIC 9(8) COMP-5.

*> Program entry point with parameters
       PROCEDURE DIVISION USING LS-REQUEST-BUF LS-RESPONSE-BUF
                                LS-RESPONSE-LEN.
       
*> Main HTTP request processing logic
       MAIN-LOGIC.
*> Initialize HTTP request fields
           MOVE SPACES TO REQUEST-METHOD
           MOVE SPACES TO REQUEST-PATH
           MOVE 0 TO LS-RESPONSE-LEN

      *>   DISPLAY "Raw request: '" LS-REQUEST-BUF(1:80) "'"

*> Find first space in request to separate HTTP method
*> HTTP format: "GET /path HTTP/1.1"
           MOVE 0 TO WS-SPACE-POS
           INSPECT LS-REQUEST-BUF TALLYING WS-SPACE-POS
               FOR CHARACTERS BEFORE INITIAL SPACE
           
      *>   DISPLAY "First space at position: " WS-SPACE-POS
      *>   DISPLAY "Character at pos 4: '" LS-REQUEST-BUF(4:1) "' = "
      *>       FUNCTION ORD(LS-REQUEST-BUF(4:1))
      *>   DISPLAY "Character at pos 5: '" LS-REQUEST-BUF(5:1) "' = "
      *>       FUNCTION ORD(LS-REQUEST-BUF(5:1))
           
*> Extract HTTP method (GET, POST, etc.) from first part of request
           IF WS-SPACE-POS > 0 AND WS-SPACE-POS <= 10
               MOVE LS-REQUEST-BUF(1:WS-SPACE-POS) TO REQUEST-METHOD
      *>       DISPLAY "Method: '" REQUEST-METHOD "'"
           END-IF
           
*> Skip space after method to find start of path
           COMPUTE WS-INDEX = WS-SPACE-POS + 2
      *>   DISPLAY "Starting path search at position: " WS-INDEX
*> Find end of path (next space or line ending)
           MOVE 0 TO WS-PATH-LEN
           PERFORM VARYING WS-SPACE-POS FROM WS-INDEX BY 1
               UNTIL WS-SPACE-POS > 8192
               IF LS-REQUEST-BUF(WS-SPACE-POS:1) = SPACE OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0D" OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0A"
                   COMPUTE WS-PATH-LEN = WS-SPACE-POS - WS-INDEX
      *>           DISPLAY "Found delimiter at position: " WS-SPACE-POS
      *>           DISPLAY "Delimiter is: "
      *>               FUNCTION ORD(LS-REQUEST-BUF(WS-SPACE-POS:1))
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
      *>   DISPLAY "Path starts at: " WS-INDEX
      *>   DISPLAY "Path length: " WS-PATH-LEN
           
*> Extract the URL path from the HTTP request
           IF WS-PATH-LEN > 0 AND WS-PATH-LEN <= 512
               MOVE LS-REQUEST-BUF(WS-INDEX:WS-PATH-LEN)
                   TO REQUEST-PATH
      *>       DISPLAY "Extracted path: '" REQUEST-PATH(1:50) "'"
           END-IF

*> Check if this is an MCP request (POST or GET to /mcp)
           DISPLAY "Extracted path: '" REQUEST-PATH(1:WS-PATH-LEN) "'"
           IF REQUEST-PATH(1:4) = "/mcp" OR 
              REQUEST-PATH(1:5) = "/mcp " OR
              REQUEST-PATH(1:5) = "/mcp\x0d" OR
              REQUEST-PATH(1:5) = "/mcp\x0a"
               DISPLAY "Routing to MCP-HANDLER"
               CALL "MCP-HANDLER" USING LS-REQUEST-BUF
                                       LS-RESPONSE-BUF
                                       LS-RESPONSE-LEN
               GOBACK
           END-IF

*> Decode URL-encoded characters (e.g., %20 -> space)
           CALL "URL-DECODE" USING REQUEST-PATH WS-DECODED-PATH

*> Validate and sanitize the requested path for security
           CALL "PATH-UTILS" USING WS-DECODED-PATH SANITIZED-PATH
                                   WS-RETURN-CODE

      *>   DISPLAY "Requested path: '" REQUEST-PATH "'"
      *>   DISPLAY "Decoded path: '" WS-DECODED-PATH "'"
      *>   DISPLAY "Sanitized path: '" SANITIZED-PATH "'"
      *>   DISPLAY "Path validation result: " WS-RETURN-CODE
           
*> If path validation failed, return 403 Forbidden
           IF WS-RETURN-CODE NOT = 0
               PERFORM BUILD-403-RESPONSE
               GOBACK
           END-IF

*> Attempt to read the requested file
           CALL "FILE-OPS" USING SANITIZED-PATH FILE-BUFFER
                                 FILE-SIZE WS-RETURN-CODE

      *>   DISPLAY "File read result: " WS-RETURN-CODE
      *>   DISPLAY "File size: " FILE-SIZE

*> If file is too large, return 413 Payload Too Large
           IF WS-RETURN-CODE = 2
               PERFORM BUILD-413-RESPONSE
               GOBACK
           END-IF

*> If file read failed, return 404 Not Found
           IF WS-RETURN-CODE NOT = 0
      *>       DISPLAY "File not found: '" SANITIZED-PATH "'"
               PERFORM BUILD-404-RESPONSE
               GOBACK
           END-IF

*> Determine MIME type based on file extension
           CALL "MIME-TYPES" USING SANITIZED-PATH MIME-TYPE

*> Build successful HTTP response with file content
           PERFORM BUILD-200-RESPONSE
           
           GOBACK.
       
*> Build HTTP 200 OK response with file content
       BUILD-200-RESPONSE.
*> Convert file size to string for Content-Length header
           MOVE FILE-SIZE TO WS-SIZE-STR
*> Initialize response buffer with LOW-VALUE for string termination
           MOVE LOW-VALUE TO LS-RESPONSE-BUF

*> Build HTTP response headers using STRING statement
*> STRING concatenates multiple values into one field
           STRING "HTTP/1.1 200 OK" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: " DELIMITED BY SIZE
                  MIME-TYPE DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: " DELIMITED BY SIZE
                  WS-SIZE-STR DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

*> Calculate length of HTTP headers
           MOVE 0 TO LS-RESPONSE-LEN
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE

      *>   DISPLAY "Header length: " LS-RESPONSE-LEN

*> Append file content after headers if file was read successfully
           IF LS-RESPONSE-LEN > 0 AND FILE-SIZE > 0
               MOVE FILE-BUFFER(1:FILE-SIZE) TO
                   LS-RESPONSE-BUF(LS-RESPONSE-LEN + 1:FILE-SIZE)
               ADD FILE-SIZE TO LS-RESPONSE-LEN
           END-IF

      *>   DISPLAY "Total response length: " LS-RESPONSE-LEN
      *>   DISPLAY "File size: " FILE-SIZE
           .
       
*> Build HTTP 404 Not Found response
       BUILD-404-RESPONSE.
*> Create complete HTTP response with headers and HTML body
           STRING "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>404 Not Found</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

*> Calculate total response length for sending
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .
       
*> Build HTTP 403 Forbidden response (for security violations)
       BUILD-403-RESPONSE.
*> Create complete HTTP response for path traversal attempts
           STRING "HTTP/1.1 403 Forbidden" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>403 Forbidden</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

*> Calculate total response length for sending
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .

*> Build HTTP 413 Payload Too Large response (for oversized files)
       BUILD-413-RESPONSE.
*> Create complete HTTP response for files exceeding buffer size
           STRING "HTTP/1.1 413 Payload Too Large" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 59" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>413 Payload Too Large</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

*> Calculate total response length for sending
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .

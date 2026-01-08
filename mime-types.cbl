*> MIME type detection based on file extensions
IDENTIFICATION DIVISION.
       PROGRAM-ID. MIME-TYPES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> Index for scanning filename backwards to find extension
       01 WS-INDEX             PIC 9(4) COMP.
*> Length of filename string
       01 WS-LENGTH            PIC 9(4) COMP.
*> Starting position of file extension after the dot
       01 WS-EXT-START         PIC 9(4) COMP.
       
*> Parameters from calling program
       LINKAGE SECTION.
*> Full file path to analyze for extension
       01 LS-FILE-PATH         PIC X(512).
*> Output MIME type string (e.g., "text/html")
       01 LS-MIME-TYPE         PIC X(64).
       
       PROCEDURE DIVISION USING LS-FILE-PATH LS-MIME-TYPE.
       
*> Main MIME type detection logic
       MAIN-LOGIC.
*> Default MIME type for unknown extensions (binary data)
           MOVE "application/octet-stream" TO LS-MIME-TYPE

*> Find the file extension by locating the last dot in filename
           MOVE 0 TO WS-EXT-START
*> Calculate filename length (excluding trailing spaces)
           INSPECT LS-FILE-PATH TALLYING WS-LENGTH
               FOR CHARACTERS BEFORE INITIAL SPACE

*> Scan backwards from end of filename to find last dot
           PERFORM VARYING WS-INDEX FROM WS-LENGTH BY -1
               UNTIL WS-INDEX < 1
               IF LS-FILE-PATH(WS-INDEX:1) = "."
                   COMPUTE WS-EXT-START = WS-INDEX + 1
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
*> If no extension found, return default MIME type
           IF WS-EXT-START = 0
               GOBACK
           END-IF
           
*> Map file extensions to MIME types for HTTP Content-Type header
*> EVALUATE compares first 4 characters of extension
           EVALUATE LS-FILE-PATH(WS-EXT-START:4)
               WHEN "html"
                   MOVE "text/html" TO LS-MIME-TYPE
               WHEN "htm "
                   MOVE "text/html" TO LS-MIME-TYPE
               WHEN "css "
                   MOVE "text/css" TO LS-MIME-TYPE
               WHEN "js  "
                   MOVE "application/javascript" TO LS-MIME-TYPE
               WHEN "json"
                   MOVE "application/json" TO LS-MIME-TYPE
               WHEN "xml "
                   MOVE "application/xml" TO LS-MIME-TYPE
               WHEN "txt "
                   MOVE "text/plain" TO LS-MIME-TYPE
               WHEN "png "
                   MOVE "image/png" TO LS-MIME-TYPE
               WHEN "jpg "
                   MOVE "image/jpeg" TO LS-MIME-TYPE
               WHEN "jpeg"
                   MOVE "image/jpeg" TO LS-MIME-TYPE
               WHEN "gif "
                   MOVE "image/gif" TO LS-MIME-TYPE
               WHEN "svg "
                   MOVE "image/svg+xml" TO LS-MIME-TYPE
               WHEN "ico "
                   MOVE "image/x-icon" TO LS-MIME-TYPE
               WHEN "pdf "
                   MOVE "application/pdf" TO LS-MIME-TYPE
               WHEN OTHER
                   MOVE "application/octet-stream" TO LS-MIME-TYPE
           END-EVALUATE
           
           GOBACK.

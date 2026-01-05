# COBOL MCP Server

This is a basic MCP (Model Context Protocol) server implementation in COBOL using the httpStreamable transport, as specified in the [MCP specification (2025-06-18)](https://modelcontextprotocol.io/specification/2025-06-18).

## Features

- **HTTP Streamable Transport**: Implements the MCP httpStreamable transport over HTTP POST and GET
- **JSON-RPC 2.0 Protocol**: Full JSON-RPC 2.0 message format support
- **MCP Initialization Handshake**: Proper capability negotiation and protocol version handling
- **Server-Sent Events (SSE)**: GET endpoint support for streaming responses
- **Minimal Tool Support**: Exposes an example MCP server with tool capabilities

## How to Run

### Build

```bash
make clean && make
```

This compiles all COBOL modules and creates the `webserver` executable.

### Start the Server

```bash
./webserver
```

The server listens on port 8080 by default. You'll see:
```
COBOL Web Server Starting...
Press Ctrl+C to stop

Server listening on port 08080
```

### Test with curl

#### 1. Initialize the connection (POST)

```bash
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}'
```

Expected response:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "protocolVersion": "2025-06-18",
        "capabilities": {
            "tools": {}
        },
        "serverInfo": {
            "name": "COBOL",
            "version": "1.0"
        }
    }
}
```

#### 2. Open SSE stream (GET)

```bash
curl http://localhost:8080/mcp
```

This opens a Server-Sent Events stream for server-initiated messages. The connection will stream any notifications or responses the server sends.

## Implementation Details

### Files Modified

- **mcp-handler.cbl**: New module that handles all MCP protocol logic
  - Detects HTTP POST vs GET methods
  - Builds proper JSON-RPC responses
  - Implements SSE support for bidirectional communication

- **http-handler.cbl**: Updated to route `/mcp` requests to the MCP handler
  - Detects requests to the `/mcp` endpoint
  - Delegates processing to MCP-HANDLER
  - Maintains backward compatibility with static file serving

- **http-structs.cpy**: Added MCP-specific data structures
  - MCP request/response structures
  - Capability flags
  - Session management fields

- **Makefile**: Updated to include mcp-handler.o in compilation

### MCP Protocol Flow

1. **Client POSTs to `/mcp`** with JSON-RPC request
   - Method: `initialize`
   - Server responds with protocol version and capabilities

2. **Client PUTs to `/mcp`** for subsequent requests
   - These follow the initialized session

3. **Client GETs to `/mcp`** to open SSE stream
   - Server sends notifications and responses over this stream
   - Connection remains open for bidirectional communication

## Current Limitations

- **Hardcoded JSON**: Responses use fixed Content-Length headers (hardcoded for init response)
- **No tool call handling**: The `tools/call` endpoint is not yet fully implemented
- **No request parsing**: The server doesn't parse the JSON body to extract parameters
- **Session management**: No persistent session tracking across requests

## Future Enhancements

1. Parse JSON request bodies to extract method name and parameters
2. Implement actual tool execution (the "hello" tool in the example)
3. Add proper session management with session IDs
4. Support resumable connections per MCP spec
5. Implement proper error handling with JSON-RPC error codes
6. Add resource and prompt support beyond just tools

## Testing with MCP Clients

This server can be tested with:
- Python FastMCP clients
- Official MCP SDK clients
- Any standard MCP-compatible tool

Example with Python FastMCP:
```python
from fastmcp import Client

async with Client("http://localhost:8080/mcp") as client:
    tools = await client.list_tools()
    print(tools)
```

## Configuration

- **Port**: Change `SERVER-PORT` in [config.cpy](config.cpy) and rebuild
- **MCP Endpoint**: Currently fixed at `/mcp` path in http-handler.cbl
- **Protocol Version**: Currently hardcoded as `2025-06-18` in mcp-handler.cbl

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/specification/2025-06-18)
- [MCP Transports Documentation](https://modelcontextprotocol.io/specification/2025-06-18/basic/transports)
- [FastMCP Example Project](https://github.com/jlowin/fastmcp)

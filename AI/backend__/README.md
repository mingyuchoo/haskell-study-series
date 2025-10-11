# OpenAI Haskell Agent

## Overview

This Haskell application implements a multi-turn conversation with OpenAI's API. It allows you to have an interactive chat with GPT-4o, maintaining conversation history and streaming responses in real-time. The application is available both as a CLI tool and as a web application.

The project follows the Onion Architecture pattern for better separation of concerns, testability, and maintainability.

## Features

- Multi-turn conversation with OpenAI's GPT-4o model
- Streaming responses for real-time interaction
- Conversation history management
- Available as both CLI and web application
- RESTful API for chat integration
- Modern web interface
- Onion Architecture for better maintainability and testability

## Prerequisites

- Haskell and Stack build tool
- OpenAI API key (set as environment variable)

## Setup

1. Clone the repository
2. Copy the `.env.example` file to `.env` and set your configuration:
   ```bash
   cp .env.example .env
   # Edit .env with your OpenAI API key and other settings
   ```
3. Build the project:
   ```bash
   stack build
   ```

## Running the Application

### CLI Mode (Legacy)

To run the original CLI application:
```bash
# This requires modifying Main.hs to use the CLI version
stack run
```

### Web Application Mode

To run the web application:
```bash
stack run
```

The web server will start on the port specified in your `.env` file (default: 8000).
Access the web interface at: http://localhost:8000

## Usage

### CLI Mode

- Enter your message when prompted with `You: `
- The AI will respond with streaming text
- To exit the conversation, type `:q`, `quit`, or `exit`

### Web Application

- Open your browser to http://localhost:8000
- Type your message in the input field and click Send or press Enter
- View the conversation history in the chat window

## Implementation Details

The application uses:  
- `http-client` and `http-client-tls` for API communication
- `aeson` for JSON parsing
- `stm` and `async` for handling streaming responses
- `text` for text processing
- `servant` for RESTful API implementation
- `warp` for the web server
- `wai-cors` for Cross-Origin Resource Sharing support

## Architecture

This project implements the Onion Architecture pattern with the following layers:

### 1. Domain Layer (Core)

- `Domain.Entities.*`: Core business entities (Message, Chat)
- `Domain.Interfaces.*`: Core interfaces (ChatService, HttpClient)

### 2. Application Layer

- `Application.Interfaces.*`: Application interfaces (ChatApplicationService)
- `Application.Services.*`: Application services (ChatService, ChatApplicationService)

### 3. Infrastructure Layer

- `Infrastructure.Http.*`: HTTP client implementations (HttpClient)
- `Infrastructure.OpenAI.*`: OpenAI-specific implementations (OpenAIService)

### 4. Presentation Layer

- `Presentation.Api.*`: API controllers (ApiHandler)
- `Presentation.Server.*`: Server implementation (Server)

This architecture provides better separation of concerns, testability, and maintainability. Dependencies point inward, with the domain layer having no dependencies on outer layers.

## Troubleshooting

### Common Issues

#### Empty Responses from OpenAI API

If you receive empty responses from the OpenAI API (`{"outputMessage":"","outputSessionId":"..."}`), check the following:

1. **JSON Encoding**: Ensure the `encodeRequest` function in `Application.Services.ChatService` is properly encoding the ChatRequest to JSON.
2. **Response Parsing**: Verify that `parseChatStreamLine` in `Infrastructure.OpenAI.OpenAIService` is correctly imported and used in the HttpClient.

#### Build Errors

If you encounter build errors related to missing modules:

1. **Module Exports**: Check that all required functions are properly exported in their respective module headers.
2. **Main Module**: Ensure the Main module is not incorrectly listed in the library section of package.yaml.

## API Documentation

### Chat Endpoint

- **URL**: `/api/chat`
- **Method**: POST
- **Headers**:
  - `Content-Type: application/json` (required)
- **Request Body**:
  ```json
  {
    "inputMessage": "Your message here",
    "sessionId": null  // or a valid UUID string for continuing a conversation
  }
  ```
- **Response**:
  ```json
  {
    "outputMessage": "AI response here",
    "sessionId": "session-uuid"
  }
  ```
- **Example curl command**:
  ```bash
  curl -X POST http://localhost:8000/api/chat \
    -H "Content-Type: application/json" \
    -d '{"inputMessage": "Hello", "sessionId": null}'
  ```

### Health Check

- **URL**: `/health`
- **Method**: GET
- **Response**:
  ```json
  {
    "status": "UP",
    "version": "0.1.0.0",
    "uptime": 0,
    "timestamp": "2025-04-27T19:57:51Z"
  }
  ```
- **Example curl command**:
  ```bash
  curl -X GET http://localhost:8000/health
  ```
  


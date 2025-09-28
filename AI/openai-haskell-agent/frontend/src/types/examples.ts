/**
 * Usage examples for the TypeScript interfaces and types
 * This file demonstrates how to use the types in practice
 */

import {
  ChatInput,
  ChatOutput,
  HealthInfo,
  Message,
  ChatState,
  ErrorType,
  ApiError,
  isChatOutput,
  isValidMessageContent,
  isSuccessStatusCode
} from './index';

// Example: Creating a ChatInput for the API
export function createChatInput(message: string, sessionId: string | null): ChatInput {
  return {
    inputMessage: message,
    sessionId
  };
}

// Example: Processing API response with type guards
export function processChatResponse(response: unknown): ChatOutput | null {
  if (isChatOutput(response)) {
    return response;
  }
  return null;
}

// Example: Creating a new message for the chat state
export function createMessage(content: string, role: 'user' | 'assistant'): Message {
  return {
    id: crypto.randomUUID(),
    content,
    role,
    timestamp: new Date()
  };
}

// Example: Initial chat state
export function createInitialChatState(): ChatState {
  return {
    messages: [],
    sessionId: null,
    isLoading: false,
    isConnected: false,
    error: null
  };
}

// Example: Creating an API error
export function createApiError(
  type: ErrorType,
  message: string,
  statusCode?: number
): ApiError {
  return {
    type,
    message,
    statusCode,
    details: statusCode ? `HTTP ${statusCode}` : undefined
  };
}

// Example: Validating user input before sending
export function validateMessageInput(input: string): boolean {
  return isValidMessageContent(input);
}

// Example: Handling HTTP response
export function handleHttpResponse(statusCode: number, data: unknown): {
  success: boolean;
  data?: unknown;
  error?: ApiError;
} {
  if (isSuccessStatusCode(statusCode)) {
    return { success: true, data };
  }
  
  return {
    success: false,
    error: createApiError(
      statusCode >= 500 ? ErrorType.SERVER_ERROR : ErrorType.NETWORK_ERROR,
      `Request failed with status ${statusCode}`,
      statusCode
    )
  };
}
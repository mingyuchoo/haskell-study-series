/**
 * Frontend-specific types for chat functionality
 */

/**
 * Represents a single message in the chat conversation
 */
export interface Message {
  id: string;
  content: string;
  role: 'user' | 'assistant';
  timestamp: Date;
}

/**
 * Complete chat application state
 */
export interface ChatState {
  messages: Message[];
  sessionId: string | null;
  isLoading: boolean;
  isConnected: boolean;
  error: string | null;
}

/**
 * Types of errors that can occur in the application
 */
export const ErrorType = {
  NETWORK_ERROR: 'NETWORK_ERROR',
  SERVER_ERROR: 'SERVER_ERROR',
  VALIDATION_ERROR: 'VALIDATION_ERROR',
  TIMEOUT_ERROR: 'TIMEOUT_ERROR',
  UNKNOWN_ERROR: 'UNKNOWN_ERROR'
} as const;

export type ErrorType = typeof ErrorType[keyof typeof ErrorType];

/**
 * Structured error information
 */
export interface ApiError {
  type: ErrorType;
  message: string;
  statusCode?: number;
  details?: string;
}
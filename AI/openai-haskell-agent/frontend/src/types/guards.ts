/**
 * Type guards and validation utilities
 * These functions provide runtime type checking and validation
 */

import { ChatInput, ChatOutput, HealthInfo } from './api';
import { Message, ErrorType } from './chat';

/**
 * Type guard to check if an object is a valid ChatInput
 */
export function isChatInput(obj: unknown): obj is ChatInput {
  return (
    typeof obj === 'object' &&
    obj !== null &&
    typeof (obj as ChatInput).inputMessage === 'string' &&
    ((obj as ChatInput).sessionId === null || typeof (obj as ChatInput).sessionId === 'string')
  );
}

/**
 * Type guard to check if an object is a valid ChatOutput
 */
export function isChatOutput(obj: unknown): obj is ChatOutput {
  return (
    typeof obj === 'object' &&
    obj !== null &&
    typeof (obj as ChatOutput).outputMessage === 'string' &&
    typeof (obj as ChatOutput).outputSessionId === 'string'
  );
}

/**
 * Type guard to check if an object is a valid HealthInfo
 */
export function isHealthInfo(obj: unknown): obj is HealthInfo {
  return (
    typeof obj === 'object' &&
    obj !== null &&
    typeof (obj as HealthInfo).status === 'string' &&
    typeof (obj as HealthInfo).version === 'string' &&
    typeof (obj as HealthInfo).uptime === 'number' &&
    typeof (obj as HealthInfo).timestamp === 'string'
  );
}

/**
 * Type guard to check if an object is a valid Message
 */
export function isMessage(obj: unknown): obj is Message {
  return (
    typeof obj === 'object' &&
    obj !== null &&
    typeof (obj as Message).id === 'string' &&
    typeof (obj as Message).content === 'string' &&
    ((obj as Message).role === 'user' || (obj as Message).role === 'assistant') &&
    (obj as Message).timestamp instanceof Date
  );
}

/**
 * Validates that a string is not empty or just whitespace
 */
export function isValidMessageContent(content: string): boolean {
  return typeof content === 'string' && content.trim().length > 0;
}

/**
 * Validates that a session ID is either null or a non-empty string
 */
export function isValidSessionId(sessionId: unknown): sessionId is string | null {
  return sessionId === null || (typeof sessionId === 'string' && sessionId.length > 0);
}

/**
 * Type guard to check if a value is a valid ErrorType
 */
export function isErrorType(value: unknown): value is ErrorType {
  return Object.values(ErrorType).includes(value as ErrorType);
}

/**
 * Validates HTTP response status codes
 */
export function isSuccessStatusCode(statusCode: number): boolean {
  return statusCode >= 200 && statusCode < 300;
}

/**
 * Validates that a timestamp string is a valid ISO date
 */
export function isValidTimestamp(timestamp: string): boolean {
  const date = new Date(timestamp);
  return !isNaN(date.getTime()) && date.toISOString() === timestamp;
}
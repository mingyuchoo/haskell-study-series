/**
 * API Types matching backend schema
 * These interfaces define the structure of data exchanged with the Haskell backend
 */

/**
 * Input payload for chat API endpoint
 */
export interface ChatInput {
  inputMessage: string;
  sessionId: string | null;
}

/**
 * Response payload from chat API endpoint
 */
export interface ChatOutput {
  outputMessage: string;
  outputSessionId: string;
}

/**
 * Response payload from health check endpoint
 */
export interface HealthInfo {
  status: string;
  version: string;
  uptime: number;
  timestamp: string;
}
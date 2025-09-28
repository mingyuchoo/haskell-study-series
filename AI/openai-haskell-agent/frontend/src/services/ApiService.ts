/**
 * API Service for backend communication
 * Handles all HTTP requests to the Haskell backend with proper error handling,
 * timeout management, and retry logic.
 */

import type { ChatInput, ChatOutput, HealthInfo } from '../types/api';
import type { ApiError } from '../types/chat';
import { ErrorType } from '../types/chat';

/**
 * Retry strategy configuration
 */
interface RetryConfig {
  maxRetries: number;
  baseDelay: number;
  maxDelay: number;
  backoffMultiplier: number;
  retryableErrors: ErrorType[];
}

/**
 * Configuration options for the API service
 */
interface ApiServiceConfig {
  baseUrl: string;
  timeout: number;
  retryConfig: RetryConfig;
}

/**
 * Default retry configuration
 */
const DEFAULT_RETRY_CONFIG: RetryConfig = {
  maxRetries: 3,
  baseDelay: 1000, // 1 second
  maxDelay: 10000, // 10 seconds max
  backoffMultiplier: 2,
  retryableErrors: [
    ErrorType.NETWORK_ERROR,
    ErrorType.TIMEOUT_ERROR,
    ErrorType.SERVER_ERROR
  ]
};

/**
 * Default configuration for the API service
 */
const DEFAULT_CONFIG: ApiServiceConfig = {
  baseUrl: 'http://localhost:8080',
  timeout: 10000, // 10 seconds
  retryConfig: DEFAULT_RETRY_CONFIG,
};

/**
 * API Service class for handling backend communication
 */
export class ApiService {
  private config: ApiServiceConfig;

  constructor(config: Partial<ApiServiceConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  /**
   * Send a chat message to the backend
   * @param input - Chat input containing message and session ID
   * @returns Promise resolving to chat output
   * @throws ApiError on failure
   */
  async sendMessage(input: ChatInput): Promise<ChatOutput> {
    const url = `${this.config.baseUrl}/api/chat`;
    
    return this.makeRequest<ChatOutput>(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(input),
    });
  }

  /**
   * Check backend health status
   * @returns Promise resolving to health information
   * @throws ApiError on failure
   */
  async checkHealth(): Promise<HealthInfo> {
    const url = `${this.config.baseUrl}/health`;
    
    return this.makeRequest<HealthInfo>(url, {
      method: 'GET',
    });
  }

  /**
   * Make an HTTP request with timeout, retry logic, and error handling
   * @param url - Request URL
   * @param options - Fetch options
   * @returns Promise resolving to parsed response data
   * @throws ApiError on failure
   */
  private async makeRequest<T>(url: string, options: RequestInit): Promise<T> {
    let lastError: ApiError | null = null;
    const { maxRetries, baseDelay, maxDelay, backoffMultiplier, retryableErrors } = this.config.retryConfig;

    for (let attempt = 0; attempt <= maxRetries; attempt++) {
      try {
        const response = await this.fetchWithTimeout(url, options);
        
        if (!response.ok) {
          throw this.createHttpError(response.status, response.statusText);
        }

        const data = await response.json();
        return data as T;
      } catch (error) {
        lastError = this.handleRequestError(error, attempt);
        
        // Don't retry if error is not retryable or on final attempt
        if (!retryableErrors.includes(lastError.type) || attempt === maxRetries) {
          throw lastError;
        }

        // Calculate delay with exponential backoff and jitter
        const delay = Math.min(
          baseDelay * Math.pow(backoffMultiplier, attempt),
          maxDelay
        );
        const jitteredDelay = delay + Math.random() * 1000; // Add up to 1s jitter
        
        await this.delay(jitteredDelay);
      }
    }

    throw lastError!;
  }

  /**
   * Fetch with timeout support
   * @param url - Request URL
   * @param options - Fetch options
   * @returns Promise resolving to Response
   */
  private async fetchWithTimeout(url: string, options: RequestInit): Promise<Response> {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), this.config.timeout);

    try {
      const response = await fetch(url, {
        ...options,
        signal: controller.signal,
      });
      return response;
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortError') {
        const timeoutError = new Error('Request timed out');
        timeoutError.name = 'AbortError';
        throw timeoutError;
      }
      throw error;
    } finally {
      clearTimeout(timeoutId);
    }
  }

  /**
   * Handle request errors and convert them to ApiError
   * @param error - The caught error
   * @param attempt - Current attempt number
   * @returns ApiError instance
   */
  private handleRequestError(error: unknown, attempt: number): ApiError {
    // Check if error is already an ApiError (duck typing)
    if (this.isApiError(error)) {
      return error;
    }

    if (error instanceof TypeError) {
      // Network errors (no internet, CORS, etc.)
      return this.createApiError(
        ErrorType.NETWORK_ERROR,
        `Network error (attempt ${attempt + 1}): ${error.message}`
      );
    }

    if (error instanceof Error) {
      if (error.name === 'AbortError') {
        return this.createApiError(ErrorType.TIMEOUT_ERROR, 'Request timed out');
      }
      
      return this.createApiError(
        ErrorType.UNKNOWN_ERROR,
        `Unexpected error: ${error.message}`
      );
    }

    return this.createApiError(
      ErrorType.UNKNOWN_ERROR,
      'An unknown error occurred'
    );
  }

  /**
   * Type guard to check if an object is an ApiError
   * @param error - Object to check
   * @returns True if object is an ApiError
   */
  private isApiError(error: unknown): error is ApiError {
    return (
      typeof error === 'object' &&
      error !== null &&
      'type' in error &&
      'message' in error &&
      Object.values(ErrorType).includes((error as any).type)
    );
  }

  /**
   * Create HTTP error based on status code
   * @param status - HTTP status code
   * @param statusText - HTTP status text
   * @returns ApiError instance
   */
  private createHttpError(status: number, statusText: string): ApiError {
    if (status >= 400 && status < 500) {
      return this.createApiError(
        ErrorType.VALIDATION_ERROR,
        `Client error (${status}): ${statusText}`,
        status
      );
    }

    if (status >= 500) {
      return this.createApiError(
        ErrorType.SERVER_ERROR,
        `Server error (${status}): ${statusText}`,
        status
      );
    }

    return this.createApiError(
      ErrorType.UNKNOWN_ERROR,
      `HTTP error (${status}): ${statusText}`,
      status
    );
  }

  /**
   * Create an ApiError instance
   * @param type - Error type
   * @param message - Error message
   * @param statusCode - Optional HTTP status code
   * @returns ApiError instance
   */
  private createApiError(type: ErrorType, message: string, statusCode?: number): ApiError {
    const error: ApiError = {
      type,
      message,
    };
    
    if (statusCode !== undefined) {
      error.statusCode = statusCode;
    }
    
    return error;
  }

  /**
   * Delay execution for specified milliseconds
   * @param ms - Milliseconds to delay
   * @returns Promise that resolves after delay
   */
  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Update the base URL for the API service
   * @param baseUrl - New base URL
   */
  updateBaseUrl(baseUrl: string): void {
    this.config.baseUrl = baseUrl;
  }

  /**
   * Get current configuration
   * @returns Current API service configuration
   */
  getConfig(): Readonly<ApiServiceConfig> {
    return { ...this.config };
  }

  /**
   * Manually retry a failed operation
   * @param operation - Function that returns a Promise to retry
   * @param maxRetries - Override max retries for this operation
   * @returns Promise resolving to operation result
   */
  async retryOperation<T>(
    operation: () => Promise<T>,
    maxRetries?: number
  ): Promise<T> {
    const retries = maxRetries ?? this.config.retryConfig.maxRetries;
    let lastError: Error | null = null;

    for (let attempt = 0; attempt <= retries; attempt++) {
      try {
        return await operation();
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));
        
        if (attempt === retries) {
          throw lastError;
        }

        // Wait before retrying
        const delay = Math.min(
          this.config.retryConfig.baseDelay * Math.pow(this.config.retryConfig.backoffMultiplier, attempt),
          this.config.retryConfig.maxDelay
        );
        await this.delay(delay);
      }
    }

    throw lastError!;
  }
}

/**
 * Default API service instance
 */
export const apiService = new ApiService();
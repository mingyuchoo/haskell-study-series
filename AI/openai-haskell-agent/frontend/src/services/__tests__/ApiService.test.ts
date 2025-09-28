/**
 * Unit tests for ApiService
 * Tests all methods, error handling, timeout, and retry logic
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { ApiService } from '../ApiService';
import { ChatInput, ChatOutput, HealthInfo } from '../../types/api';
import { ErrorType } from '../../types/chat';

// Mock fetch globally
const mockFetch = vi.fn();
global.fetch = mockFetch;

// Helper function to create a proper Response mock
const createMockResponse = (options: {
  ok: boolean;
  status?: number;
  statusText?: string;
  json?: () => Promise<any>;
}): Response => {
  return {
    ok: options.ok,
    status: options.status || (options.ok ? 200 : 500),
    statusText: options.statusText || (options.ok ? 'OK' : 'Internal Server Error'),
    json: options.json || (async () => ({})),
    headers: new Headers(),
    redirected: false,
    type: 'basic',
    url: '',
    clone: vi.fn(),
    body: null,
    bodyUsed: false,
    arrayBuffer: vi.fn(),
    blob: vi.fn(),
    formData: vi.fn(),
    text: vi.fn(),
  } as Response;
};

describe('ApiService', () => {
  let apiService: ApiService;

  beforeEach(() => {
    apiService = new ApiService({
      baseUrl: 'http://localhost:8080',
      timeout: 5000,
      maxRetries: 2,
      retryDelay: 100,
    });
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('sendMessage', () => {
    const mockChatInput: ChatInput = {
      inputMessage: 'Hello, AI!',
      sessionId: null,
    };

    const mockChatOutput: ChatOutput = {
      outputMessage: 'Hello! How can I help you?',
      outputSessionId: 'session-123',
    };

    it('should send message successfully', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse({
        ok: true,
        json: async () => mockChatOutput,
      }));

      const result = await apiService.sendMessage(mockChatInput);

      expect(result).toEqual(mockChatOutput);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://localhost:8080/api/chat',
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(mockChatInput),
          signal: expect.any(AbortSignal),
        }
      );
    });

    it('should handle server errors', async () => {
      const mockResponse = createMockResponse({
        ok: false,
        status: 500,
        statusText: 'Internal Server Error',
        json: async () => ({}),
      });
      
      // Mock all retry attempts to return the same server error
      mockFetch
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse);

      await expect(apiService.sendMessage(mockChatInput)).rejects.toMatchObject({
        type: ErrorType.SERVER_ERROR,
        message: 'Server error (500): Internal Server Error',
        statusCode: 500,
      });
    });

    it('should handle client errors', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse({
        ok: false,
        status: 400,
        statusText: 'Bad Request',
        json: async () => ({}),
      }));

      await expect(apiService.sendMessage(mockChatInput)).rejects.toMatchObject({
        type: ErrorType.VALIDATION_ERROR,
        message: 'Client error (400): Bad Request',
        statusCode: 400,
      });
    });

    it('should handle network errors', async () => {
      mockFetch.mockRejectedValueOnce(new TypeError('Network error'));

      await expect(apiService.sendMessage(mockChatInput)).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR,
        message: expect.stringContaining('Network error'),
      });
    });

    it('should retry on network errors', async () => {
      mockFetch
        .mockRejectedValueOnce(new TypeError('Network error'))
        .mockRejectedValueOnce(new TypeError('Network error'))
        .mockResolvedValueOnce(createMockResponse({
          ok: true,
          json: async () => mockChatOutput,
        }));

      const result = await apiService.sendMessage(mockChatInput);

      expect(result).toEqual(mockChatOutput);
      expect(mockFetch).toHaveBeenCalledTimes(3);
    });

    it('should not retry on validation errors', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse({
        ok: false,
        status: 400,
        statusText: 'Bad Request',
        json: async () => ({}),
      }));

      await expect(apiService.sendMessage(mockChatInput)).rejects.toMatchObject({
        type: ErrorType.VALIDATION_ERROR,
      });

      expect(mockFetch).toHaveBeenCalledTimes(1);
    });

    it('should handle timeout errors', async () => {
      // Mock AbortError which is what happens on timeout
      const abortError = new Error('The operation was aborted');
      abortError.name = 'AbortError';
      
      // Mock all retry attempts to fail with AbortError
      mockFetch
        .mockRejectedValueOnce(abortError)
        .mockRejectedValueOnce(abortError)
        .mockRejectedValueOnce(abortError);

      await expect(apiService.sendMessage(mockChatInput)).rejects.toMatchObject({
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out',
      });
    });
  });

  describe('checkHealth', () => {
    const mockHealthInfo: HealthInfo = {
      status: 'healthy',
      version: '1.0.0',
      uptime: 3600,
      timestamp: '2023-01-01T00:00:00Z',
    };

    it('should check health successfully', async () => {
      mockFetch.mockResolvedValueOnce(createMockResponse({
        ok: true,
        json: async () => mockHealthInfo,
      }));

      const result = await apiService.checkHealth();

      expect(result).toEqual(mockHealthInfo);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://localhost:8080/health',
        {
          method: 'GET',
          signal: expect.any(AbortSignal),
        }
      );
    });

    it('should handle health check failures', async () => {
      const mockResponse = createMockResponse({
        ok: false,
        status: 503,
        statusText: 'Service Unavailable',
        json: async () => ({}),
      });
      
      // Mock all retry attempts to return the same server error
      mockFetch
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse);

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.SERVER_ERROR,
        message: 'Server error (503): Service Unavailable',
        statusCode: 503,
      });
    });

    it('should retry health check on network errors', async () => {
      mockFetch
        .mockRejectedValueOnce(new TypeError('Network error'))
        .mockResolvedValueOnce(createMockResponse({
          ok: true,
          json: async () => mockHealthInfo,
        }));

      const result = await apiService.checkHealth();

      expect(result).toEqual(mockHealthInfo);
      expect(mockFetch).toHaveBeenCalledTimes(2);
    });
  });

  describe('configuration', () => {
    it('should use default configuration', () => {
      const defaultService = new ApiService();
      const config = defaultService.getConfig();

      expect(config).toEqual({
        baseUrl: 'http://localhost:8080',
        timeout: 10000,
        maxRetries: 3,
        retryDelay: 1000,
      });
    });

    it('should allow custom configuration', () => {
      const customService = new ApiService({
        baseUrl: 'https://api.example.com',
        timeout: 5000,
      });
      const config = customService.getConfig();

      expect(config.baseUrl).toBe('https://api.example.com');
      expect(config.timeout).toBe(5000);
      expect(config.maxRetries).toBe(3); // Should use default
    });

    it('should update base URL', () => {
      apiService.updateBaseUrl('https://new-api.example.com');
      const config = apiService.getConfig();

      expect(config.baseUrl).toBe('https://new-api.example.com');
    });
  });

  describe('error handling edge cases', () => {
    it('should handle JSON parsing errors', async () => {
      const mockResponse = createMockResponse({
        ok: true,
        json: async () => {
          throw new Error('Invalid JSON');
        },
      });
      
      // Mock all retry attempts to return the same JSON parsing error
      mockFetch
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse)
        .mockResolvedValueOnce(mockResponse);

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.UNKNOWN_ERROR,
        message: expect.stringContaining('Invalid JSON'),
      });
    });

    it('should handle unknown error types', async () => {
      // Mock all retries to fail with unknown error to avoid retries
      mockFetch
        .mockRejectedValueOnce('Unknown error')
        .mockRejectedValueOnce('Unknown error')
        .mockRejectedValueOnce('Unknown error');

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.UNKNOWN_ERROR,
        message: 'An unknown error occurred',
      });
    });

    it('should handle AbortError specifically', async () => {
      const abortError = new Error('The operation was aborted');
      abortError.name = 'AbortError';
      
      // Mock all retries to fail with AbortError to avoid retries
      mockFetch
        .mockRejectedValueOnce(abortError)
        .mockRejectedValueOnce(abortError)
        .mockRejectedValueOnce(abortError);

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out',
      });
    });
  });

  describe('retry logic', () => {
    it('should implement exponential backoff', async () => {
      const startTime = Date.now();
      
      mockFetch
        .mockRejectedValueOnce(new TypeError('Network error'))
        .mockRejectedValueOnce(new TypeError('Network error'))
        .mockRejectedValueOnce(new TypeError('Network error'));

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR,
      });

      const endTime = Date.now();
      const elapsed = endTime - startTime;
      
      // Should have waited at least 100ms + 200ms = 300ms for retries
      expect(elapsed).toBeGreaterThan(250);
      expect(mockFetch).toHaveBeenCalledTimes(3); // Initial + 2 retries
    });

    it('should stop retrying after max attempts', async () => {
      mockFetch.mockRejectedValue(new TypeError('Network error'));

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR,
      });

      expect(mockFetch).toHaveBeenCalledTimes(3); // Initial + 2 retries
    });
  });
});
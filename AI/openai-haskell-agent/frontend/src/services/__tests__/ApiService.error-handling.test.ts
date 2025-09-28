import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { ApiService } from '../ApiService';
import { ErrorType } from '../../types/chat';

// Mock fetch globally
const mockFetch = vi.fn();
global.fetch = mockFetch;

describe('ApiService Error Handling', () => {
  let apiService: ApiService;

  beforeEach(() => {
    vi.clearAllMocks();
    apiService = new ApiService({
      baseUrl: 'http://localhost:8080',
      timeout: 1000,
      retryConfig: {
        maxRetries: 2,
        baseDelay: 100,
        maxDelay: 1000,
        backoffMultiplier: 2,
        retryableErrors: [ErrorType.NETWORK_ERROR, ErrorType.TIMEOUT_ERROR, ErrorType.SERVER_ERROR]
      }
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Network Error Handling', () => {
    it('should retry network errors with exponential backoff', async () => {
      // Mock network error (TypeError is thrown by fetch for network issues)
      mockFetch
        .mockRejectedValueOnce(new TypeError('Failed to fetch'))
        .mockRejectedValueOnce(new TypeError('Failed to fetch'))
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ outputMessage: 'Success', outputSessionId: 'session123' })
        } as Response);

      const startTime = Date.now();
      const result = await apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      });

      const endTime = Date.now();
      const duration = endTime - startTime;

      expect(result).toEqual({ outputMessage: 'Success', outputSessionId: 'session123' });
      expect(mockFetch).toHaveBeenCalledTimes(3);
      // Should have waited for retries (at least 100ms + 200ms)
      expect(duration).toBeGreaterThan(250);
    });

    it('should throw network error after max retries', async () => {
      mockFetch.mockRejectedValue(new TypeError('Failed to fetch'));

      await expect(apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR,
        message: expect.stringContaining('Network error')
      });

      expect(mockFetch).toHaveBeenCalledTimes(3); // Initial + 2 retries
    });
  });

  describe('Timeout Error Handling', () => {
    it('should handle timeout errors and retry', async () => {
      const abortError = new Error('Request timed out');
      abortError.name = 'AbortError';

      mockFetch
        .mockRejectedValueOnce(abortError)
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ outputMessage: 'Success', outputSessionId: 'session123' })
        } as Response);

      const result = await apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      });

      expect(result).toEqual({ outputMessage: 'Success', outputSessionId: 'session123' });
      expect(mockFetch).toHaveBeenCalledTimes(2);
    });

    it('should throw timeout error after max retries', async () => {
      const abortError = new Error('Request timed out');
      abortError.name = 'AbortError';
      mockFetch.mockRejectedValue(abortError);

      await expect(apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out'
      });

      expect(mockFetch).toHaveBeenCalledTimes(3);
    });
  });

  describe('Server Error Handling', () => {
    it('should retry server errors (5xx)', async () => {
      mockFetch
        .mockResolvedValueOnce({
          ok: false,
          status: 500,
          statusText: 'Internal Server Error'
        } as Response)
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ outputMessage: 'Success', outputSessionId: 'session123' })
        } as Response);

      const result = await apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      });

      expect(result).toEqual({ outputMessage: 'Success', outputSessionId: 'session123' });
      expect(mockFetch).toHaveBeenCalledTimes(2);
    });

    it('should throw server error after max retries', async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 500,
        statusText: 'Internal Server Error'
      } as Response);

      await expect(apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.SERVER_ERROR,
        message: 'Server error (500): Internal Server Error',
        statusCode: 500
      });

      expect(mockFetch).toHaveBeenCalledTimes(3);
    });
  });

  describe('Client Error Handling', () => {
    it('should not retry client errors (4xx)', async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 400,
        statusText: 'Bad Request'
      } as Response);

      await expect(apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.VALIDATION_ERROR,
        message: 'Client error (400): Bad Request',
        statusCode: 400
      });

      expect(mockFetch).toHaveBeenCalledTimes(1); // No retries
    });

    it('should handle 404 errors as validation errors', async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 404,
        statusText: 'Not Found'
      } as Response);

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.VALIDATION_ERROR,
        message: 'Client error (404): Not Found',
        statusCode: 404
      });

      expect(mockFetch).toHaveBeenCalledTimes(1);
    });
  });

  describe('Retry Configuration', () => {
    it('should respect custom retry configuration', async () => {
      const customApiService = new ApiService({
        retryConfig: {
          maxRetries: 1,
          baseDelay: 50,
          maxDelay: 500,
          backoffMultiplier: 3,
          retryableErrors: [ErrorType.NETWORK_ERROR]
        }
      });

      mockFetch.mockRejectedValue(new TypeError('Failed to fetch'));

      await expect(customApiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR
      });

      expect(mockFetch).toHaveBeenCalledTimes(2); // Initial + 1 retry
    });

    it('should apply jitter to retry delays', async () => {
      const delays: number[] = [];
      const originalSetTimeout = global.setTimeout;
      
      global.setTimeout = vi.fn((callback, delay) => {
        delays.push(delay);
        return originalSetTimeout(callback, 0); // Execute immediately for test
      }) as any;

      mockFetch
        .mockRejectedValueOnce(new TypeError('Failed to fetch'))
        .mockRejectedValueOnce(new TypeError('Failed to fetch'))
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ outputMessage: 'Success', outputSessionId: 'session123' })
        } as Response);

      await apiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      });

      // Should have 2 delays (for 2 retries)
      expect(delays).toHaveLength(2);
      // First delay should be base delay (100ms) + jitter
      expect(delays[0]).toBeGreaterThanOrEqual(100);
      expect(delays[0]).toBeLessThan(1100); // 100 + up to 1000ms jitter
      // Second delay should be exponential backoff (200ms) + jitter
      expect(delays[1]).toBeGreaterThanOrEqual(200);
      expect(delays[1]).toBeLessThan(1200); // 200 + up to 1000ms jitter

      global.setTimeout = originalSetTimeout;
    });

    it('should respect max delay limit', async () => {
      const customApiService = new ApiService({
        retryConfig: {
          maxRetries: 3,
          baseDelay: 500,
          maxDelay: 800, // Lower than what exponential backoff would produce
          backoffMultiplier: 3,
          retryableErrors: [ErrorType.NETWORK_ERROR]
        }
      });

      const delays: number[] = [];
      const originalSetTimeout = global.setTimeout;
      
      global.setTimeout = vi.fn((callback, delay) => {
        delays.push(delay);
        return originalSetTimeout(callback, 0);
      }) as any;

      mockFetch.mockRejectedValue(new TypeError('Failed to fetch'));

      await expect(customApiService.sendMessage({
        inputMessage: 'test message',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR
      });

      // All delays should be capped at maxDelay + jitter
      delays.forEach(delay => {
        expect(delay).toBeLessThan(1800); // 800 max + 1000 jitter
      });

      global.setTimeout = originalSetTimeout;
    });
  });

  describe('Manual Retry Operation', () => {
    it('should retry failed operations manually', async () => {
      let callCount = 0;
      const operation = vi.fn(async () => {
        callCount++;
        if (callCount < 3) {
          throw new Error('Operation failed');
        }
        return 'Success';
      });

      const result = await apiService.retryOperation(operation, 3);

      expect(result).toBe('Success');
      expect(operation).toHaveBeenCalledTimes(3);
    });

    it('should throw error after max retries in manual retry', async () => {
      const operation = vi.fn(async () => {
        throw new Error('Operation failed');
      });

      await expect(apiService.retryOperation(operation, 2)).rejects.toThrow('Operation failed');

      expect(operation).toHaveBeenCalledTimes(3); // Initial + 2 retries
    });

    it('should use default max retries when not specified', async () => {
      const operation = vi.fn(async () => {
        throw new Error('Operation failed');
      });

      await expect(apiService.retryOperation(operation)).rejects.toThrow('Operation failed');

      expect(operation).toHaveBeenCalledTimes(3); // Initial + 2 retries (default)
    });
  });

  describe('Error Type Classification', () => {
    it('should classify different HTTP status codes correctly', async () => {
      const testCases = [
        { status: 400, expectedType: ErrorType.VALIDATION_ERROR },
        { status: 401, expectedType: ErrorType.VALIDATION_ERROR },
        { status: 403, expectedType: ErrorType.VALIDATION_ERROR },
        { status: 404, expectedType: ErrorType.VALIDATION_ERROR },
        { status: 422, expectedType: ErrorType.VALIDATION_ERROR },
        { status: 500, expectedType: ErrorType.SERVER_ERROR },
        { status: 502, expectedType: ErrorType.SERVER_ERROR },
        { status: 503, expectedType: ErrorType.SERVER_ERROR },
        { status: 504, expectedType: ErrorType.SERVER_ERROR }
      ];

      for (const testCase of testCases) {
        mockFetch.mockResolvedValueOnce({
          ok: false,
          status: testCase.status,
          statusText: 'Error'
        } as Response);

        await expect(apiService.sendMessage({
          inputMessage: 'test',
          sessionId: null
        })).rejects.toMatchObject({
          type: testCase.expectedType,
          statusCode: testCase.status
        });
      }
    });

    it('should handle unknown status codes as unknown errors', async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 999,
        statusText: 'Unknown Error'
      } as Response);

      await expect(apiService.sendMessage({
        inputMessage: 'test',
        sessionId: null
      })).rejects.toMatchObject({
        type: ErrorType.UNKNOWN_ERROR,
        statusCode: 999
      });
    });
  });

  describe('Health Check Error Handling', () => {
    it('should handle health check network errors', async () => {
      mockFetch.mockRejectedValue(new TypeError('Failed to fetch'));

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.NETWORK_ERROR,
        message: expect.stringContaining('Network error')
      });
    });

    it('should handle health check server errors', async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 503,
        statusText: 'Service Unavailable'
      } as Response);

      await expect(apiService.checkHealth()).rejects.toMatchObject({
        type: ErrorType.SERVER_ERROR,
        message: 'Server error (503): Service Unavailable',
        statusCode: 503
      });
    });
  });
});
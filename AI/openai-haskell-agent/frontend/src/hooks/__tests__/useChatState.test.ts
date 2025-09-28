import { renderHook, act } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { useChatState } from '../useChatState';
import { ApiService } from '../../services/ApiService';
import { ChatOutput } from '../../types/api';
import { ApiError, ErrorType } from '../../types/chat';

// Mock the ApiService
vi.mock('../../services/ApiService');

describe('useChatState', () => {
  let mockApiService: {
    sendMessage: ReturnType<typeof vi.fn>;
    checkHealth: ReturnType<typeof vi.fn>;
  };

  beforeEach(() => {
    mockApiService = {
      sendMessage: vi.fn(),
      checkHealth: vi.fn(),
    };
  });

  it('should initialize with default state', () => {
    const { result } = renderHook(() => useChatState(mockApiService));

    expect(result.current.state).toEqual({
      messages: [],
      sessionId: null,
      isLoading: false,
      isConnected: false,
      error: null,
    });
  });

  it('should provide all required actions', () => {
    const { result } = renderHook(() => useChatState(mockApiService));

    expect(result.current.actions).toHaveProperty('sendMessage');
    expect(result.current.actions).toHaveProperty('clearConversation');
    expect(result.current.actions).toHaveProperty('setError');
    expect(result.current.actions).toHaveProperty('setConnectionStatus');
    expect(result.current.actions).toHaveProperty('retryLastMessage');
  });

  describe('sendMessage', () => {
    it('should not send empty messages', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      await act(async () => {
        await result.current.actions.sendMessage('');
      });

      expect(mockApiService.sendMessage).not.toHaveBeenCalled();
      expect(result.current.state.messages).toHaveLength(0);
    });

    it('should not send whitespace-only messages', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      await act(async () => {
        await result.current.actions.sendMessage('   ');
      });

      expect(mockApiService.sendMessage).not.toHaveBeenCalled();
      expect(result.current.state.messages).toHaveLength(0);
    });

    it('should add user message immediately and set loading state', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      // Mock a delayed response
      mockApiService.sendMessage.mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve({
          outputMessage: 'AI response',
          outputSessionId: 'session123'
        }), 100))
      );

      act(() => {
        result.current.actions.sendMessage('Hello');
      });

      // Check immediate state after sending
      expect(result.current.state.messages).toHaveLength(1);
      expect(result.current.state.messages[0].content).toBe('Hello');
      expect(result.current.state.messages[0].role).toBe('user');
      expect(result.current.state.isLoading).toBe(true);
      expect(result.current.state.error).toBeNull();
    });

    it('should handle successful API response', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      const mockResponse: ChatOutput = {
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      };
      
      mockApiService.sendMessage.mockResolvedValue(mockResponse);

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(result.current.state.messages).toHaveLength(2);
      expect(result.current.state.messages[0].content).toBe('Hello');
      expect(result.current.state.messages[0].role).toBe('user');
      expect(result.current.state.messages[1].content).toBe('AI response');
      expect(result.current.state.messages[1].role).toBe('assistant');
      expect(result.current.state.sessionId).toBe('session123');
      expect(result.current.state.isLoading).toBe(false);
      expect(result.current.state.error).toBeNull();
    });

    it('should send correct ChatInput to API service', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(mockApiService.sendMessage).toHaveBeenCalledWith({
        inputMessage: 'Hello',
        sessionId: null,
      });
    });

    it('should maintain session ID across multiple messages', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      // First message
      mockApiService.sendMessage.mockResolvedValueOnce({
        outputMessage: 'First response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('First message');
      });

      // Second message should use the session ID
      mockApiService.sendMessage.mockResolvedValueOnce({
        outputMessage: 'Second response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('Second message');
      });

      expect(mockApiService.sendMessage).toHaveBeenNthCalledWith(2, {
        inputMessage: 'Second message',
        sessionId: 'session123',
      });
    });

    it('should handle API errors gracefully', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      const errorMessage = 'Network error';
      mockApiService.sendMessage.mockRejectedValue(new Error(errorMessage));

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(result.current.state.messages).toHaveLength(1); // Only user message
      expect(result.current.state.messages[0].role).toBe('user');
      expect(result.current.state.isLoading).toBe(false);
      expect(result.current.state.error).toBe(errorMessage);
    });

    it('should handle string errors', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      const errorMessage = 'String error';
      mockApiService.sendMessage.mockRejectedValue(errorMessage);

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(result.current.state.error).toBe(errorMessage);
    });

    it('should handle unknown errors', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      mockApiService.sendMessage.mockRejectedValue({ unknown: 'error' });

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(result.current.state.error).toBe('Failed to send message');
    });

    it('should trim whitespace from messages', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('  Hello World  ');
      });

      expect(result.current.state.messages[0].content).toBe('Hello World');
      expect(mockApiService.sendMessage).toHaveBeenCalledWith({
        inputMessage: 'Hello World',
        sessionId: null,
      });
    });
  });

  describe('clearConversation', () => {
    it('should clear all messages and reset session', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      // First add some messages
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      expect(result.current.state.messages).toHaveLength(2);
      expect(result.current.state.sessionId).toBe('session123');

      // Now clear conversation
      act(() => {
        result.current.actions.clearConversation();
      });

      expect(result.current.state.messages).toHaveLength(0);
      expect(result.current.state.sessionId).toBeNull();
      expect(result.current.state.error).toBeNull();
    });

    it('should not affect loading or connection state', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      // Set some state
      act(() => {
        result.current.actions.setConnectionStatus(true);
      });

      // Clear conversation
      act(() => {
        result.current.actions.clearConversation();
      });

      expect(result.current.state.isConnected).toBe(true);
      expect(result.current.state.isLoading).toBe(false);
    });
  });

  describe('setError', () => {
    it('should set error message', () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      act(() => {
        result.current.actions.setError('Test error');
      });

      expect(result.current.state.error).toBe('Test error');
    });

    it('should clear error when set to null', () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      act(() => {
        result.current.actions.setError('Test error');
      });

      expect(result.current.state.error).toBe('Test error');

      act(() => {
        result.current.actions.setError(null);
      });

      expect(result.current.state.error).toBeNull();
    });
  });

  describe('setConnectionStatus', () => {
    it('should update connection status', () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      expect(result.current.state.isConnected).toBe(false);

      act(() => {
        result.current.actions.setConnectionStatus(true);
      });

      expect(result.current.state.isConnected).toBe(true);

      act(() => {
        result.current.actions.setConnectionStatus(false);
      });

      expect(result.current.state.isConnected).toBe(false);
    });
  });

  describe('message generation', () => {
    it('should generate unique message IDs', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('First message');
      });

      await act(async () => {
        await result.current.actions.sendMessage('Second message');
      });

      const messages = result.current.state.messages;
      const messageIds = messages.map(msg => msg.id);
      const uniqueIds = new Set(messageIds);

      expect(uniqueIds.size).toBe(messageIds.length);
      expect(messages.every(msg => msg.id.startsWith('msg_'))).toBe(true);
    });

    it('should set correct timestamps', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));
      
      const beforeTime = new Date();
      
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('Hello');
      });

      const afterTime = new Date();
      const messages = result.current.state.messages;

      expect(messages).toHaveLength(2);
      expect(messages[0].timestamp.getTime()).toBeGreaterThanOrEqual(beforeTime.getTime());
      expect(messages[0].timestamp.getTime()).toBeLessThanOrEqual(afterTime.getTime());
      expect(messages[1].timestamp.getTime()).toBeGreaterThanOrEqual(beforeTime.getTime());
      expect(messages[1].timestamp.getTime()).toBeLessThanOrEqual(afterTime.getTime());
    });
  });
});  desc
ribe('retryLastMessage', () => {
    it('should retry the last failed message', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      // First, send a message that fails
      const apiError: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network connection failed'
      };
      mockApiService.sendMessage.mockRejectedValueOnce(apiError);

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Test message');
        } catch (error) {
          // Expected to fail
        }
      });

      expect(result.current.state.messages).toHaveLength(1); // Only user message
      expect(result.current.state.error).toBe('Network connection failed');

      // Now retry should succeed
      mockApiService.sendMessage.mockResolvedValueOnce({
        outputMessage: 'AI response',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.retryLastMessage();
      });

      expect(result.current.state.messages).toHaveLength(2); // User + AI message
      expect(result.current.state.error).toBeNull();
      expect(mockApiService.sendMessage).toHaveBeenCalledTimes(2);
      expect(mockApiService.sendMessage).toHaveBeenLastCalledWith({
        inputMessage: 'Test message',
        sessionId: null
      });
    });

    it('should remove duplicate user message when retrying', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      // Send a message that fails
      const apiError: ApiError = {
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out'
      };
      mockApiService.sendMessage.mockRejectedValueOnce(apiError);

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Retry test');
        } catch (error) {
          // Expected to fail
        }
      });

      expect(result.current.state.messages).toHaveLength(1);
      expect(result.current.state.messages[0].content).toBe('Retry test');
      expect(result.current.state.messages[0].role).toBe('user');

      // Retry should succeed
      mockApiService.sendMessage.mockResolvedValueOnce({
        outputMessage: 'Retry successful',
        outputSessionId: 'session456'
      });

      await act(async () => {
        await result.current.actions.retryLastMessage();
      });

      expect(result.current.state.messages).toHaveLength(2);
      expect(result.current.state.messages[0].content).toBe('Retry test');
      expect(result.current.state.messages[0].role).toBe('user');
      expect(result.current.state.messages[1].content).toBe('Retry successful');
      expect(result.current.state.messages[1].role).toBe('assistant');
    });

    it('should throw error when no message to retry', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      await act(async () => {
        await expect(result.current.actions.retryLastMessage()).rejects.toThrow('No message to retry');
      });
    });

    it('should handle retry failures', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      // First message fails
      const firstError: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };
      mockApiService.sendMessage.mockRejectedValueOnce(firstError);

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Failing message');
        } catch (error) {
          // Expected to fail
        }
      });

      // Retry also fails
      const retryError: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Server error'
      };
      mockApiService.sendMessage.mockRejectedValueOnce(retryError);

      await act(async () => {
        try {
          await result.current.actions.retryLastMessage();
        } catch (error) {
          // Expected to fail
        }
      });

      expect(result.current.state.messages).toHaveLength(1); // Only user message
      expect(result.current.state.error).toBe('Server error');
      expect(mockApiService.sendMessage).toHaveBeenCalledTimes(2);
    });

    it('should clear last message reference on successful send', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      // Send a successful message
      mockApiService.sendMessage.mockResolvedValueOnce({
        outputMessage: 'Success',
        outputSessionId: 'session123'
      });

      await act(async () => {
        await result.current.actions.sendMessage('Successful message');
      });

      expect(result.current.state.messages).toHaveLength(2);

      // Try to retry - should fail because there's no failed message
      await act(async () => {
        await expect(result.current.actions.retryLastMessage()).rejects.toThrow('No message to retry');
      });
    });
  });

  describe('error handling improvements', () => {
    it('should handle ApiError objects in sendMessage', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      const apiError: ApiError = {
        type: ErrorType.VALIDATION_ERROR,
        message: 'Invalid input provided',
        statusCode: 400,
        details: 'Message too long'
      };

      mockApiService.sendMessage.mockRejectedValueOnce(apiError);

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Test message');
        } catch (error) {
          expect(error).toBe(apiError);
        }
      });

      expect(result.current.state.error).toBe('Invalid input provided');
      expect(result.current.state.isLoading).toBe(false);
    });

    it('should handle Error objects in sendMessage', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      const error = new Error('Standard error message');
      mockApiService.sendMessage.mockRejectedValueOnce(error);

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Test message');
        } catch (thrownError) {
          expect(thrownError).toBeInstanceOf(Error);
          expect((thrownError as Error).message).toBe('Standard error message');
        }
      });

      expect(result.current.state.error).toBe('Standard error message');
    });

    it('should handle string errors in sendMessage', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      mockApiService.sendMessage.mockRejectedValueOnce('String error message');

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Test message');
        } catch (error) {
          expect(error).toBeInstanceOf(Error);
          expect((error as Error).message).toBe('String error message');
        }
      });

      expect(result.current.state.error).toBe('String error message');
    });

    it('should handle unknown error types in sendMessage', async () => {
      const { result } = renderHook(() => useChatState(mockApiService));

      mockApiService.sendMessage.mockRejectedValueOnce({ unknown: 'error' });

      await act(async () => {
        try {
          await result.current.actions.sendMessage('Test message');
        } catch (error) {
          expect(error).toBeInstanceOf(Error);
          expect((error as Error).message).toBe('Failed to send message');
        }
      });

      expect(result.current.state.error).toBe('Failed to send message');
    });
  });
});
/**
 * Integration tests for complete user flows
 * Tests the full integration between App, ChatContainer, and all child components
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent, waitFor, act } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import App from '../App';
import { ApiService } from '../services/ApiService';
import type { ChatOutput, HealthInfo } from '../types/api';

// Mock the ApiService
vi.mock('../services/ApiService');

// Mock environment variables
vi.mock('import.meta', () => ({
  env: {
    VITE_API_BASE_URL: 'http://localhost:8080',
    VITE_API_TIMEOUT: '10000',
    VITE_API_MAX_RETRIES: '3',
    VITE_API_RETRY_DELAY: '1000',
  }
}));

describe('Complete User Flow Integration Tests', () => {
  let mockApiService: {
    checkHealth: ReturnType<typeof vi.fn>;
    sendMessage: ReturnType<typeof vi.fn>;
  };

  const mockHealthResponse: HealthInfo = {
    status: 'healthy',
    version: '1.0.0',
    uptime: 12345,
    timestamp: new Date().toISOString(),
  };

  beforeEach(() => {
    // Reset all mocks
    vi.clearAllMocks();
    vi.clearAllTimers();
    
    // Create mock API service methods
    mockApiService = {
      checkHealth: vi.fn(),
      sendMessage: vi.fn(),
    };

    // Mock the ApiService constructor to return our mock
    (ApiService as any).mockImplementation(() => mockApiService);
  });

  afterEach(() => {
    vi.clearAllTimers();
  });

  describe('Complete Conversation Flow', () => {
    it('should handle a complete conversation from startup to multiple message exchanges', async () => {
      const user = userEvent.setup();
      
      // Mock successful health check
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // Mock successful message responses
      const mockResponses: ChatOutput[] = [
        {
          outputMessage: 'Hello! How can I help you today?',
          outputSessionId: 'session-123'
        },
        {
          outputMessage: 'That\'s a great question! Let me help you with that.',
          outputSessionId: 'session-123'
        },
        {
          outputMessage: 'Is there anything else you\'d like to know?',
          outputSessionId: 'session-123'
        }
      ];

      mockApiService.sendMessage
        .mockResolvedValueOnce(mockResponses[0])
        .mockResolvedValueOnce(mockResponses[1])
        .mockResolvedValueOnce(mockResponses[2]);

      render(<App />);

      // Wait for initial health check and connection
      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Verify initial state
      expect(screen.getByText('0 messages')).toBeInTheDocument();
      expect(screen.getByPlaceholderText('Type your message...')).toBeInTheDocument();

      // Send first message
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'Hello, I need help with something');
      await user.keyboard('{Enter}');

      // Verify loading state
      await waitFor(() => {
        expect(screen.getByPlaceholderText('Sending message...')).toBeInTheDocument();
      });

      // Wait for response and verify session continuity
      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenCalledWith({
          inputMessage: 'Hello, I need help with something',
          sessionId: null // First message should have null sessionId
        });
      });

      await waitFor(() => {
        expect(screen.getByText('Hello! How can I help you today?')).toBeInTheDocument();
        expect(screen.getByText('2 messages')).toBeInTheDocument();
      });

      // Send second message with session continuity
      await user.clear(messageInput);
      await user.type(messageInput, 'Can you explain how this works?');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenCalledWith({
          inputMessage: 'Can you explain how this works?',
          sessionId: 'session-123' // Should use session from first response
        });
      });

      await waitFor(() => {
        expect(screen.getByText('That\'s a great question! Let me help you with that.')).toBeInTheDocument();
        expect(screen.getByText('4 messages')).toBeInTheDocument();
      });

      // Send third message to verify continued session
      await user.clear(messageInput);
      await user.type(messageInput, 'Thank you, that was helpful!');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenCalledWith({
          inputMessage: 'Thank you, that was helpful!',
          sessionId: 'session-123' // Should continue using same session
        });
      });

      await waitFor(() => {
        expect(screen.getByText('Is there anything else you\'d like to know?')).toBeInTheDocument();
        expect(screen.getByText('6 messages')).toBeInTheDocument();
      });

      // Verify all messages are displayed in order
      const messageList = screen.getByTestId('message-list');
      expect(messageList).toBeInTheDocument();
    });

    it('should handle session reset when conversation is cleared', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      const mockResponses: ChatOutput[] = [
        {
          outputMessage: 'First conversation response',
          outputSessionId: 'session-123'
        },
        {
          outputMessage: 'New conversation response',
          outputSessionId: 'session-456'
        }
      ];

      mockApiService.sendMessage
        .mockResolvedValueOnce(mockResponses[0])
        .mockResolvedValueOnce(mockResponses[1]);

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Send first message
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'First message');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(screen.getByText('First conversation response')).toBeInTheDocument();
        expect(screen.getByText('2 messages')).toBeInTheDocument();
      });

      // Clear conversation
      const clearButton = screen.getByTestId('clear-conversation-button');
      await user.click(clearButton);

      // Confirm clearing
      const confirmButton = screen.getByTestId('confirm-clear-button');
      await user.click(confirmButton);

      await waitFor(() => {
        expect(screen.getByText('0 messages')).toBeInTheDocument();
        expect(screen.getByText('Conversation cleared')).toBeInTheDocument();
      });

      // Send new message after clearing
      await user.type(messageInput, 'New conversation message');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenLastCalledWith({
          inputMessage: 'New conversation message',
          sessionId: null // Should reset to null after clearing
        });
      });

      await waitFor(() => {
        expect(screen.getByText('New conversation response')).toBeInTheDocument();
        expect(screen.getByText('2 messages')).toBeInTheDocument();
      });
    });
  });

  describe('Error Recovery Flow', () => {
    it('should handle network errors with retry functionality', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // First attempt fails, second succeeds
      mockApiService.sendMessage
        .mockRejectedValueOnce(new Error('Network connection failed'))
        .mockResolvedValueOnce({
          outputMessage: 'Message sent successfully after retry',
          outputSessionId: 'session-123'
        });

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Send message that will fail
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'This message will fail first');
      await user.keyboard('{Enter}');

      // Wait for error to appear
      await waitFor(() => {
        expect(screen.getByText(/Unable to connect to the server/)).toBeInTheDocument();
        expect(screen.getByTestId('error-retry-button')).toBeInTheDocument();
      });

      // Retry the message
      const retryButton = screen.getByTestId('error-retry-button');
      await user.click(retryButton);

      // Wait for successful retry
      await waitFor(() => {
        expect(screen.getByText('Message sent successfully after retry')).toBeInTheDocument();
        expect(screen.queryByTestId('error-display')).not.toBeInTheDocument();
      });

      // Verify the message was retried with same content
      expect(mockApiService.sendMessage).toHaveBeenCalledTimes(2);
      expect(mockApiService.sendMessage).toHaveBeenLastCalledWith({
        inputMessage: 'This message will fail first',
        sessionId: null
      });
    });

    it('should handle connection status changes and recovery', async () => {
      const user = userEvent.setup();
      
      // Start with failed health check
      mockApiService.checkHealth.mockRejectedValueOnce(new Error('Connection failed'));
      
      render(<App />);

      // Wait for disconnected state
      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Disconnected');
        expect(screen.getByPlaceholderText('Connecting to server...')).toBeInTheDocument();
      });

      // Verify input is disabled when disconnected
      const messageInput = screen.getByPlaceholderText('Connecting to server...');
      expect(messageInput).toBeDisabled();

      // Simulate connection recovery
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // Wait for periodic health check to succeed (we need to advance timers)
      act(() => {
        vi.advanceTimersByTime(30000); // Advance by health check interval
      });

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
        expect(screen.getByPlaceholderText('Type your message...')).toBeInTheDocument();
      });

      // Verify input is enabled after reconnection
      const enabledInput = screen.getByPlaceholderText('Type your message...');
      expect(enabledInput).not.toBeDisabled();

      // Test that messaging works after reconnection
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'Connection restored successfully',
        outputSessionId: 'session-123'
      });

      await user.type(enabledInput, 'Test message after reconnection');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(screen.getByText('Connection restored successfully')).toBeInTheDocument();
      });
    });

    it('should handle validation errors without retry option', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // Mock validation error
      mockApiService.sendMessage.mockRejectedValue({
        type: 'VALIDATION_ERROR',
        message: 'Invalid input provided'
      });

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Send message that will cause validation error
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'Invalid message');
      await user.keyboard('{Enter}');

      // Wait for error to appear
      await waitFor(() => {
        expect(screen.getByText(/There was an issue with your request/)).toBeInTheDocument();
        expect(screen.queryByTestId('error-retry-button')).not.toBeInTheDocument();
        expect(screen.getByTestId('error-dismiss-button')).toBeInTheDocument();
      });

      // Dismiss the error
      const dismissButton = screen.getByTestId('error-dismiss-button');
      await user.click(dismissButton);

      await waitFor(() => {
        expect(screen.queryByTestId('error-display')).not.toBeInTheDocument();
      });
    });
  });

  describe('State Management Across Components', () => {
    it('should maintain proper state synchronization across all components', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      mockApiService.sendMessage.mockResolvedValue({
        outputMessage: 'State sync test response',
        outputSessionId: 'session-123'
      });

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Verify initial state across components
      expect(screen.getByText('0 messages')).toBeInTheDocument(); // Footer
      expect(screen.getByTestId('connection-status')).toHaveTextContent('connected'); // StatusIndicator
      expect(screen.getByPlaceholderText('Type your message...')).not.toBeDisabled(); // MessageInput

      // Send a message and verify state updates across all components
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'Test state synchronization');
      await user.keyboard('{Enter}');

      // During loading state
      await waitFor(() => {
        expect(screen.getByPlaceholderText('Sending message...')).toBeInTheDocument(); // MessageInput
        expect(screen.getByTestId('connection-status')).toHaveTextContent('connecting'); // StatusIndicator
        expect(screen.getByTestId('typing-indicator')).toBeInTheDocument(); // StatusIndicator
      });

      // After message is sent
      await waitFor(() => {
        expect(screen.getByText('State sync test response')).toBeInTheDocument(); // MessageList
        expect(screen.getByText('2 messages')).toBeInTheDocument(); // Footer
        expect(screen.getByTestId('connection-status')).toHaveTextContent('connected'); // StatusIndicator
        expect(screen.queryByTestId('typing-indicator')).not.toBeInTheDocument(); // StatusIndicator
        expect(screen.getByPlaceholderText('Type your message...')).not.toBeDisabled(); // MessageInput
      });
    });

    it('should handle concurrent state updates correctly', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // Mock delayed response to test concurrent handling
      mockApiService.sendMessage.mockImplementation(() => 
        new Promise(resolve => 
          setTimeout(() => resolve({
            outputMessage: 'Delayed response',
            outputSessionId: 'session-123'
          }), 100)
        )
      );

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      const messageInput = screen.getByPlaceholderText('Type your message...');
      
      // Send first message
      await user.type(messageInput, 'First message');
      await user.keyboard('{Enter}');

      // Immediately try to send second message (should be disabled)
      await user.clear(messageInput);
      await user.type(messageInput, 'Second message');
      
      // Input should be disabled during first message processing
      expect(screen.getByPlaceholderText('Sending message...')).toBeDisabled();

      // Wait for first message to complete
      await waitFor(() => {
        expect(screen.getByText('Delayed response')).toBeInTheDocument();
        expect(screen.getByPlaceholderText('Type your message...')).not.toBeDisabled();
      });

      // Now second message should be sendable
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenCalledTimes(2);
      });
    });
  });

  describe('Component Integration Edge Cases', () => {
    it('should handle rapid clear and send operations', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      mockApiService.sendMessage
        .mockResolvedValueOnce({
          outputMessage: 'First response',
          outputSessionId: 'session-123'
        })
        .mockResolvedValueOnce({
          outputMessage: 'Second response after clear',
          outputSessionId: 'session-456'
        });

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      // Send first message
      const messageInput = screen.getByPlaceholderText('Type your message...');
      await user.type(messageInput, 'First message');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(screen.getByText('First response')).toBeInTheDocument();
      });

      // Rapidly clear conversation and send new message
      const clearButton = screen.getByTestId('clear-conversation-button');
      await user.click(clearButton);
      
      const confirmButton = screen.getByTestId('confirm-clear-button');
      await user.click(confirmButton);

      // Immediately send new message
      await user.type(messageInput, 'Message after rapid clear');
      await user.keyboard('{Enter}');

      await waitFor(() => {
        expect(mockApiService.sendMessage).toHaveBeenLastCalledWith({
          inputMessage: 'Message after rapid clear',
          sessionId: null // Should be null after clear
        });
      });

      await waitFor(() => {
        expect(screen.getByText('Second response after clear')).toBeInTheDocument();
        expect(screen.queryByText('First response')).not.toBeInTheDocument();
      });
    });

    it('should handle error dismissal during loading state', async () => {
      const user = userEvent.setup();
      
      mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
      
      // First call fails, second succeeds
      mockApiService.sendMessage
        .mockRejectedValueOnce(new Error('Network error'))
        .mockImplementation(() => 
          new Promise(resolve => 
            setTimeout(() => resolve({
              outputMessage: 'Success after error',
              outputSessionId: 'session-123'
            }), 200)
          )
        );

      render(<App />);

      await waitFor(() => {
        const footer = screen.getByRole('contentinfo');
        expect(footer).toHaveTextContent('Connected');
      });

      const messageInput = screen.getByPlaceholderText('Type your message...');
      
      // Send message that will fail
      await user.type(messageInput, 'This will fail');
      await user.keyboard('{Enter}');

      // Wait for error
      await waitFor(() => {
        expect(screen.getByTestId('error-display')).toBeInTheDocument();
      });

      // Start retry (which will take time)
      const retryButton = screen.getByTestId('error-retry-button');
      await user.click(retryButton);

      // Immediately try to dismiss error during retry
      const dismissButton = screen.getByTestId('error-dismiss-button');
      await user.click(dismissButton);

      // Error should be dismissed even though retry is in progress
      expect(screen.queryByTestId('error-display')).not.toBeInTheDocument();

      // Wait for retry to complete
      await waitFor(() => {
        expect(screen.getByText('Success after error')).toBeInTheDocument();
      });
    });
  });
});
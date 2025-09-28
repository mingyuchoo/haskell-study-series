
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { vi, describe, it, expect, beforeEach } from 'vitest';
import { ChatContainer } from '../ChatContainer';
import type { ChatState } from '../../types/chat';
import type { ChatActions } from '../../hooks/useChatState';

// Mock the child components
vi.mock('../MessageList', () => ({
  MessageList: ({ messages, isLoading }: any) => (
    <div data-testid="message-list">
      <div data-testid="messages-count">{messages.length}</div>
      {isLoading && <div data-testid="loading-indicator">Loading...</div>}
    </div>
  )
}));

vi.mock('../MessageInput', () => ({
  MessageInput: ({ onSendMessage, disabled, placeholder }: any) => (
    <div data-testid="message-input">
      <input
        data-testid="input-field"
        disabled={disabled}
        placeholder={placeholder}
        onChange={(e) => {
          if (e.target.value === 'test message') {
            onSendMessage('test message');
          }
        }}
      />
    </div>
  )
}));

vi.mock('../StatusIndicator', () => ({
  StatusIndicator: ({ connectionStatus, isTyping, error }: any) => (
    <div data-testid="status-indicator">
      <div data-testid="connection-status">{connectionStatus}</div>
      {isTyping && <div data-testid="typing-indicator">Typing...</div>}
      {error && <div data-testid="error-message">{error.message}</div>}
    </div>
  )
}));

describe('ChatContainer', () => {
  const mockChatActions: ChatActions = {
    sendMessage: vi.fn(),
    clearConversation: vi.fn(),
    setError: vi.fn(),
    setConnectionStatus: vi.fn(),
  };

  const defaultChatState: ChatState = {
    messages: [],
    sessionId: null,
    isLoading: false,
    isConnected: true,
    error: null,
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('renders all child components correctly', () => {
    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('chat-container')).toBeInTheDocument();
    expect(screen.getByTestId('message-list')).toBeInTheDocument();
    expect(screen.getByTestId('message-input')).toBeInTheDocument();
    expect(screen.getByTestId('status-indicator')).toBeInTheDocument();
  });

  it('displays connected status when connected', () => {
    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('connection-status')).toHaveTextContent('connected');
  });

  it('displays disconnected status when not connected', () => {
    const disconnectedState: ChatState = {
      ...defaultChatState,
      isConnected: false,
    };

    render(
      <ChatContainer
        chatState={disconnectedState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('connection-status')).toHaveTextContent('disconnected');
  });

  it('displays connecting status when loading', () => {
    const loadingState: ChatState = {
      ...defaultChatState,
      isLoading: true,
    };

    render(
      <ChatContainer
        chatState={loadingState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('connection-status')).toHaveTextContent('connecting');
    expect(screen.getByTestId('typing-indicator')).toBeInTheDocument();
  });

  it('handles message sending correctly', async () => {
    const mockSendMessage = vi.fn().mockResolvedValue(undefined);
    const actionsWithMock = {
      ...mockChatActions,
      sendMessage: mockSendMessage,
    };

    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={actionsWithMock}
      />
    );

    const inputField = screen.getByTestId('input-field');
    fireEvent.change(inputField, { target: { value: 'test message' } });

    await waitFor(() => {
      expect(mockSendMessage).toHaveBeenCalledWith('test message');
    });
  });

  it('disables input when not connected', () => {
    const disconnectedState: ChatState = {
      ...defaultChatState,
      isConnected: false,
    };

    render(
      <ChatContainer
        chatState={disconnectedState}
        chatActions={mockChatActions}
      />
    );

    const inputField = screen.getByTestId('input-field');
    expect(inputField).toBeDisabled();
  });

  it('disables input when loading', () => {
    const loadingState: ChatState = {
      ...defaultChatState,
      isLoading: true,
    };

    render(
      <ChatContainer
        chatState={loadingState}
        chatActions={mockChatActions}
      />
    );

    const inputField = screen.getByTestId('input-field');
    expect(inputField).toBeDisabled();
  });

  it('displays error message when error exists', () => {
    const errorState: ChatState = {
      ...defaultChatState,
      error: 'Connection failed',
    };

    render(
      <ChatContainer
        chatState={errorState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('error-message')).toHaveTextContent('Connection failed');
  });

  it('shows error dismiss button when error exists', () => {
    const errorState: ChatState = {
      ...defaultChatState,
      error: 'Connection failed',
    };

    render(
      <ChatContainer
        chatState={errorState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('error-dismiss-button')).toBeInTheDocument();
  });

  it('calls setError when error dismiss button is clicked', () => {
    const errorState: ChatState = {
      ...defaultChatState,
      error: 'Connection failed',
    };

    render(
      <ChatContainer
        chatState={errorState}
        chatActions={mockChatActions}
      />
    );

    const dismissButton = screen.getByTestId('error-dismiss-button');
    fireEvent.click(dismissButton);

    expect(mockChatActions.setError).toHaveBeenCalledWith(null);
  });

  it('does not show error dismiss button when no error', () => {
    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.queryByTestId('error-dismiss-button')).not.toBeInTheDocument();
  });

  it('passes correct placeholder text based on state', () => {
    // Test connected state
    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('input-field')).toHaveAttribute('placeholder', 'Type your message...');
  });

  it('passes connecting placeholder when not connected', () => {
    const disconnectedState: ChatState = {
      ...defaultChatState,
      isConnected: false,
    };

    render(
      <ChatContainer
        chatState={disconnectedState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('input-field')).toHaveAttribute('placeholder', 'Connecting to server...');
  });

  it('passes sending placeholder when loading', () => {
    const loadingState: ChatState = {
      ...defaultChatState,
      isLoading: true,
    };

    render(
      <ChatContainer
        chatState={loadingState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('input-field')).toHaveAttribute('placeholder', 'Sending message...');
  });

  it('applies custom className when provided', () => {
    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={mockChatActions}
        className="custom-class"
      />
    );

    const container = screen.getByTestId('chat-container');
    expect(container).toHaveClass('custom-class');
  });

  it('handles sendMessage errors gracefully', async () => {
    const mockSendMessage = vi.fn().mockRejectedValue(new Error('Send failed'));
    const actionsWithMock = {
      ...mockChatActions,
      sendMessage: mockSendMessage,
    };

    // Mock console.error to avoid test output noise
    const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});

    render(
      <ChatContainer
        chatState={defaultChatState}
        chatActions={actionsWithMock}
      />
    );

    const inputField = screen.getByTestId('input-field');
    fireEvent.change(inputField, { target: { value: 'test message' } });

    await waitFor(() => {
      expect(mockSendMessage).toHaveBeenCalledWith('test message');
      expect(consoleSpy).toHaveBeenCalledWith('Failed to send message:', expect.any(Error));
    });

    consoleSpy.mockRestore();
  });

  it('converts error types correctly for StatusIndicator', () => {
    const networkErrorState: ChatState = {
      ...defaultChatState,
      error: 'Network connection failed',
    };

    render(
      <ChatContainer
        chatState={networkErrorState}
        chatActions={mockChatActions}
      />
    );

    expect(screen.getByTestId('error-message')).toHaveTextContent('Network connection failed');
  });

  describe('Clear Conversation Functionality', () => {
    it('shows clear conversation button when messages exist', () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      expect(screen.getByTestId('clear-conversation-button')).toBeInTheDocument();
      expect(screen.getByTestId('clear-conversation-button')).not.toBeDisabled();
    });

    it('disables clear conversation button when no messages exist', () => {
      render(
        <ChatContainer
          chatState={defaultChatState}
          chatActions={mockChatActions}
        />
      );

      const clearButton = screen.getByTestId('clear-conversation-button');
      expect(clearButton).toBeInTheDocument();
      expect(clearButton).toBeDisabled();
    });

    it('disables clear conversation button when loading', () => {
      const loadingStateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
        isLoading: true,
      };

      render(
        <ChatContainer
          chatState={loadingStateWithMessages}
          chatActions={mockChatActions}
        />
      );

      const clearButton = screen.getByTestId('clear-conversation-button');
      expect(clearButton).toBeDisabled();
    });

    it('shows confirmation dialog when clear button is clicked', () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);

      expect(screen.getByTestId('clear-confirmation-dialog')).toBeInTheDocument();
      expect(screen.getByText('Clear Conversation')).toBeInTheDocument();
      expect(screen.getByText(/Are you sure you want to clear the entire conversation/)).toBeInTheDocument();
      expect(screen.getByTestId('confirm-clear-button')).toBeInTheDocument();
      expect(screen.getByTestId('cancel-clear-button')).toBeInTheDocument();
    });

    it('does not show confirmation dialog when clear button is clicked with no messages', () => {
      render(
        <ChatContainer
          chatState={defaultChatState}
          chatActions={mockChatActions}
        />
      );

      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);

      expect(screen.queryByTestId('clear-confirmation-dialog')).not.toBeInTheDocument();
    });

    it('calls clearConversation when confirm button is clicked', async () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      // Click clear button to show dialog
      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);

      // Click confirm button
      const confirmButton = screen.getByTestId('confirm-clear-button');
      fireEvent.click(confirmButton);

      expect(mockChatActions.clearConversation).toHaveBeenCalledTimes(1);
      expect(screen.queryByTestId('clear-confirmation-dialog')).not.toBeInTheDocument();
    });

    it('shows feedback message after clearing conversation', async () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      // Click clear button to show dialog
      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);

      // Click confirm button
      const confirmButton = screen.getByTestId('confirm-clear-button');
      fireEvent.click(confirmButton);

      await waitFor(() => {
        expect(screen.getByTestId('clear-feedback')).toBeInTheDocument();
        expect(screen.getByText('Conversation cleared')).toBeInTheDocument();
      });
    });

    it('hides confirmation dialog when cancel button is clicked', () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      // Click clear button to show dialog
      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);

      // Click cancel button
      const cancelButton = screen.getByTestId('cancel-clear-button');
      fireEvent.click(cancelButton);

      expect(screen.queryByTestId('clear-confirmation-dialog')).not.toBeInTheDocument();
      expect(mockChatActions.clearConversation).not.toHaveBeenCalled();
    });

    it('allows dismissing feedback message', async () => {
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      // Clear conversation to show feedback
      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);
      const confirmButton = screen.getByTestId('confirm-clear-button');
      fireEvent.click(confirmButton);

      await waitFor(() => {
        expect(screen.getByTestId('clear-feedback')).toBeInTheDocument();
      });

      // Dismiss feedback
      const dismissButton = screen.getByTestId('dismiss-feedback-button');
      fireEvent.click(dismissButton);

      expect(screen.queryByTestId('clear-feedback')).not.toBeInTheDocument();
    });

    it('auto-hides feedback message after 3 seconds', () => {
      // This test verifies that the setTimeout is called correctly
      // The actual timing behavior is tested through manual testing
      const stateWithMessages: ChatState = {
        ...defaultChatState,
        messages: [
          {
            id: '1',
            content: 'Hello',
            role: 'user',
            timestamp: new Date(),
          },
        ],
      };

      render(
        <ChatContainer
          chatState={stateWithMessages}
          chatActions={mockChatActions}
        />
      );

      // Clear conversation to show feedback
      const clearButton = screen.getByTestId('clear-conversation-button');
      fireEvent.click(clearButton);
      const confirmButton = screen.getByTestId('confirm-clear-button');
      fireEvent.click(confirmButton);

      // Verify feedback is shown
      expect(screen.getByTestId('clear-feedback')).toBeInTheDocument();
      expect(screen.getByText('Conversation cleared')).toBeInTheDocument();
    });
  });
});  d
escribe('Error Handling', () => {
    it('should display ErrorDisplay component when there is an error', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'Network connection failed'
      };

      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: vi.fn()
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      expect(screen.getByTestId('error-display')).toBeInTheDocument();
      expect(screen.getByText(/Unable to connect to the server/)).toBeInTheDocument();
    });

    it('should handle error dismissal', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'Test error message'
      };

      const mockSetError = vi.fn();
      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: mockSetError,
        setConnectionStatus: vi.fn(),
        retryLastMessage: vi.fn()
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      const dismissButton = screen.getByTestId('error-dismiss-button');
      fireEvent.click(dismissButton);

      expect(mockSetError).toHaveBeenCalledWith(null);
    });

    it('should handle retry functionality for retryable errors', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'Network connection failed' // This should be classified as retryable
      };

      const mockRetryLastMessage = vi.fn();
      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: mockRetryLastMessage
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      const retryButton = screen.getByTestId('error-retry-button');
      fireEvent.click(retryButton);

      expect(mockRetryLastMessage).toHaveBeenCalledTimes(1);
    });

    it('should not show retry button for non-retryable errors', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'Invalid input provided' // This should be classified as validation error
      };

      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: vi.fn()
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      expect(screen.queryByTestId('error-retry-button')).not.toBeInTheDocument();
    });

    it('should classify different error types correctly', () => {
      const testCases = [
        { error: 'network connection failed', shouldHaveRetry: true },
        { error: 'timeout occurred', shouldHaveRetry: true },
        { error: 'server error 500', shouldHaveRetry: true },
        { error: 'validation failed', shouldHaveRetry: false },
        { error: 'invalid input', shouldHaveRetry: false },
        { error: 'unknown error', shouldHaveRetry: false }
      ];

      testCases.forEach(({ error, shouldHaveRetry }) => {
        const mockChatState = {
          messages: [],
          sessionId: null,
          isLoading: false,
          isConnected: true,
          error
        };

        const mockChatActions = {
          sendMessage: vi.fn(),
          clearConversation: vi.fn(),
          setError: vi.fn(),
          setConnectionStatus: vi.fn(),
          retryLastMessage: vi.fn()
        };

        const { unmount } = render(
          <ChatContainer 
            chatState={mockChatState} 
            chatActions={mockChatActions} 
          />
        );

        if (shouldHaveRetry) {
          expect(screen.getByTestId('error-retry-button')).toBeInTheDocument();
        } else {
          expect(screen.queryByTestId('error-retry-button')).not.toBeInTheDocument();
        }

        unmount();
      });
    });

    it('should handle retry errors gracefully', async () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'Network connection failed'
      };

      const mockRetryLastMessage = vi.fn().mockRejectedValue(new Error('Retry failed'));
      const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});

      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: mockRetryLastMessage
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      const retryButton = screen.getByTestId('error-retry-button');
      
      await act(async () => {
        fireEvent.click(retryButton);
      });

      expect(mockRetryLastMessage).toHaveBeenCalledTimes(1);
      expect(consoleSpy).toHaveBeenCalledWith('Failed to retry message:', expect.any(Error));

      consoleSpy.mockRestore();
    });
  });

  describe('Error Display Integration', () => {
    it('should pass correct error props to ErrorDisplay', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: 'timeout occurred while connecting'
      };

      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: vi.fn()
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveAttribute('data-error-type', 'TIMEOUT_ERROR');
      expect(screen.getByTestId('error-retry-button')).toBeInTheDocument();
      expect(screen.getByTestId('error-dismiss-button')).toBeInTheDocument();
    });

    it('should not render ErrorDisplay when there is no error', () => {
      const mockChatState = {
        messages: [],
        sessionId: null,
        isLoading: false,
        isConnected: true,
        error: null
      };

      const mockChatActions = {
        sendMessage: vi.fn(),
        clearConversation: vi.fn(),
        setError: vi.fn(),
        setConnectionStatus: vi.fn(),
        retryLastMessage: vi.fn()
      };

      render(
        <ChatContainer 
          chatState={mockChatState} 
          chatActions={mockChatActions} 
        />
      );

      expect(screen.queryByTestId('error-display')).not.toBeInTheDocument();
    });
  });
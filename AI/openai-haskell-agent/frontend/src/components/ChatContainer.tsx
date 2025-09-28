import React, { useCallback, useMemo, useState } from 'react';
import { MessageList } from './MessageList';
import { MessageInput } from './MessageInput';
import { StatusIndicator, type ConnectionStatus } from './StatusIndicator';
import { ErrorDisplay } from './ErrorDisplay';
import type { ChatState, ApiError } from '../types/chat';
import { ErrorType } from '../types/chat';
import type { ChatActions } from '../hooks/useChatState';
import styles from './ChatContainer.module.css';

/**
 * Props interface for the ChatContainer component
 */
export interface ChatContainerProps {
  /** Current chat state */
  chatState: ChatState;
  /** Chat actions for state management */
  chatActions: ChatActions;
  /** Additional CSS class name */
  className?: string;
}

/**
 * ChatContainer component - Main chat interface orchestrator
 * Integrates MessageList, MessageInput, and StatusIndicator components
 * Handles message sending logic, loading states, and error feedback
 */
export const ChatContainer: React.FC<ChatContainerProps> = ({
  chatState,
  chatActions,
  className
}) => {
  const { messages, isLoading, isConnected, error } = chatState;
  const { sendMessage, setError, clearConversation, retryLastMessage } = chatActions;
  
  // State for confirmation dialog
  const [showClearConfirmation, setShowClearConfirmation] = useState(false);
  const [clearFeedback, setClearFeedback] = useState<string | null>(null);

  /**
   * Handles message sending with error handling
   */
  const handleSendMessage = useCallback(async (message: string) => {
    try {
      await sendMessage(message);
    } catch (error) {
      // Error is already handled in the hook, but we can add additional UI feedback here
      console.error('Failed to send message:', error);
    }
  }, [sendMessage]);

  /**
   * Handles retry functionality
   */
  const handleRetry = useCallback(async () => {
    try {
      await retryLastMessage();
    } catch (error) {
      console.error('Failed to retry message:', error);
    }
  }, [retryLastMessage]);

  /**
   * Determines connection status based on current state
   */
  const connectionStatus: ConnectionStatus = useMemo(() => {
    if (isLoading) {
      return 'connecting';
    }
    return isConnected ? 'connected' : 'disconnected';
  }, [isConnected, isLoading]);

  /**
   * Converts error string to ApiError format for display components
   */
  const apiError: ApiError | null = useMemo(() => {
    if (!error) return null;
    
    // Try to determine error type from message content
    let errorType: ApiError['type'] = ErrorType.UNKNOWN_ERROR;
    
    if (error.toLowerCase().includes('network') || error.toLowerCase().includes('connection')) {
      errorType = ErrorType.NETWORK_ERROR;
    } else if (error.toLowerCase().includes('timeout')) {
      errorType = ErrorType.TIMEOUT_ERROR;
    } else if (error.toLowerCase().includes('server')) {
      errorType = ErrorType.SERVER_ERROR;
    } else if (error.toLowerCase().includes('validation') || error.toLowerCase().includes('invalid')) {
      errorType = ErrorType.VALIDATION_ERROR;
    }

    return {
      type: errorType,
      message: error
    };
  }, [error]);

  /**
   * Determines if retry is available for the current error
   */
  const canRetry = useMemo(() => {
    if (!apiError) return false;
    
    // Allow retry for network, timeout, and server errors
    return [
      ErrorType.NETWORK_ERROR,
      ErrorType.TIMEOUT_ERROR,
      ErrorType.SERVER_ERROR
    ].includes(apiError.type);
  }, [apiError]);

  /**
   * Handles error dismissal
   */
  const handleErrorDismiss = useCallback(() => {
    setError(null);
  }, [setError]);

  /**
   * Handles clear conversation button click
   */
  const handleClearClick = useCallback(() => {
    if (messages.length === 0) {
      return; // No messages to clear
    }
    setShowClearConfirmation(true);
  }, [messages.length]);

  /**
   * Confirms clearing the conversation
   */
  const handleClearConfirm = useCallback(() => {
    clearConversation();
    setShowClearConfirmation(false);
    setClearFeedback('Conversation cleared');
    
    // Hide feedback after 3 seconds
    setTimeout(() => {
      setClearFeedback(null);
    }, 3000);
  }, [clearConversation]);

  /**
   * Cancels clearing the conversation
   */
  const handleClearCancel = useCallback(() => {
    setShowClearConfirmation(false);
  }, []);

  /**
   * Dismisses clear feedback
   */
  const handleFeedbackDismiss = useCallback(() => {
    setClearFeedback(null);
  }, []);

  // Combine CSS classes
  const containerClasses = [
    styles['chatContainer'],
    className
  ].filter(Boolean).join(' ');

  return (
    <div className={containerClasses} data-testid="chat-container">
      {/* Status indicator at the top */}
      <div className={styles['statusSection']}>
        <StatusIndicator
          connectionStatus={connectionStatus}
          isTyping={isLoading}
          error={apiError}
          className={styles['statusIndicator']}
        />
        
        <div className={styles['actionButtons']}>
          {/* Clear conversation button */}
          <button
            className={styles['clearButton']}
            onClick={handleClearClick}
            disabled={messages.length === 0 || isLoading}
            aria-label="Clear conversation"
            data-testid="clear-conversation-button"
            title="Clear conversation"
          >
            🗑️
          </button>
          
          {/* Error dismissal button */}
          {error && (
            <button
              className={styles['errorDismissButton']}
              onClick={handleErrorDismiss}
              aria-label="Dismiss error"
              data-testid="error-dismiss-button"
            >
              ✕
            </button>
          )}
        </div>
      </div>

      {/* Clear confirmation dialog */}
      {showClearConfirmation && (
        <div className={styles['confirmationOverlay']} data-testid="clear-confirmation-dialog">
          <div className={styles['confirmationDialog']}>
            <h3>Clear Conversation</h3>
            <p>Are you sure you want to clear the entire conversation? This action cannot be undone.</p>
            <div className={styles['confirmationButtons']}>
              <button
                className={styles['confirmButton']}
                onClick={handleClearConfirm}
                data-testid="confirm-clear-button"
              >
                Clear
              </button>
              <button
                className={styles['cancelButton']}
                onClick={handleClearCancel}
                data-testid="cancel-clear-button"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Clear feedback message */}
      {clearFeedback && (
        <div className={styles['feedbackMessage']} data-testid="clear-feedback">
          <span>{clearFeedback}</span>
          <button
            className={styles['feedbackDismissButton']}
            onClick={handleFeedbackDismiss}
            aria-label="Dismiss feedback"
            data-testid="dismiss-feedback-button"
          >
            ✕
          </button>
        </div>
      )}

      {/* Error display */}
      {apiError && (
        <ErrorDisplay
          error={apiError}
          onRetry={canRetry ? handleRetry : undefined}
          onDismiss={handleErrorDismiss}
          canRetry={canRetry}
          className={styles['errorDisplay']}
        />
      )}

      {/* Main chat area */}
      <div className={styles['chatContent']}>
        {/* Message history */}
        <div className={styles['messageSection']}>
          <MessageList
            messages={messages}
            isLoading={isLoading}
            className={styles['messageList']}
          />
        </div>

        {/* Input area */}
        <div className={styles['inputSection']}>
          <MessageInput
            onSendMessage={handleSendMessage}
            disabled={!isConnected || isLoading}
            placeholder={
              !isConnected 
                ? "Connecting to server..." 
                : isLoading 
                  ? "Sending message..." 
                  : "Type your message..."
            }
            maxLength={1000}
          />
        </div>
      </div>
    </div>
  );
};

export default ChatContainer;
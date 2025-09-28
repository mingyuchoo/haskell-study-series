import React, { useState, useCallback, KeyboardEvent, FormEvent } from 'react';
import styles from './MessageInput.module.css';

/**
 * Props for the MessageInput component
 */
export interface MessageInputProps {
  /** Function to call when a message is sent */
  onSendMessage: (message: string) => void;
  /** Whether the input should be disabled (e.g., during loading) */
  disabled?: boolean;
  /** Placeholder text for the input field */
  placeholder?: string;
  /** Maximum length for input messages */
  maxLength?: number;
}

/**
 * MessageInput component for user message input
 * Provides text input with send button, Enter key submission, and validation
 */
export const MessageInput: React.FC<MessageInputProps> = ({
  onSendMessage,
  disabled = false,
  placeholder = "Type your message...",
  maxLength = 1000,
}) => {
  const [inputValue, setInputValue] = useState<string>('');

  /**
   * Validates and sends the message
   */
  const handleSendMessage = useCallback(() => {
    const trimmedMessage = inputValue.trim();
    
    // Validate input - don't send empty messages
    if (!trimmedMessage || disabled) {
      return;
    }

    // Send the message and clear input
    onSendMessage(trimmedMessage);
    setInputValue('');
  }, [inputValue, onSendMessage, disabled]);

  /**
   * Handles Enter key submission
   */
  const handleKeyDown = useCallback((event: KeyboardEvent<HTMLTextAreaElement>) => {
    if (event.key === 'Enter' && !event.shiftKey) {
      event.preventDefault();
      handleSendMessage();
    }
  }, [handleSendMessage]);

  /**
   * Handles form submission (for accessibility)
   */
  const handleSubmit = useCallback((event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    handleSendMessage();
  }, [handleSendMessage]);

  /**
   * Handles input value changes
   */
  const handleInputChange = useCallback((event: React.ChangeEvent<HTMLTextAreaElement>) => {
    setInputValue(event.target.value);
  }, []);

  // Check if send button should be disabled
  const isSendDisabled = disabled || !inputValue.trim();

  return (
    <form className={styles.messageInputContainer} onSubmit={handleSubmit}>
      <div className={styles.inputWrapper}>
        <textarea
          className={`${styles.messageInput} ${disabled ? styles.disabled : ''}`}
          value={inputValue}
          onChange={handleInputChange}
          onKeyDown={handleKeyDown}
          placeholder={disabled ? "Connecting..." : placeholder}
          disabled={disabled}
          maxLength={maxLength}
          rows={1}
          aria-label="Message input"
          aria-describedby="send-button"
        />
        <button
          type="submit"
          className={`${styles.sendButton} ${isSendDisabled ? styles.disabled : ''}`}
          disabled={isSendDisabled}
          aria-label="Send message"
          id="send-button"
        >
          {disabled ? (
            <span className={styles.loadingSpinner} aria-hidden="true" />
          ) : (
            <span className={styles.sendIcon} aria-hidden="true">
              ➤
            </span>
          )}
        </button>
      </div>
      {disabled && (
        <div className={styles.statusMessage} aria-live="polite">
          Processing message...
        </div>
      )}
    </form>
  );
};

export type { MessageInputProps };
import React, { useEffect, useRef } from 'react';
import type { Message as MessageType } from '../types/chat';
import { Message } from './Message';
import styles from './MessageList.module.css';

/**
 * Props interface for the MessageList component
 */
export interface MessageListProps {
  messages: MessageType[];
  isLoading?: boolean;
  className?: string;
}

/**
 * MessageList component for displaying conversation history
 * Handles auto-scrolling, typing indicators, and empty states
 */
export const MessageList: React.FC<MessageListProps> = ({ 
  messages, 
  isLoading = false, 
  className 
}) => {
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  // Auto-scroll to bottom when new messages are added
  useEffect(() => {
    if (messagesEndRef.current && messagesEndRef.current.scrollIntoView) {
      messagesEndRef.current.scrollIntoView({ 
        behavior: 'smooth',
        block: 'end'
      });
    }
  }, [messages, isLoading]);

  // Combine CSS classes
  const containerClasses = [
    styles['messageList'],
    className
  ].filter(Boolean).join(' ');

  // Render empty state when no messages exist
  if (messages.length === 0 && !isLoading) {
    return (
      <div className={containerClasses} data-testid="message-list">
        <div className={styles['emptyState']} data-testid="empty-state">
          <div className={styles['emptyStateIcon']}>💬</div>
          <div className={styles['emptyStateText']}>
            Start a conversation with the AI assistant
          </div>
          <div className={styles['emptyStateSubtext']}>
            Type your message below to begin
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className={containerClasses} ref={containerRef} data-testid="message-list">
      <div className={styles['messagesContainer']} data-testid="messages-container">
        {messages.map((message) => (
          <Message 
            key={message.id} 
            message={message}
            className={styles['messageItem']}
          />
        ))}
        
        {/* Typing indicator during API calls */}
        {isLoading && (
          <div className={styles['typingIndicator']} data-testid="typing-indicator">
            <div className={styles['typingIndicatorContent']}>
              <div className={styles['typingDots']} data-testid="typing-dots">
                <span className={styles['dot']} data-testid="typing-dot"></span>
                <span className={styles['dot']} data-testid="typing-dot"></span>
                <span className={styles['dot']} data-testid="typing-dot"></span>
              </div>
              <span className={styles['typingText']}>AI is typing...</span>
            </div>
          </div>
        )}
        
        {/* Invisible element for auto-scroll target */}
        <div ref={messagesEndRef} />
      </div>
    </div>
  );
};

export default MessageList;
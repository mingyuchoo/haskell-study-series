import React from 'react';
import type { Message as MessageType } from '../types/chat';
import styles from './Message.module.css';

/**
 * Props interface for the Message component
 */
export interface MessageProps {
  message: MessageType;
  className?: string;
}

/**
 * Message component for displaying individual chat messages
 * Provides visual distinction between user and assistant messages
 */
export const Message: React.FC<MessageProps> = ({ message, className }) => {
  const { content, role, timestamp } = message;
  
  // Format timestamp for display
  const formatTimestamp = (date: Date): string => {
    return date.toLocaleTimeString([], { 
      hour: '2-digit', 
      minute: '2-digit',
      hour12: false 
    });
  };

  // Combine CSS classes based on message role
  const messageClasses = [
    styles['message'],
    styles[`message--${role}`],
    className
  ].filter(Boolean).join(' ');

  return (
    <div className={messageClasses}>
      <div className={styles['messageContent']}>
        <div className={styles['messageText']}>
          {content}
        </div>
        <div className={styles['messageTimestamp']}>
          {formatTimestamp(timestamp)}
        </div>
      </div>
    </div>
  );
};

export default Message;
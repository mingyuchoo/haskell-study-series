import React from 'react';
import type { ApiError } from '../types/chat';
import styles from './StatusIndicator.module.css';

/**
 * Connection status types
 */
export type ConnectionStatus = 'connected' | 'disconnected' | 'connecting';

/**
 * Props interface for the StatusIndicator component
 */
export interface StatusIndicatorProps {
  /** Current connection status */
  connectionStatus: ConnectionStatus;
  /** Whether the AI is currently typing/processing */
  isTyping?: boolean;
  /** Current error state, if any */
  error?: ApiError | null;
  /** Additional CSS class name */
  className?: string;
}

/**
 * StatusIndicator component for showing connection status and activity
 * Displays connection status, typing indicators, and error states
 */
export const StatusIndicator: React.FC<StatusIndicatorProps> = ({
  connectionStatus,
  isTyping = false,
  error = null,
  className
}) => {
  /**
   * Get the appropriate status message based on current state
   */
  const getStatusMessage = (): string => {
    if (error) {
      switch (error.type) {
        case 'NETWORK_ERROR':
          return 'Connection lost - Check your internet connection';
        case 'SERVER_ERROR':
          return 'Server error - Please try again';
        case 'TIMEOUT_ERROR':
          return 'Request timed out - Please try again';
        case 'VALIDATION_ERROR':
          return 'Invalid request - Please check your input';
        default:
          return 'An error occurred - Please try again';
      }
    }

    if (isTyping) {
      return 'AI is typing...';
    }

    switch (connectionStatus) {
      case 'connected':
        return 'Connected';
      case 'connecting':
        return 'Connecting...';
      case 'disconnected':
        return 'Disconnected';
      default:
        return 'Unknown status';
    }
  };

  /**
   * Get the appropriate CSS class for the current state
   */
  const getStatusClass = (): string => {
    if (error) {
      return styles['status--error'];
    }

    if (isTyping) {
      return styles['status--typing'];
    }

    return styles[`status--${connectionStatus}`];
  };

  /**
   * Get the appropriate icon for the current state
   */
  const getStatusIcon = (): React.ReactNode => {
    if (error) {
      return <span className={styles.errorIcon} aria-hidden="true">⚠️</span>;
    }

    if (isTyping) {
      return (
        <div className={styles.typingIndicator} aria-hidden="true">
          <span className={styles.dot}></span>
          <span className={styles.dot}></span>
          <span className={styles.dot}></span>
        </div>
      );
    }

    switch (connectionStatus) {
      case 'connected':
        return <span className={styles.connectedIcon} aria-hidden="true">●</span>;
      case 'connecting':
        return <span className={styles.connectingSpinner} aria-hidden="true"></span>;
      case 'disconnected':
        return <span className={styles.disconnectedIcon} aria-hidden="true">●</span>;
      default:
        return null;
    }
  };

  // Combine CSS classes
  const containerClasses = [
    styles.statusIndicator,
    getStatusClass(),
    className
  ].filter(Boolean).join(' ');

  return (
    <div 
      className={containerClasses}
      role="status"
      aria-live="polite"
      aria-label={`Connection status: ${getStatusMessage()}`}
    >
      <div className={styles.statusContent}>
        <div className={styles.iconContainer}>
          {getStatusIcon()}
        </div>
        <span className={styles.statusText}>
          {getStatusMessage()}
        </span>
      </div>
    </div>
  );
};

export default StatusIndicator;
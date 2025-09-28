import React, { useCallback } from 'react';
import { ApiError, ErrorType } from '../types/chat';
import styles from './ErrorDisplay.module.css';

/**
 * Props interface for the ErrorDisplay component
 */
export interface ErrorDisplayProps {
  /** The error to display */
  error: ApiError;
  /** Callback when user clicks retry button */
  onRetry?: () => void;
  /** Callback when user dismisses the error */
  onDismiss?: () => void;
  /** Whether retry functionality is available */
  canRetry?: boolean;
  /** Additional CSS class name */
  className?: string;
}

/**
 * ErrorDisplay component for showing detailed error information
 * Provides user-friendly error messages and retry functionality
 */
export const ErrorDisplay: React.FC<ErrorDisplayProps> = ({
  error,
  onRetry,
  onDismiss,
  canRetry = true,
  className
}) => {
  /**
   * Get user-friendly error message based on error type
   */
  const getUserFriendlyMessage = useCallback((error: ApiError): string => {
    switch (error.type) {
      case ErrorType.NETWORK_ERROR:
        return 'Unable to connect to the server. Please check your internet connection and try again.';
      case ErrorType.SERVER_ERROR:
        return 'The server encountered an error. Please try again in a moment.';
      case ErrorType.TIMEOUT_ERROR:
        return 'The request took too long to complete. Please try again.';
      case ErrorType.VALIDATION_ERROR:
        return 'There was an issue with your request. Please check your input and try again.';
      case ErrorType.UNKNOWN_ERROR:
      default:
        return 'An unexpected error occurred. Please try again.';
    }
  }, []);

  /**
   * Get error icon based on error type
   */
  const getErrorIcon = useCallback((errorType: ErrorType): string => {
    switch (errorType) {
      case ErrorType.NETWORK_ERROR:
        return '🌐';
      case ErrorType.SERVER_ERROR:
        return '🔧';
      case ErrorType.TIMEOUT_ERROR:
        return '⏱️';
      case ErrorType.VALIDATION_ERROR:
        return '⚠️';
      case ErrorType.UNKNOWN_ERROR:
      default:
        return '❌';
    }
  }, []);

  /**
   * Get error severity class
   */
  const getErrorSeverityClass = useCallback((errorType: ErrorType): string => {
    switch (errorType) {
      case ErrorType.NETWORK_ERROR:
      case ErrorType.TIMEOUT_ERROR:
        return styles['error--warning'];
      case ErrorType.SERVER_ERROR:
      case ErrorType.UNKNOWN_ERROR:
        return styles['error--critical'];
      case ErrorType.VALIDATION_ERROR:
        return styles['error--info'];
      default:
        return styles['error--critical'];
    }
  }, []);

  /**
   * Handle retry button click
   */
  const handleRetry = useCallback(() => {
    if (onRetry) {
      onRetry();
    }
  }, [onRetry]);

  /**
   * Handle dismiss button click
   */
  const handleDismiss = useCallback(() => {
    if (onDismiss) {
      onDismiss();
    }
  }, [onDismiss]);

  // Combine CSS classes
  const containerClasses = [
    styles.errorDisplay,
    getErrorSeverityClass(error.type),
    className
  ].filter(Boolean).join(' ');

  const userMessage = getUserFriendlyMessage(error);
  const errorIcon = getErrorIcon(error.type);

  return (
    <div 
      className={containerClasses}
      role="alert"
      aria-live="assertive"
      data-testid="error-display"
      data-error-type={error.type}
    >
      <div className={styles.errorContent}>
        <div className={styles.errorHeader}>
          <span className={styles.errorIcon} aria-hidden="true">
            {errorIcon}
          </span>
          <span className={styles.errorTitle}>
            {error.type === ErrorType.NETWORK_ERROR ? 'Connection Error' :
             error.type === ErrorType.SERVER_ERROR ? 'Server Error' :
             error.type === ErrorType.TIMEOUT_ERROR ? 'Timeout Error' :
             error.type === ErrorType.VALIDATION_ERROR ? 'Input Error' :
             'Error'}
          </span>
        </div>
        
        <div className={styles.errorMessage}>
          {userMessage}
        </div>

        {/* Show technical details in development or if available */}
        {(process.env.NODE_ENV === 'development' || error.details) && (
          <details className={styles.errorDetails}>
            <summary className={styles.errorDetailsSummary}>
              Technical Details
            </summary>
            <div className={styles.errorDetailsContent}>
              <p><strong>Type:</strong> {error.type}</p>
              <p><strong>Message:</strong> {error.message}</p>
              {error.statusCode && (
                <p><strong>Status Code:</strong> {error.statusCode}</p>
              )}
              {error.details && (
                <p><strong>Details:</strong> {error.details}</p>
              )}
            </div>
          </details>
        )}

        <div className={styles.errorActions}>
          {canRetry && onRetry && (
            <button
              className={styles.retryButton}
              onClick={handleRetry}
              data-testid="error-retry-button"
              aria-label="Retry the failed operation"
            >
              🔄 Retry
            </button>
          )}
          
          {onDismiss && (
            <button
              className={styles.dismissButton}
              onClick={handleDismiss}
              data-testid="error-dismiss-button"
              aria-label="Dismiss this error"
            >
              ✕ Dismiss
            </button>
          )}
        </div>
      </div>
    </div>
  );
};

export default ErrorDisplay;
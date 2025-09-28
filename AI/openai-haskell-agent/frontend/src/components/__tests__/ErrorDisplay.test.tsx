import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { ErrorDisplay } from '../ErrorDisplay';
import { ErrorType, type ApiError } from '../../types/chat';

describe('ErrorDisplay', () => {
  const mockOnRetry = vi.fn();
  const mockOnDismiss = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Error Message Display', () => {
    it('displays network error with appropriate message and icon', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network connection failed'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Connection Error')).toBeInTheDocument();
      expect(screen.getByText(/Unable to connect to the server/)).toBeInTheDocument();
      expect(screen.getByText('🌐')).toBeInTheDocument();
      expect(screen.getByTestId('error-display')).toHaveAttribute('data-error-type', 'NETWORK_ERROR');
    });

    it('displays server error with appropriate message and icon', () => {
      const error: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Internal server error',
        statusCode: 500
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Server Error')).toBeInTheDocument();
      expect(screen.getByText(/The server encountered an error/)).toBeInTheDocument();
      expect(screen.getByText('🔧')).toBeInTheDocument();
    });

    it('displays timeout error with appropriate message and icon', () => {
      const error: ApiError = {
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Timeout Error')).toBeInTheDocument();
      expect(screen.getByText(/The request took too long/)).toBeInTheDocument();
      expect(screen.getByText('⏱️')).toBeInTheDocument();
    });

    it('displays validation error with appropriate message and icon', () => {
      const error: ApiError = {
        type: ErrorType.VALIDATION_ERROR,
        message: 'Invalid input provided'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Input Error')).toBeInTheDocument();
      expect(screen.getByText(/There was an issue with your request/)).toBeInTheDocument();
      expect(screen.getByText('⚠️')).toBeInTheDocument();
    });

    it('displays unknown error with default message and icon', () => {
      const error: ApiError = {
        type: ErrorType.UNKNOWN_ERROR,
        message: 'Something went wrong'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Error')).toBeInTheDocument();
      expect(screen.getByText(/An unexpected error occurred/)).toBeInTheDocument();
      expect(screen.getByText('❌')).toBeInTheDocument();
    });
  });

  describe('Retry Functionality', () => {
    it('shows retry button when onRetry is provided and canRetry is true', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(
        <ErrorDisplay 
          error={error} 
          onRetry={mockOnRetry} 
          canRetry={true} 
        />
      );

      const retryButton = screen.getByTestId('error-retry-button');
      expect(retryButton).toBeInTheDocument();
      expect(retryButton).toHaveTextContent('🔄 Retry');
    });

    it('hides retry button when canRetry is false', () => {
      const error: ApiError = {
        type: ErrorType.VALIDATION_ERROR,
        message: 'Validation error'
      };

      render(
        <ErrorDisplay 
          error={error} 
          onRetry={mockOnRetry} 
          canRetry={false} 
        />
      );

      expect(screen.queryByTestId('error-retry-button')).not.toBeInTheDocument();
    });

    it('hides retry button when onRetry is not provided', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} canRetry={true} />);

      expect(screen.queryByTestId('error-retry-button')).not.toBeInTheDocument();
    });

    it('calls onRetry when retry button is clicked', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(
        <ErrorDisplay 
          error={error} 
          onRetry={mockOnRetry} 
          canRetry={true} 
        />
      );

      fireEvent.click(screen.getByTestId('error-retry-button'));
      expect(mockOnRetry).toHaveBeenCalledTimes(1);
    });
  });

  describe('Dismiss Functionality', () => {
    it('shows dismiss button when onDismiss is provided', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} onDismiss={mockOnDismiss} />);

      const dismissButton = screen.getByTestId('error-dismiss-button');
      expect(dismissButton).toBeInTheDocument();
      expect(dismissButton).toHaveTextContent('✕ Dismiss');
    });

    it('hides dismiss button when onDismiss is not provided', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.queryByTestId('error-dismiss-button')).not.toBeInTheDocument();
    });

    it('calls onDismiss when dismiss button is clicked', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} onDismiss={mockOnDismiss} />);

      fireEvent.click(screen.getByTestId('error-dismiss-button'));
      expect(mockOnDismiss).toHaveBeenCalledTimes(1);
    });
  });

  describe('Technical Details', () => {
    it('shows technical details in development mode', () => {
      const originalEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'development';

      const error: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Internal server error',
        statusCode: 500,
        details: 'Database connection failed'
      };

      render(<ErrorDisplay error={error} />);

      const detailsElement = screen.getByText('Technical Details');
      expect(detailsElement).toBeInTheDocument();

      // Click to expand details
      fireEvent.click(detailsElement);

      expect(screen.getByText('Type:')).toBeInTheDocument();
      expect(screen.getByText('SERVER_ERROR')).toBeInTheDocument();
      expect(screen.getByText('Message:')).toBeInTheDocument();
      expect(screen.getByText('Internal server error')).toBeInTheDocument();
      expect(screen.getByText('Status Code:')).toBeInTheDocument();
      expect(screen.getByText('500')).toBeInTheDocument();
      expect(screen.getByText('Details:')).toBeInTheDocument();
      expect(screen.getByText('Database connection failed')).toBeInTheDocument();

      process.env.NODE_ENV = originalEnv;
    });

    it('shows technical details when error has details property', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Connection failed',
        details: 'Timeout after 10 seconds'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.getByText('Technical Details')).toBeInTheDocument();
    });

    it('hides technical details in production without details', () => {
      const originalEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'production';

      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Connection failed'
      };

      render(<ErrorDisplay error={error} />);

      expect(screen.queryByText('Technical Details')).not.toBeInTheDocument();

      process.env.NODE_ENV = originalEnv;
    });
  });

  describe('CSS Classes and Styling', () => {
    it('applies warning severity class for network errors', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} />);

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveClass('error--warning');
    });

    it('applies critical severity class for server errors', () => {
      const error: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Server error'
      };

      render(<ErrorDisplay error={error} />);

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveClass('error--critical');
    });

    it('applies info severity class for validation errors', () => {
      const error: ApiError = {
        type: ErrorType.VALIDATION_ERROR,
        message: 'Validation error'
      };

      render(<ErrorDisplay error={error} />);

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveClass('error--info');
    });

    it('applies custom className when provided', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} className="custom-error" />);

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveClass('custom-error');
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA attributes', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} />);

      const errorDisplay = screen.getByTestId('error-display');
      expect(errorDisplay).toHaveAttribute('role', 'alert');
      expect(errorDisplay).toHaveAttribute('aria-live', 'assertive');
    });

    it('has proper aria-label for retry button', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(
        <ErrorDisplay 
          error={error} 
          onRetry={mockOnRetry} 
          canRetry={true} 
        />
      );

      const retryButton = screen.getByTestId('error-retry-button');
      expect(retryButton).toHaveAttribute('aria-label', 'Retry the failed operation');
    });

    it('has proper aria-label for dismiss button', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };

      render(<ErrorDisplay error={error} onDismiss={mockOnDismiss} />);

      const dismissButton = screen.getByTestId('error-dismiss-button');
      expect(dismissButton).toHaveAttribute('aria-label', 'Dismiss this error');
    });
  });
});
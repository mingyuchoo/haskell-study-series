import React from 'react';
import { render, screen } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import { StatusIndicator, type StatusIndicatorProps, type ConnectionStatus } from '../StatusIndicator';
import { ErrorType, type ApiError } from '../../types/chat';

// Helper function to create test props
const createProps = (overrides: Partial<StatusIndicatorProps> = {}): StatusIndicatorProps => ({
  connectionStatus: 'connected',
  isTyping: false,
  error: null,
  ...overrides,
});

describe('StatusIndicator', () => {
  describe('Connection Status Display', () => {
    it('should display connected status correctly', () => {
      render(<StatusIndicator {...createProps({ connectionStatus: 'connected' })} />);
      
      expect(screen.getByRole('status')).toBeInTheDocument();
      expect(screen.getByText('Connected')).toBeInTheDocument();
      expect(screen.getByLabelText('Connection status: Connected')).toBeInTheDocument();
    });

    it('should display disconnected status correctly', () => {
      render(<StatusIndicator {...createProps({ connectionStatus: 'disconnected' })} />);
      
      expect(screen.getByText('Disconnected')).toBeInTheDocument();
      expect(screen.getByLabelText('Connection status: Disconnected')).toBeInTheDocument();
    });

    it('should display connecting status correctly', () => {
      render(<StatusIndicator {...createProps({ connectionStatus: 'connecting' })} />);
      
      expect(screen.getByText('Connecting...')).toBeInTheDocument();
      expect(screen.getByLabelText('Connection status: Connecting...')).toBeInTheDocument();
    });

    it('should handle unknown connection status gracefully', () => {
      render(<StatusIndicator {...createProps({ connectionStatus: 'unknown' as ConnectionStatus })} />);
      
      expect(screen.getByText('Unknown status')).toBeInTheDocument();
    });
  });

  describe('Typing Indicator', () => {
    it('should display typing indicator when isTyping is true', () => {
      render(<StatusIndicator {...createProps({ isTyping: true })} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
      expect(screen.getByLabelText('Connection status: AI is typing...')).toBeInTheDocument();
    });

    it('should not display typing indicator when isTyping is false', () => {
      render(<StatusIndicator {...createProps({ isTyping: false })} />);
      
      expect(screen.queryByText('AI is typing...')).not.toBeInTheDocument();
    });

    it('should prioritize typing indicator over connection status', () => {
      render(<StatusIndicator {...createProps({ 
        connectionStatus: 'connected',
        isTyping: true 
      })} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
      expect(screen.queryByText('Connected')).not.toBeInTheDocument();
    });
  });

  describe('Error States', () => {
    it('should display network error correctly', () => {
      const networkError: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network connection failed'
      };
      
      render(<StatusIndicator {...createProps({ error: networkError })} />);
      
      expect(screen.getByText('Connection lost - Check your internet connection')).toBeInTheDocument();
      expect(screen.getByLabelText('Connection status: Connection lost - Check your internet connection')).toBeInTheDocument();
    });

    it('should display server error correctly', () => {
      const serverError: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Internal server error',
        statusCode: 500
      };
      
      render(<StatusIndicator {...createProps({ error: serverError })} />);
      
      expect(screen.getByText('Server error - Please try again')).toBeInTheDocument();
    });

    it('should display timeout error correctly', () => {
      const timeoutError: ApiError = {
        type: ErrorType.TIMEOUT_ERROR,
        message: 'Request timed out'
      };
      
      render(<StatusIndicator {...createProps({ error: timeoutError })} />);
      
      expect(screen.getByText('Request timed out - Please try again')).toBeInTheDocument();
    });

    it('should display validation error correctly', () => {
      const validationError: ApiError = {
        type: ErrorType.VALIDATION_ERROR,
        message: 'Invalid input provided'
      };
      
      render(<StatusIndicator {...createProps({ error: validationError })} />);
      
      expect(screen.getByText('Invalid request - Please check your input')).toBeInTheDocument();
    });

    it('should display unknown error correctly', () => {
      const unknownError: ApiError = {
        type: ErrorType.UNKNOWN_ERROR,
        message: 'Something went wrong'
      };
      
      render(<StatusIndicator {...createProps({ error: unknownError })} />);
      
      expect(screen.getByText('An error occurred - Please try again')).toBeInTheDocument();
    });

    it('should prioritize error over typing indicator and connection status', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };
      
      render(<StatusIndicator {...createProps({ 
        connectionStatus: 'connected',
        isTyping: true,
        error 
      })} />);
      
      expect(screen.getByText('Connection lost - Check your internet connection')).toBeInTheDocument();
      expect(screen.queryByText('AI is typing...')).not.toBeInTheDocument();
      expect(screen.queryByText('Connected')).not.toBeInTheDocument();
    });
  });

  describe('CSS Classes', () => {
    it('should apply correct CSS class for connected status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'connected' })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement?.className).toContain('status--connected');
    });

    it('should apply correct CSS class for disconnected status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'disconnected' })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement?.className).toContain('status--disconnected');
    });

    it('should apply correct CSS class for connecting status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'connecting' })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement?.className).toContain('status--connecting');
    });

    it('should apply correct CSS class for typing state', () => {
      const { container } = render(<StatusIndicator {...createProps({ isTyping: true })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement?.className).toContain('status--typing');
    });

    it('should apply correct CSS class for error state', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };
      
      const { container } = render(<StatusIndicator {...createProps({ error })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement?.className).toContain('status--error');
    });

    it('should apply custom className when provided', () => {
      const { container } = render(<StatusIndicator {...createProps({ className: 'custom-class' })} />);
      const statusElement = container.querySelector('[role="status"]');
      
      expect(statusElement).toHaveClass('custom-class');
    });
  });

  describe('Accessibility', () => {
    it('should have proper ARIA attributes', () => {
      render(<StatusIndicator {...createProps()} />);
      
      const statusElement = screen.getByRole('status');
      expect(statusElement).toHaveAttribute('aria-live', 'polite');
      expect(statusElement).toHaveAttribute('aria-label');
    });

    it('should update aria-label based on current state', () => {
      const { rerender } = render(<StatusIndicator {...createProps({ connectionStatus: 'connected' })} />);
      
      expect(screen.getByLabelText('Connection status: Connected')).toBeInTheDocument();
      
      rerender(<StatusIndicator {...createProps({ connectionStatus: 'disconnected' })} />);
      expect(screen.getByLabelText('Connection status: Disconnected')).toBeInTheDocument();
    });

    it('should have aria-hidden on decorative icons', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'connected' })} />);
      
      // Check that icons have aria-hidden
      const icons = container.querySelectorAll('[aria-hidden="true"]');
      expect(icons.length).toBeGreaterThan(0);
    });
  });

  describe('Visual Indicators', () => {
    it('should render connected icon for connected status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'connected' })} />);
      
      expect(container.querySelector('[class*="connectedIcon"]')).toBeInTheDocument();
    });

    it('should render disconnected icon for disconnected status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'disconnected' })} />);
      
      expect(container.querySelector('[class*="disconnectedIcon"]')).toBeInTheDocument();
    });

    it('should render connecting spinner for connecting status', () => {
      const { container } = render(<StatusIndicator {...createProps({ connectionStatus: 'connecting' })} />);
      
      expect(container.querySelector('[class*="connectingSpinner"]')).toBeInTheDocument();
    });

    it('should render typing indicator dots when typing', () => {
      const { container } = render(<StatusIndicator {...createProps({ isTyping: true })} />);
      
      expect(container.querySelector('[class*="typingIndicator"]')).toBeInTheDocument();
      expect(container.querySelectorAll('[class*="dot"]')).toHaveLength(3);
    });

    it('should render error icon for error states', () => {
      const error: ApiError = {
        type: ErrorType.NETWORK_ERROR,
        message: 'Network error'
      };
      
      const { container } = render(<StatusIndicator {...createProps({ error })} />);
      
      expect(container.querySelector('[class*="errorIcon"]')).toBeInTheDocument();
    });
  });

  describe('State Priority', () => {
    it('should prioritize error over all other states', () => {
      const error: ApiError = {
        type: ErrorType.SERVER_ERROR,
        message: 'Server error'
      };
      
      render(<StatusIndicator {...createProps({ 
        connectionStatus: 'connected',
        isTyping: true,
        error 
      })} />);
      
      expect(screen.getByText('Server error - Please try again')).toBeInTheDocument();
    });

    it('should prioritize typing over connection status', () => {
      render(<StatusIndicator {...createProps({ 
        connectionStatus: 'disconnected',
        isTyping: true 
      })} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
      expect(screen.queryByText('Disconnected')).not.toBeInTheDocument();
    });

    it('should show connection status when no error or typing', () => {
      render(<StatusIndicator {...createProps({ 
        connectionStatus: 'connected',
        isTyping: false,
        error: null 
      })} />);
      
      expect(screen.getByText('Connected')).toBeInTheDocument();
    });
  });
});
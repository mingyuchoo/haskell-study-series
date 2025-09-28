import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { MessageInput, type MessageInputProps } from '../MessageInput';

describe('MessageInput', () => {
  const defaultProps: MessageInputProps = {
    onSendMessage: vi.fn(),
    disabled: false,
    placeholder: 'Type your message...',
    maxLength: 1000,
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with default props', () => {
      render(<MessageInput {...defaultProps} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      expect(input).toBeInTheDocument();
      expect(sendButton).toBeInTheDocument();
      expect(input).toHaveAttribute('placeholder', 'Type your message...');
      expect(input).toHaveAttribute('maxlength', '1000');
    });

    it('renders with custom placeholder', () => {
      render(<MessageInput {...defaultProps} placeholder="Custom placeholder" />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      expect(input).toHaveAttribute('placeholder', 'Custom placeholder');
    });

    it('renders with custom maxLength', () => {
      render(<MessageInput {...defaultProps} maxLength={500} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      expect(input).toHaveAttribute('maxlength', '500');
    });

    it('shows loading state when disabled', () => {
      render(<MessageInput {...defaultProps} disabled={true} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      const statusMessage = screen.getByText('Processing message...');
      
      expect(input).toBeDisabled();
      expect(input).toHaveAttribute('placeholder', 'Connecting...');
      expect(sendButton).toBeDisabled();
      expect(statusMessage).toBeInTheDocument();
    });
  });

  describe('User Input', () => {
    it('updates input value when user types', async () => {
      const user = userEvent.setup();
      render(<MessageInput {...defaultProps} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      await user.type(input, 'Hello world');
      
      expect(input).toHaveValue('Hello world');
    });

    it('clears input after sending message', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      await user.type(input, 'Test message');
      await user.click(sendButton);
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
      expect(input).toHaveValue('');
    });

    it('trims whitespace from messages', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      await user.type(input, '  Test message  ');
      await user.click(sendButton);
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
    });
  });

  describe('Send Button Behavior', () => {
    it('sends message when send button is clicked', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      await user.type(input, 'Test message');
      await user.click(sendButton);
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
      expect(mockOnSendMessage).toHaveBeenCalledTimes(1);
    });

    it('disables send button when input is empty', () => {
      render(<MessageInput {...defaultProps} />);
      
      const sendButton = screen.getByRole('button', { name: /send message/i });
      expect(sendButton).toBeDisabled();
    });

    it('enables send button when input has content', async () => {
      const user = userEvent.setup();
      render(<MessageInput {...defaultProps} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      expect(sendButton).toBeDisabled();
      
      await user.type(input, 'Test');
      
      expect(sendButton).not.toBeDisabled();
    });

    it('disables send button when only whitespace is entered', async () => {
      const user = userEvent.setup();
      render(<MessageInput {...defaultProps} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      await user.type(input, '   ');
      
      expect(sendButton).toBeDisabled();
    });
  });

  describe('Enter Key Submission', () => {
    it('sends message when Enter key is pressed', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      await user.type(input, 'Test message');
      await user.keyboard('{Enter}');
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
      expect(input).toHaveValue('');
    });

    it('does not send message when Shift+Enter is pressed', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      await user.type(input, 'Test message');
      await user.keyboard('{Shift>}{Enter}{/Shift}');
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
      // The value should still contain the message plus a newline from Shift+Enter
      expect(input.value).toContain('Test message');
    });

    it('does not send empty message when Enter is pressed', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      await user.click(input);
      await user.keyboard('{Enter}');
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
    });
  });

  describe('Form Submission', () => {
    it('sends message when form is submitted', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const form = input.closest('form');
      
      await user.type(input, 'Test message');
      
      if (form) {
        fireEvent.submit(form);
      }
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
    });

    it('prevents default form submission behavior', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const form = input.closest('form');
      
      await user.type(input, 'Test message');
      
      const submitEvent = new Event('submit', { bubbles: true, cancelable: true });
      const preventDefaultSpy = vi.spyOn(submitEvent, 'preventDefault');
      
      if (form) {
        form.dispatchEvent(submitEvent);
      }
      
      expect(preventDefaultSpy).toHaveBeenCalled();
    });
  });

  describe('Input Validation', () => {
    it('does not send empty messages', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      // Try to click send button without typing anything
      await user.click(sendButton);
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
    });

    it('does not send whitespace-only messages', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      await user.type(input, '   \n\t  ');
      await user.click(sendButton);
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
    });

    it('respects maxLength attribute', async () => {
      const user = userEvent.setup();
      render(<MessageInput {...defaultProps} maxLength={10} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      await user.type(input, 'This is a very long message that exceeds the limit');
      
      expect(input).toHaveValue('This is a ');
    });
  });

  describe('Disabled State', () => {
    it('does not send message when disabled', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} disabled={true} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      // Input should be disabled, but let's try anyway
      expect(input).toBeDisabled();
      expect(sendButton).toBeDisabled();
      
      // Try to interact with disabled elements
      await user.click(sendButton);
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
    });

    it('shows loading spinner when disabled', () => {
      render(<MessageInput {...defaultProps} disabled={true} />);
      
      const sendButton = screen.getByRole('button', { name: /send message/i });
      const spinner = sendButton.querySelector('span[aria-hidden="true"]');
      expect(spinner).toBeInTheDocument();
    });

    it('does not respond to Enter key when disabled', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} disabled={true} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      
      // Try to press Enter on disabled input
      await user.click(input);
      await user.keyboard('{Enter}');
      
      expect(mockOnSendMessage).not.toHaveBeenCalled();
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA labels', () => {
      render(<MessageInput {...defaultProps} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      expect(input).toHaveAttribute('aria-label', 'Message input');
      expect(sendButton).toHaveAttribute('aria-label', 'Send message');
      expect(sendButton).toHaveAttribute('id', 'send-button');
      expect(input).toHaveAttribute('aria-describedby', 'send-button');
    });

    it('has live region for status updates', () => {
      render(<MessageInput {...defaultProps} disabled={true} />);
      
      const statusMessage = screen.getByText('Processing message...');
      expect(statusMessage).toHaveAttribute('aria-live', 'polite');
    });

    it('supports keyboard navigation', async () => {
      const user = userEvent.setup();
      const mockOnSendMessage = vi.fn();
      render(<MessageInput {...defaultProps} onSendMessage={mockOnSendMessage} />);
      
      const input = screen.getByRole('textbox', { name: /message input/i });
      const sendButton = screen.getByRole('button', { name: /send message/i });
      
      // Tab to input
      await user.tab();
      expect(input).toHaveFocus();
      
      // Type message
      await user.type(input, 'Test message');
      
      // Tab to send button
      await user.tab();
      expect(sendButton).toHaveFocus();
      
      // Press Enter to send
      await user.keyboard('{Enter}');
      
      expect(mockOnSendMessage).toHaveBeenCalledWith('Test message');
    });
  });
});
import { render, screen } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import { Message } from '../Message';
import type { Message as MessageType } from '../../types/chat';
import styles from '../Message.module.css';

describe('Message Component', () => {
  // Helper function to format timestamp like the component does
  const formatTimestamp = (date: Date): string => {
    return date.toLocaleTimeString([], { 
      hour: '2-digit', 
      minute: '2-digit',
      hour12: false 
    });
  };

  const mockUserMessage: MessageType = {
    id: 'user-1',
    content: 'Hello, how are you?',
    role: 'user',
    timestamp: new Date('2024-01-01T10:30:00Z')
  };

  const mockAssistantMessage: MessageType = {
    id: 'assistant-1',
    content: 'I am doing well, thank you for asking! How can I help you today?',
    role: 'assistant',
    timestamp: new Date('2024-01-01T10:30:15Z')
  };

  describe('Basic Rendering', () => {
    it('renders user message correctly', () => {
      render(<Message message={mockUserMessage} />);
      
      expect(screen.getByText('Hello, how are you?')).toBeInTheDocument();
      expect(screen.getByText(formatTimestamp(mockUserMessage.timestamp))).toBeInTheDocument();
    });

    it('renders assistant message correctly', () => {
      render(<Message message={mockAssistantMessage} />);
      
      expect(screen.getByText('I am doing well, thank you for asking! How can I help you today?')).toBeInTheDocument();
      expect(screen.getByText(formatTimestamp(mockAssistantMessage.timestamp))).toBeInTheDocument();
    });

    it('applies correct CSS classes for user messages', () => {
      const { container } = render(<Message message={mockUserMessage} />);
      const messageElement = container.firstChild as HTMLElement;
      
      expect(messageElement).toHaveClass(styles['message']);
      expect(messageElement).toHaveClass(styles['message--user']);
    });

    it('applies correct CSS classes for assistant messages', () => {
      const { container } = render(<Message message={mockAssistantMessage} />);
      const messageElement = container.firstChild as HTMLElement;
      
      expect(messageElement).toHaveClass(styles['message']);
      expect(messageElement).toHaveClass(styles['message--assistant']);
    });
  });

  describe('Timestamp Formatting', () => {
    it('formats timestamp correctly in 24-hour format', () => {
      const message: MessageType = {
        id: 'test-1',
        content: 'Test message',
        role: 'user',
        timestamp: new Date('2024-01-01T14:45:30Z')
      };

      render(<Message message={message} />);
      expect(screen.getByText(formatTimestamp(message.timestamp))).toBeInTheDocument();
    });

    it('formats timestamp correctly for single digit hours and minutes', () => {
      const message: MessageType = {
        id: 'test-2',
        content: 'Test message',
        role: 'user',
        timestamp: new Date('2024-01-01T09:05:00Z')
      };

      render(<Message message={message} />);
      expect(screen.getByText(formatTimestamp(message.timestamp))).toBeInTheDocument();
    });
  });

  describe('Content Handling', () => {
    it('handles multiline content correctly', () => {
      const multilineMessage: MessageType = {
        id: 'multiline-1',
        content: 'Line 1\nLine 2\nLine 3',
        role: 'assistant',
        timestamp: new Date('2024-01-01T10:30:00Z')
      };

      render(<Message message={multilineMessage} />);
      // Use a more flexible text matcher for multiline content
      expect(screen.getByText((content, element) => {
        return element?.textContent === 'Line 1\nLine 2\nLine 3';
      })).toBeInTheDocument();
    });

    it('handles empty content gracefully', () => {
      const emptyMessage: MessageType = {
        id: 'empty-1',
        content: '',
        role: 'user',
        timestamp: new Date('2024-01-01T10:30:00Z')
      };

      render(<Message message={emptyMessage} />);
      expect(screen.getByText(formatTimestamp(emptyMessage.timestamp))).toBeInTheDocument();
    });

    it('handles long content without breaking layout', () => {
      const longMessage: MessageType = {
        id: 'long-1',
        content: 'This is a very long message that should wrap properly and not break the layout of the chat interface even when it contains many words and characters',
        role: 'assistant',
        timestamp: new Date('2024-01-01T10:30:00Z')
      };

      render(<Message message={longMessage} />);
      expect(screen.getByText(longMessage.content)).toBeInTheDocument();
    });
  });

  describe('Custom Props', () => {
    it('applies custom className when provided', () => {
      const { container } = render(
        <Message message={mockUserMessage} className="custom-class" />
      );
      const messageElement = container.firstChild as HTMLElement;
      
      expect(messageElement).toHaveClass('custom-class');
      expect(messageElement).toHaveClass(styles['message']);
      expect(messageElement).toHaveClass(styles['message--user']);
    });

    it('works without custom className', () => {
      const { container } = render(<Message message={mockUserMessage} />);
      const messageElement = container.firstChild as HTMLElement;
      
      expect(messageElement).toHaveClass(styles['message']);
      expect(messageElement).toHaveClass(styles['message--user']);
    });
  });

  describe('Accessibility', () => {
    it('has proper structure for screen readers', () => {
      render(<Message message={mockUserMessage} />);
      
      const messageText = screen.getByText('Hello, how are you?');
      const timestamp = screen.getByText(formatTimestamp(mockUserMessage.timestamp));
      
      expect(messageText).toBeInTheDocument();
      expect(timestamp).toBeInTheDocument();
    });

    it('maintains semantic structure', () => {
      const { container } = render(<Message message={mockUserMessage} />);
      
      // Check that the component has a proper div structure using CSS modules classes
      const messageDiv = container.querySelector(`.${styles['message']}`);
      const contentDiv = container.querySelector(`.${styles['messageContent']}`);
      const textDiv = container.querySelector(`.${styles['messageText']}`);
      const timestampDiv = container.querySelector(`.${styles['messageTimestamp']}`);
      
      expect(messageDiv).toBeInTheDocument();
      expect(contentDiv).toBeInTheDocument();
      expect(textDiv).toBeInTheDocument();
      expect(timestampDiv).toBeInTheDocument();
    });
  });

  describe('Role-based Styling', () => {
    it('applies different styles for user vs assistant messages', () => {
      const { container: userContainer } = render(<Message message={mockUserMessage} />);
      const { container: assistantContainer } = render(<Message message={mockAssistantMessage} />);
      
      const userMessage = userContainer.firstChild as HTMLElement;
      const assistantMessage = assistantContainer.firstChild as HTMLElement;
      
      expect(userMessage).toHaveClass(styles['message--user']);
      expect(assistantMessage).toHaveClass(styles['message--assistant']);
      expect(userMessage).not.toHaveClass(styles['message--assistant']);
      expect(assistantMessage).not.toHaveClass(styles['message--user']);
    });
  });
});
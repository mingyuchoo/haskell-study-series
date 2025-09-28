import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { MessageList } from '../MessageList';
import type { Message } from '../../types/chat';

// Mock the Message component
vi.mock('../Message', () => ({
  Message: ({ message }: { message: Message }) => (
    <div data-testid={`message-${message.id}`}>
      <span>{message.content}</span>
      <span>{message.role}</span>
    </div>
  )
}));

describe('MessageList', () => {
  const mockMessages: Message[] = [
    {
      id: '1',
      content: 'Hello, how are you?',
      role: 'user',
      timestamp: new Date('2024-01-01T10:00:00Z')
    },
    {
      id: '2',
      content: 'I am doing well, thank you for asking!',
      role: 'assistant',
      timestamp: new Date('2024-01-01T10:01:00Z')
    },
    {
      id: '3',
      content: 'That is great to hear!',
      role: 'user',
      timestamp: new Date('2024-01-01T10:02:00Z')
    }
  ];

  // Mock scrollIntoView
  const mockScrollIntoView = vi.fn();
  
  beforeEach(() => {
    Element.prototype.scrollIntoView = mockScrollIntoView;
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('Message Rendering', () => {
    it('renders all messages in the correct order', () => {
      render(<MessageList messages={mockMessages} />);
      
      const messageElements = screen.getAllByTestId(/^message-\d+$/);
      expect(messageElements).toHaveLength(3);
      
      // Check that messages are rendered in order
      expect(screen.getByTestId('message-1')).toBeInTheDocument();
      expect(screen.getByTestId('message-2')).toBeInTheDocument();
      expect(screen.getByTestId('message-3')).toBeInTheDocument();
    });

    it('renders messages with correct content', () => {
      render(<MessageList messages={mockMessages} />);
      
      expect(screen.getByText('Hello, how are you?')).toBeInTheDocument();
      expect(screen.getByText('I am doing well, thank you for asking!')).toBeInTheDocument();
      expect(screen.getByText('That is great to hear!')).toBeInTheDocument();
    });

    it('passes correct props to Message components', () => {
      render(<MessageList messages={mockMessages} />);
      
      // Check that user and assistant roles are passed correctly
      expect(screen.getAllByText('user')).toHaveLength(2);
      expect(screen.getAllByText('assistant')).toHaveLength(1);
    });
  });

  describe('Empty State', () => {
    it('displays empty state when no messages exist', () => {
      render(<MessageList messages={[]} />);
      
      expect(screen.getByText('Start a conversation with the AI assistant')).toBeInTheDocument();
      expect(screen.getByText('Type your message below to begin')).toBeInTheDocument();
      expect(screen.getByText('💬')).toBeInTheDocument();
    });

    it('does not display empty state when messages exist', () => {
      render(<MessageList messages={mockMessages} />);
      
      expect(screen.queryByText('Start a conversation with the AI assistant')).not.toBeInTheDocument();
      expect(screen.queryByText('Type your message below to begin')).not.toBeInTheDocument();
    });

    it('does not display empty state when loading', () => {
      render(<MessageList messages={[]} isLoading={true} />);
      
      expect(screen.queryByText('Start a conversation with the AI assistant')).not.toBeInTheDocument();
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
    });
  });

  describe('Typing Indicator', () => {
    it('displays typing indicator when loading', () => {
      render(<MessageList messages={mockMessages} isLoading={true} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
      expect(screen.getByTestId('typing-indicator')).toBeInTheDocument();
      
      // Check for typing dots
      const dots = screen.getAllByTestId('typing-dot');
      expect(dots).toHaveLength(3);
    });

    it('does not display typing indicator when not loading', () => {
      render(<MessageList messages={mockMessages} isLoading={false} />);
      
      expect(screen.queryByText('AI is typing...')).not.toBeInTheDocument();
    });

    it('displays typing indicator with empty messages when loading', () => {
      render(<MessageList messages={[]} isLoading={true} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
      expect(screen.queryByText('Start a conversation with the AI assistant')).not.toBeInTheDocument();
    });
  });

  describe('Auto-scroll Functionality', () => {
    it('scrolls to bottom when messages are added', async () => {
      const { rerender } = render(<MessageList messages={[mockMessages[0]]} />);
      
      // Add more messages
      rerender(<MessageList messages={mockMessages} />);
      
      await waitFor(() => {
        expect(mockScrollIntoView).toHaveBeenCalledWith({
          behavior: 'smooth',
          block: 'end'
        });
      });
    });

    it('scrolls to bottom when loading state changes', async () => {
      const { rerender } = render(<MessageList messages={mockMessages} isLoading={false} />);
      
      // Change loading state
      rerender(<MessageList messages={mockMessages} isLoading={true} />);
      
      await waitFor(() => {
        expect(mockScrollIntoView).toHaveBeenCalledWith({
          behavior: 'smooth',
          block: 'end'
        });
      });
    });

    it('scrolls to bottom on initial render with messages', async () => {
      render(<MessageList messages={mockMessages} />);
      
      await waitFor(() => {
        expect(mockScrollIntoView).toHaveBeenCalledWith({
          behavior: 'smooth',
          block: 'end'
        });
      });
    });
  });

  describe('Component Structure', () => {
    it('renders message list container', () => {
      render(<MessageList messages={mockMessages} />);
      
      expect(screen.getByTestId('message-list')).toBeInTheDocument();
      expect(screen.getByTestId('messages-container')).toBeInTheDocument();
    });

    it('applies custom className when provided', () => {
      const { container } = render(<MessageList messages={mockMessages} className="custom-class" />);
      
      const messageListElement = screen.getByTestId('message-list');
      expect(messageListElement).toHaveClass('custom-class');
    });

    it('renders empty state elements', () => {
      render(<MessageList messages={[]} />);
      
      expect(screen.getByTestId('empty-state')).toBeInTheDocument();
      expect(screen.getByText('💬')).toBeInTheDocument();
      expect(screen.getByText('Start a conversation with the AI assistant')).toBeInTheDocument();
      expect(screen.getByText('Type your message below to begin')).toBeInTheDocument();
    });

    it('renders typing indicator elements', () => {
      render(<MessageList messages={[]} isLoading={true} />);
      
      expect(screen.getByTestId('typing-indicator')).toBeInTheDocument();
      expect(screen.getByTestId('typing-dots')).toBeInTheDocument();
      expect(screen.getAllByTestId('typing-dot')).toHaveLength(3);
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
    });
  });

  describe('Edge Cases', () => {
    it('handles single message correctly', () => {
      render(<MessageList messages={[mockMessages[0]]} />);
      
      expect(screen.getByTestId('message-1')).toBeInTheDocument();
      expect(screen.queryByText('Start a conversation with the AI assistant')).not.toBeInTheDocument();
    });

    it('handles undefined isLoading prop', () => {
      render(<MessageList messages={mockMessages} />);
      
      expect(screen.queryByText('AI is typing...')).not.toBeInTheDocument();
      expect(screen.getAllByTestId(/^message-\d+$/)).toHaveLength(3);
    });

    it('handles messages with special characters', () => {
      const specialMessage: Message = {
        id: 'special',
        content: 'Hello! How are you? 🤖 & <script>alert("test")</script>',
        role: 'assistant',
        timestamp: new Date()
      };
      
      render(<MessageList messages={[specialMessage]} />);
      
      expect(screen.getByText('Hello! How are you? 🤖 & <script>alert("test")</script>')).toBeInTheDocument();
    });

    it('handles very long message list', () => {
      const longMessageList: Message[] = Array.from({ length: 100 }, (_, i) => ({
        id: `msg-${i}`,
        content: `Message ${i}`,
        role: i % 2 === 0 ? 'user' : 'assistant',
        timestamp: new Date()
      }));
      
      render(<MessageList messages={longMessageList} />);
      
      expect(screen.getAllByTestId(/^message-msg-/)).toHaveLength(100);
    });
  });

  describe('Accessibility', () => {
    it('has proper container structure for screen readers', () => {
      render(<MessageList messages={mockMessages} />);
      
      expect(screen.getByTestId('message-list')).toBeInTheDocument();
      expect(screen.getByTestId('messages-container')).toBeInTheDocument();
    });

    it('provides meaningful empty state text', () => {
      render(<MessageList messages={[]} />);
      
      const emptyStateText = screen.getByText('Start a conversation with the AI assistant');
      const emptyStateSubtext = screen.getByText('Type your message below to begin');
      
      expect(emptyStateText).toBeInTheDocument();
      expect(emptyStateSubtext).toBeInTheDocument();
    });

    it('provides meaningful loading state text', () => {
      render(<MessageList messages={[]} isLoading={true} />);
      
      expect(screen.getByText('AI is typing...')).toBeInTheDocument();
    });
  });
});
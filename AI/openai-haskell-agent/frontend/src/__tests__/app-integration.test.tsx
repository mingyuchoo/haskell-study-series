/**
 * Simplified integration tests focusing on core app integration
 * Tests the main user flows without complex edge cases
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import App from '../App';
import { ApiService } from '../services/ApiService';
import type { ChatOutput, HealthInfo } from '../types/api';

// Mock the ApiService
vi.mock('../services/ApiService');

// Mock environment variables
vi.mock('import.meta', () => ({
  env: {
    VITE_API_BASE_URL: 'http://localhost:8080',
    VITE_API_TIMEOUT: '10000',
    VITE_API_MAX_RETRIES: '3',
    VITE_API_RETRY_DELAY: '1000',
  }
}));

describe('App Integration Tests', () => {
  let mockApiService: {
    checkHealth: ReturnType<typeof vi.fn>;
    sendMessage: ReturnType<typeof vi.fn>;
  };

  const mockHealthResponse: HealthInfo = {
    status: 'healthy',
    version: '1.0.0',
    uptime: 12345,
    timestamp: new Date().toISOString(),
  };

  beforeEach(() => {
    vi.clearAllMocks();
    vi.clearAllTimers();
    
    mockApiService = {
      checkHealth: vi.fn(),
      sendMessage: vi.fn(),
    };

    (ApiService as any).mockImplementation(() => mockApiService);
  });

  afterEach(() => {
    vi.clearAllTimers();
  });

  it('should render the complete app structure and establish connection', async () => {
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);

    render(<App />);

    // Check main app structure
    expect(screen.getByTestId('app')).toBeInTheDocument();
    expect(screen.getByText('AI Chatbot')).toBeInTheDocument();
    expect(screen.getByTestId('chat-container')).toBeInTheDocument();

    // Wait for health check and connection
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Verify components are integrated
    expect(screen.getByTestId('message-list')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Type your message...')).toBeInTheDocument();
    expect(screen.getByTestId('connection-status')).toHaveTextContent('connected');
  });

  it('should handle a complete message exchange flow', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockResolvedValue({
      outputMessage: 'Hello! How can I help you?',
      outputSessionId: 'session-123'
    });

    render(<App />);

    // Wait for connection
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Send a message
    const messageInput = screen.getByPlaceholderText('Type your message...');
    await user.type(messageInput, 'Hello there');
    await user.keyboard('{Enter}');

    // Verify API call was made correctly
    await waitFor(() => {
      expect(mockApiService.sendMessage).toHaveBeenCalledWith({
        inputMessage: 'Hello there',
        sessionId: null
      });
    });

    // Verify response appears
    await waitFor(() => {
      expect(screen.getByText('Hello! How can I help you?')).toBeInTheDocument();
    });

    // Verify message count updated
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('2 messages');
    });
  });

  it('should handle session continuity across multiple messages', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage
      .mockResolvedValueOnce({
        outputMessage: 'First response',
        outputSessionId: 'session-123'
      })
      .mockResolvedValueOnce({
        outputMessage: 'Second response',
        outputSessionId: 'session-123'
      });

    render(<App />);

    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    const messageInput = screen.getByPlaceholderText('Type your message...');

    // Send first message
    await user.type(messageInput, 'First message');
    await user.keyboard('{Enter}');

    await waitFor(() => {
      expect(screen.getByText('First response')).toBeInTheDocument();
    });

    // Send second message - should use session ID from first response
    await user.type(messageInput, 'Second message');
    await user.keyboard('{Enter}');

    await waitFor(() => {
      expect(mockApiService.sendMessage).toHaveBeenLastCalledWith({
        inputMessage: 'Second message',
        sessionId: 'session-123'
      });
    });

    await waitFor(() => {
      expect(screen.getByText('Second response')).toBeInTheDocument();
    });
  });

  it('should handle connection failures gracefully', async () => {
    mockApiService.checkHealth.mockRejectedValue(new Error('Connection failed'));

    render(<App />);

    // Wait for disconnected state
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Disconnected');
    });

    // Verify input is disabled when disconnected
    const messageInput = screen.getByPlaceholderText('Connecting to server...');
    expect(messageInput).toBeDisabled();
  });

  it('should handle message sending errors', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockRejectedValue(new Error('Network error'));

    render(<App />);

    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    const messageInput = screen.getByPlaceholderText('Type your message...');
    await user.type(messageInput, 'This will fail');
    await user.keyboard('{Enter}');

    // Wait for error to appear
    await waitFor(() => {
      expect(screen.getByTestId('error-display')).toBeInTheDocument();
    });
  });

  it('should handle conversation clearing', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockResolvedValue({
      outputMessage: 'Test response',
      outputSessionId: 'session-123'
    });

    render(<App />);

    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Send a message first
    const messageInput = screen.getByPlaceholderText('Type your message...');
    await user.type(messageInput, 'Test message');
    await user.keyboard('{Enter}');

    await waitFor(() => {
      expect(screen.getByText('Test response')).toBeInTheDocument();
    });

    // Clear conversation
    const clearButton = screen.getByTestId('clear-conversation-button');
    await user.click(clearButton);

    const confirmButton = screen.getByTestId('confirm-clear-button');
    await user.click(confirmButton);

    // Verify conversation is cleared
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('0 messages');
    });

    expect(screen.queryByText('Test response')).not.toBeInTheDocument();
  });

  it('should maintain proper state across all components', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockResolvedValue({
      outputMessage: 'State test response',
      outputSessionId: 'session-123'
    });

    render(<App />);

    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Verify initial state
    expect(screen.getByTestId('connection-status')).toHaveTextContent('connected');
    expect(screen.getByPlaceholderText('Type your message...')).not.toBeDisabled();

    // Send message and verify state updates
    const messageInput = screen.getByPlaceholderText('Type your message...');
    await user.type(messageInput, 'State test');
    await user.keyboard('{Enter}');

    // During loading
    await waitFor(() => {
      expect(screen.getByPlaceholderText('Sending message...')).toBeInTheDocument();
      expect(screen.getByTestId('connection-status')).toHaveTextContent('connecting');
    });

    // After completion
    await waitFor(() => {
      expect(screen.getByText('State test response')).toBeInTheDocument();
      expect(screen.getByTestId('connection-status')).toHaveTextContent('connected');
      expect(screen.getByPlaceholderText('Type your message...')).not.toBeDisabled();
    });
  });
});
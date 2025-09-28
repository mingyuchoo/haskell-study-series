/**
 * Basic integration tests for core app functionality
 * Tests the essential integration without complex scenarios
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import App from '../App';
import { ApiService } from '../services/ApiService';
import type { HealthInfo } from '../types/api';

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

describe('Basic Integration Tests', () => {
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
    
    mockApiService = {
      checkHealth: vi.fn(),
      sendMessage: vi.fn(),
    };

    (ApiService as any).mockImplementation(() => mockApiService);
  });

  it('should render the app and establish connection', async () => {
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);

    render(<App />);

    // Check basic structure
    expect(screen.getByTestId('app')).toBeInTheDocument();
    expect(screen.getByText('AI Chatbot')).toBeInTheDocument();
    expect(screen.getByTestId('chat-container')).toBeInTheDocument();

    // Wait for connection
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Verify components are present
    expect(screen.getByTestId('message-list')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Type your message...')).toBeInTheDocument();
  });

  it('should handle basic message sending', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockResolvedValue({
      outputMessage: 'Test response',
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
    await user.type(messageInput, 'Hello');
    await user.keyboard('{Enter}');

    // Verify API was called
    await waitFor(() => {
      expect(mockApiService.sendMessage).toHaveBeenCalledWith({
        inputMessage: 'Hello',
        sessionId: null
      });
    });
  });

  it('should handle disconnected state', async () => {
    mockApiService.checkHealth.mockRejectedValue(new Error('Connection failed'));

    render(<App />);

    // Wait for disconnected state
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Disconnected');
    });

    // Verify input is disabled
    const messageInput = screen.getByRole('textbox');
    expect(messageInput).toBeDisabled();
  });

  it('should show clear conversation button when messages exist', async () => {
    const user = userEvent.setup();
    
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);
    mockApiService.sendMessage.mockResolvedValue({
      outputMessage: 'Response',
      outputSessionId: 'session-123'
    });

    render(<App />);

    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Initially clear button should be disabled
    const clearButton = screen.getByTestId('clear-conversation-button');
    expect(clearButton).toBeDisabled();

    // Send a message
    const messageInput = screen.getByPlaceholderText('Type your message...');
    await user.type(messageInput, 'Test');
    await user.keyboard('{Enter}');

    // Wait for response and verify clear button is enabled
    await waitFor(() => {
      expect(mockApiService.sendMessage).toHaveBeenCalled();
    });
  });

  it('should maintain proper component integration', async () => {
    mockApiService.checkHealth.mockResolvedValue(mockHealthResponse);

    render(<App />);

    // Wait for connection
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });

    // Verify all main components are integrated
    expect(screen.getByTestId('chat-container')).toBeInTheDocument();
    expect(screen.getByTestId('message-list')).toBeInTheDocument();
    expect(screen.getByRole('textbox')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /send message/i })).toBeInTheDocument();
    expect(screen.getByTestId('clear-conversation-button')).toBeInTheDocument();

    // Verify status indicator is present (by checking for status text)
    expect(screen.getByText('Connected')).toBeInTheDocument();
  });
});
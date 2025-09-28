import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/react';
import App from './App';
import { ApiService } from './services/ApiService';

// Mock the ApiService
vi.mock('./services/ApiService');

// Mock environment variables
vi.mock('import.meta', () => ({
  env: {
    VITE_API_BASE_URL: 'http://localhost:8080',
    VITE_API_TIMEOUT: '10000',
    VITE_API_MAX_RETRIES: '3',
    VITE_API_RETRY_DELAY: '1000',
  }
}));

describe('App Component', () => {
  let mockApiService: {
    checkHealth: ReturnType<typeof vi.fn>;
    sendMessage: ReturnType<typeof vi.fn>;
  };

  beforeEach(() => {
    // Reset all mocks
    vi.clearAllMocks();
    
    // Create mock API service methods
    mockApiService = {
      checkHealth: vi.fn(),
      sendMessage: vi.fn(),
    };

    // Mock the ApiService constructor to return our mock
    (ApiService as any).mockImplementation(() => mockApiService);
  });

  afterEach(() => {
    vi.clearAllTimers();
  });

  it('renders the main app structure', () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Check for main app elements
    expect(screen.getByTestId('app')).toBeInTheDocument();
    expect(screen.getByText('AI Chatbot')).toBeInTheDocument();
    expect(screen.getByRole('main')).toBeInTheDocument();
  });

  it('performs health check on startup', async () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Wait for health check to be called
    await waitFor(() => {
      expect(mockApiService.checkHealth).toHaveBeenCalledTimes(1);
    });
  });

  it('shows connected status when health check succeeds', async () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Wait for health check and status update - check for Connected text in footer
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Connected');
    });
  });

  it('shows disconnected status when health check fails', async () => {
    mockApiService.checkHealth.mockRejectedValue(new Error('Connection failed'));

    render(<App />);

    // Wait for health check failure and status update - check for Disconnected text in footer
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('Disconnected');
    });
  });

  it('displays message count in footer', async () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Initially should show 0 messages
    await waitFor(() => {
      const footer = screen.getByRole('contentinfo');
      expect(footer).toHaveTextContent('0 message');
    });
  });

  it('handles API service configuration from environment variables', () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Verify ApiService was called with correct config
    expect(ApiService).toHaveBeenCalledWith({
      baseUrl: 'http://localhost:8080',
      timeout: 10000,
      maxRetries: 3,
      retryDelay: 1000,
    });
  });

  it('renders chat container with proper props', async () => {
    mockApiService.checkHealth.mockResolvedValue({
      status: 'healthy',
      version: '1.0.0',
      uptime: 12345,
      timestamp: new Date().toISOString(),
    });

    render(<App />);

    // Wait for component to render
    await waitFor(() => {
      expect(screen.getByTestId('chat-container')).toBeInTheDocument();
    });
  });
});
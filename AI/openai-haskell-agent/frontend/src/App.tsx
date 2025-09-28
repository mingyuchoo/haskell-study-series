import React, { useEffect, useCallback, useMemo } from 'react';
import { ChatContainer } from './components/ChatContainer';
import { ErrorBoundary } from './components/ErrorBoundary';
import { ApiService } from './services/ApiService';
import { useChatState } from './hooks/useChatState';
import './App.css';

/**
 * Configuration for the API service
 * Can be overridden via environment variables
 */
const API_CONFIG = {
  baseUrl: import.meta.env['VITE_API_BASE_URL'] || 'http://localhost:8080',
  timeout: parseInt(import.meta.env['VITE_API_TIMEOUT'] || '10000', 10),
  maxRetries: parseInt(import.meta.env['VITE_API_MAX_RETRIES'] || '3', 10),
  retryDelay: parseInt(import.meta.env['VITE_API_RETRY_DELAY'] || '1000', 10),
};

/**
 * Health check interval in milliseconds
 */
const HEALTH_CHECK_INTERVAL = 30000; // 30 seconds

/**
 * Main App component with global state management
 * Manages API service initialization, health checks, and error boundaries
 */
function App() {
  // Initialize API service with configuration
  const apiService = useMemo(() => new ApiService(API_CONFIG), []);
  
  // Initialize chat state with the API service
  const { state: chatState, actions: chatActions } = useChatState(apiService);

  /**
   * Performs health check and updates connection status
   */
  const performHealthCheck = useCallback(async () => {
    try {
      await apiService.checkHealth();
      chatActions.setConnectionStatus(true);
      chatActions.setError(null);
    } catch (error) {
      chatActions.setConnectionStatus(false);
      
      // Only set error if we don't already have one to avoid overriding user errors
      if (!chatState.error) {
        const errorMessage = error instanceof Error 
          ? `Connection failed: ${error.message}`
          : 'Unable to connect to server';
        chatActions.setError(errorMessage);
      }
    }
  }, [apiService, chatActions, chatState.error]);

  /**
   * Initialize app on startup
   */
  useEffect(() => {
    // Perform initial health check
    performHealthCheck();

    // Set up periodic health checks
    const healthCheckInterval = setInterval(performHealthCheck, HEALTH_CHECK_INTERVAL);

    // Cleanup interval on unmount
    return () => {
      clearInterval(healthCheckInterval);
    };
  }, [performHealthCheck]);

  /**
   * Handle global errors from error boundary
   */
  const handleGlobalError = useCallback((error: Error, errorInfo: React.ErrorInfo) => {
    console.error('Global error caught by ErrorBoundary:', error, errorInfo);
    
    // You could send error reports to a logging service here
    // For now, we'll just log to console
  }, []);

  /**
   * Error boundary fallback component
   */
  const errorFallback = (
    <div className="app-error">
      <h1>AI Chatbot</h1>
      <div className="error-container">
        <h2>Application Error</h2>
        <p>
          The chat application encountered an unexpected error. 
          Please refresh the page to continue.
        </p>
        <button 
          onClick={() => window.location.reload()}
          className="retry-button"
        >
          Refresh Application
        </button>
      </div>
    </div>
  );

  return (
    <ErrorBoundary fallback={errorFallback} onError={handleGlobalError}>
      <div className="app" data-testid="app">
        <header className="app-header">
          <h1>AI Chatbot</h1>
        </header>
        
        <main className="app-main">
          <ChatContainer
            chatState={chatState}
            chatActions={chatActions}
            className="main-chat-container"
          />
        </main>
        
        <footer className="app-footer">
          <p>
            {chatState.isConnected ? 'Connected' : 'Disconnected'} • 
            {chatState.messages.length} message{chatState.messages.length !== 1 ? 's' : ''}
          </p>
        </footer>
      </div>
    </ErrorBoundary>
  );
}

export default App;

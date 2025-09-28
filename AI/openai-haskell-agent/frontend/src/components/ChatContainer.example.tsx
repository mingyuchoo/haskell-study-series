/**
 * Example usage of ChatContainer component
 * This demonstrates how to integrate ChatContainer with the useChatState hook and ApiService
 */

import React, { useEffect } from 'react';
import { ChatContainer } from './ChatContainer';
import { useChatState } from '../hooks/useChatState';
import { ApiService } from '../services/ApiService';

// Create API service instance
const apiService = new ApiService({
  baseUrl: 'http://localhost:8080',
  timeout: 10000,
  maxRetries: 3,
  retryDelay: 1000,
});

/**
 * Example App component showing ChatContainer integration
 */
export const ChatApp: React.FC = () => {
  // Use the chat state hook with the API service
  const { state, actions } = useChatState(apiService);

  // Check backend health on component mount
  useEffect(() => {
    const checkHealth = async () => {
      try {
        await apiService.checkHealth();
        actions.setConnectionStatus(true);
      } catch (error) {
        actions.setConnectionStatus(false);
        actions.setError('Failed to connect to backend server');
      }
    };

    checkHealth();
  }, [actions]);

  return (
    <div style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
      <header style={{ padding: '1rem', backgroundColor: '#f8f9fa', borderBottom: '1px solid #dee2e6' }}>
        <h1 style={{ margin: 0, fontSize: '1.5rem', color: '#333' }}>
          AI Chatbot
        </h1>
      </header>
      
      <main style={{ flex: 1, padding: '1rem', maxWidth: '800px', margin: '0 auto', width: '100%' }}>
        <ChatContainer
          chatState={state}
          chatActions={actions}
        />
      </main>
    </div>
  );
};

export default ChatApp;
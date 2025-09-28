import { useState, useCallback, useRef } from 'react';
import { Message, ChatState, ApiError, ErrorType } from '../types/chat';
import { ChatInput, ChatOutput } from '../types/api';
import { ApiService } from '../services/ApiService';

/**
 * Actions that can be performed on chat state
 */
export interface ChatActions {
  sendMessage: (message: string) => Promise<void>;
  clearConversation: () => void;
  setError: (error: string | null) => void;
  setConnectionStatus: (isConnected: boolean) => void;
  retryLastMessage: () => Promise<void>;
}

/**
 * Return type for the useChatState hook
 */
export interface UseChatStateReturn {
  state: ChatState;
  actions: ChatActions;
}

/**
 * Custom hook for managing chat state and operations
 * Handles messages, session management, loading states, and API communication
 */
export function useChatState(apiService: ApiService): UseChatStateReturn {
  const [state, setState] = useState<ChatState>({
    messages: [],
    sessionId: null,
    isLoading: false,
    isConnected: false,
    error: null,
  });

  // Use ref to track current sessionId to avoid stale closures
  const sessionIdRef = useRef<string | null>(null);
  // Track the last message for retry functionality
  const lastMessageRef = useRef<string | null>(null);
  
  // Keep ref in sync with state
  sessionIdRef.current = state.sessionId;

  /**
   * Generates a unique ID for messages
   */
  const generateMessageId = useCallback((): string => {
    return `msg_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }, []);

  /**
   * Creates a new message object
   */
  const createMessage = useCallback((
    content: string,
    role: 'user' | 'assistant'
  ): Message => {
    return {
      id: generateMessageId(),
      content,
      role,
      timestamp: new Date(),
    };
  }, [generateMessageId]);

  /**
   * Sends a message to the AI chatbot
   * Handles session management and error states
   */
  const sendMessage = useCallback(async (message: string): Promise<void> => {
    if (!message.trim()) {
      return;
    }

    const trimmedMessage = message.trim();
    lastMessageRef.current = trimmedMessage;

    // Add user message immediately to UI
    const userMessage = createMessage(trimmedMessage, 'user');
    
    setState(prevState => ({
      ...prevState,
      messages: [...prevState.messages, userMessage],
      isLoading: true,
      error: null,
    }));

    try {
      const chatInput: ChatInput = {
        inputMessage: trimmedMessage,
        sessionId: sessionIdRef.current,
      };

      const response: ChatOutput = await apiService.sendMessage(chatInput);

      // Add assistant response and update session
      const assistantMessage = createMessage(response.outputMessage, 'assistant');
      
      setState(prevState => ({
        ...prevState,
        messages: [...prevState.messages, assistantMessage],
        sessionId: response.outputSessionId,
        isLoading: false,
        error: null,
      }));

      // Clear last message on success
      lastMessageRef.current = null;

    } catch (error) {
      let errorMessage = 'Failed to send message';
      let apiError: ApiError | null = null;
      
      // Handle ApiError objects
      if (error && typeof error === 'object' && 'type' in error && 'message' in error) {
        apiError = error as ApiError;
        errorMessage = apiError.message;
      } else if (error instanceof Error) {
        errorMessage = error.message;
      } else if (typeof error === 'string') {
        errorMessage = error;
      }

      setState(prevState => ({
        ...prevState,
        isLoading: false,
        error: errorMessage,
      }));

      // Re-throw the error so components can handle it
      throw apiError || new Error(errorMessage);
    }
  }, [createMessage, apiService]);

  /**
   * Clears the conversation history and resets session
   */
  const clearConversation = useCallback((): void => {
    setState(prevState => ({
      ...prevState,
      messages: [],
      sessionId: null,
      error: null,
    }));
    sessionIdRef.current = null;
  }, []);

  /**
   * Sets error state
   */
  const setError = useCallback((error: string | null): void => {
    setState(prevState => ({
      ...prevState,
      error,
    }));
  }, []);

  /**
   * Updates connection status
   */
  const setConnectionStatus = useCallback((isConnected: boolean): void => {
    setState(prevState => ({
      ...prevState,
      isConnected,
    }));
  }, []);

  /**
   * Retries the last failed message
   */
  const retryLastMessage = useCallback(async (): Promise<void> => {
    if (!lastMessageRef.current) {
      throw new Error('No message to retry');
    }

    // Remove the last user message from the UI (it will be re-added by sendMessage)
    setState(prevState => ({
      ...prevState,
      messages: prevState.messages.slice(0, -1),
      error: null,
    }));

    // Retry sending the message
    await sendMessage(lastMessageRef.current);
  }, [sendMessage]);

  const actions: ChatActions = {
    sendMessage,
    clearConversation,
    setError,
    setConnectionStatus,
    retryLastMessage,
  };

  return {
    state,
    actions,
  };
}
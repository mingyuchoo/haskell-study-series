/**
 * OpenAI Chat Web App - Client-side JavaScript
 * 
 * Refactored to follow Railway Oriented Programming principles
 * with a focus on explicit success/failure paths and functional composition
 */

document.addEventListener('DOMContentLoaded', () => {
    // DOM Elements
    const elements = {
        chatMessages: document.getElementById('chat-messages'),
        messageInput: document.getElementById('message-input'),
        sendButton: document.getElementById('send-button'),
        statusDiv: document.getElementById('status'),
        welcomeMessage: document.querySelector('.welcome-message')
    };

    // Application State
    const state = {
        sessionId: null,
        isFirstMessage: true
    };

    /**
     * Result type for Railway Oriented Programming
     * Represents either a success or failure outcome
     */
    class Result {
        constructor(isSuccess, value, error = null) {
            this.isSuccess = isSuccess;
            this.value = value;
            this.error = error;
        }

        static success(value) {
            return new Result(true, value);
        }

        static failure(error) {
            return new Result(false, null, error);
        }

        // Apply a function to the value if this is a success, otherwise pass through the failure
        map(fn) {
            return this.isSuccess ? Result.success(fn(this.value)) : this;
        }

        // Apply a function that returns a Result if this is a success, otherwise pass through the failure
        flatMap(fn) {
            return this.isSuccess ? fn(this.value) : this;
        }

        // Handle both success and failure cases
        match({ onSuccess, onFailure }) {
            return this.isSuccess
                ? onSuccess(this.value)
                : onFailure(this.error);
        }
    }

    /**
     * Converts markdown text to HTML using the marked.js library
     * @param {string} markdown - The markdown text to convert
     * @returns {Result} - Success with HTML string or failure with error
     */
    const markdownToHtml = (markdown) => {
        try {
            if (typeof markdown !== 'string') {
                return Result.failure(new Error('Input must be a string'));
            }

            // Use marked.js library for markdown conversion
            const html = marked.parse(markdown);

            return Result.success(html);
        } catch (error) {
            console.error('Markdown conversion error:', error);
            return Result.failure(error);
        }
    };

    /**
     * Creates a message element and adds it to the chat
     * @param {string} content - The message content
     * @param {boolean} isUser - Whether the message is from the user
     * @returns {Result} - Success with the created element or failure with error
     */
    const createMessageElement = (content, isUser) => {
        try {
            const messageDiv = document.createElement('div');
            messageDiv.className = `message ${isUser ? 'user-message' : 'assistant-message'}`;

            if (isUser) {
                // User messages are plain text
                messageDiv.textContent = content;
            } else {
                // Assistant messages use markdown - render with marked.js
                try {
                    messageDiv.innerHTML = marked.parse(content);
                    return Result.success(messageDiv);
                } catch (error) {
                    console.error('Error rendering markdown:', error);
                    return Result.failure(error);
                }
            }

            return Result.success(messageDiv);
        } catch (error) {
            console.error('Error creating message element:', error);
            return Result.failure(error);
        }
    };

    /**
     * Adds a message to the chat container
     * @param {string} content - The message content
     * @param {boolean} isUser - Whether the message is from the user
     * @returns {Result} - Success with the added element or failure with error
     */
    const addMessage = (content, isUser) => {
        return createMessageElement(content, isUser).flatMap(messageDiv => {
            try {
                elements.chatMessages.appendChild(messageDiv);
                elements.chatMessages.scrollTop = elements.chatMessages.scrollHeight;
                return Result.success(messageDiv);
            } catch (error) {
                console.error('Error adding message to chat:', error);
                return Result.failure(error);
            }
        });
    };

    /**
     * Creates and shows a typing indicator
     * @returns {Result} - Success with the indicator element or failure with error
     */
    const showTypingIndicator = () => {
        try {
            // Remove any existing typing indicator
            const existingIndicator = document.getElementById('typing-indicator');
            if (existingIndicator) {
                existingIndicator.remove();
            }

            // Create assistant message box
            const messageDiv = document.createElement('div');
            messageDiv.className = 'message assistant-message';
            messageDiv.id = 'assistant-typing-message';

            // Create typing indicator inside the message box
            const typingDiv = document.createElement('div');
            typingDiv.className = 'typing-indicator';
            typingDiv.id = 'typing-indicator';

            // Add dots to the typing indicator
            for (let i = 0; i < 3; i <>) {
                const dot = document.createElement('span');
                typingDiv.appendChild(dot);
            }

            // Add typing indicator to the message box
            messageDiv.appendChild(typingDiv);

            // Add message box to chat
            elements.chatMessages.appendChild(messageDiv);
            elements.chatMessages.scrollTop = elements.chatMessages.scrollHeight;

            return Result.success(messageDiv);
        } catch (error) {
            console.error('Error showing typing indicator:', error);
            return Result.failure(error);
        }
    };

    /**
     * Updates the status display
     * @param {string} status - The status type ('thinking', 'ready', 'error')
     * @param {string} message - The status message
     */
    const updateStatus = (status, message = '') => {
        const statusIcons = {
            thinking: '<i class="fas fa-spinner fa-spin status-thinking"></i> Thinking...',
            ready: '<i class="fas fa-circle status-ready"></i> Ready to chat',
            error: `<i class="fas fa-exclamation-circle status-error"></i> Error: ${message}`
        };

        elements.statusDiv.innerHTML = statusIcons[status] || statusIcons.ready;
    };

    /**
     * Fetches a response from the API
     * @param {string} content - The user's message
     * @returns {Promise<Result>} - Success with response data or failure with error
     */
    const fetchResponse = async (content) => {
        try {
            const response = await fetch('/api/chat', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    inputMessage: content,
                    sessionId: state.sessionId
                })
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const data = await response.json();
            return Result.success(data);
        } catch (error) {
            console.error('API request error:', error);
            return Result.failure(error);
        }
    };

    /**
     * Simulates streaming of the response
     * @param {string} fullResponse - The complete response text
     * @param {HTMLElement} typingMessageBox - The message element to update
     * @returns {Promise<Result>} - Success when streaming completes or failure with error
     */
    const simulateResponseStreaming = (fullResponse, typingMessageBox) => {
        return new Promise((resolve) => {
            let currentResponse = '';
            let lastResponseLength = 0;

            const streamingInterval = setInterval(() => {
                try {
                    if (lastResponseLength >= fullResponse.length) {
                        clearInterval(streamingInterval);
                        updateStatus('ready');
                        resolve(Result.success());
                        return;
                    }

                    // Add a few characters at a time to simulate streaming
                    const chunkSize = Math.floor(Math.random() * 5) + 1;  // 1-5 characters at a time
                    const nextChunk = fullResponse.substring(lastResponseLength, lastResponseLength + chunkSize);
                    lastResponseLength += chunkSize;

                    // Update the response with the new chunk
                    currentResponse += nextChunk;

                    // Update the message with the current response using marked.js
                    try {
                        // Use marked.js to render markdown
                        const html = marked.parse(currentResponse);
                        typingMessageBox.innerHTML = html;
                        typingMessageBox.setAttribute('data-raw-content', currentResponse);
                        typingMessageBox.removeAttribute('id'); // Remove the special ID

                        // Scroll to the bottom
                        elements.chatMessages.scrollTop = elements.chatMessages.scrollHeight;
                    } catch (error) {
                        console.error('Error updating streaming response:', error);
                        // Continue streaming even if there's a markdown error
                    }
                } catch (error) {
                    clearInterval(streamingInterval);
                    console.error('Streaming error:', error);
                    resolve(Result.failure(error));
                }
            }, 20);  // Update every 20ms for a smooth effect
        });
    };

    /**
     * Handles errors during the message sending process
     * @param {Error} error - The error that occurred
     */
    const handleMessageError = (error) => {
        console.error('Message handling error:', error);

        // Show error message in the typing indicator box
        const typingMessageBox = document.getElementById('assistant-typing-message');
        if (typingMessageBox) {
            typingMessageBox.innerHTML = `<p>Error: ${error.message}</p>`;
            typingMessageBox.classList.add('error');
            typingMessageBox.removeAttribute('id');
        }

        updateStatus('error', error.message);
    };

    /**
     * Processes a user message and handles the response
     * @param {string} content - The user's message
     */
    const sendMessage = async (content) => {
        // Validate input
        if (!content.trim()) return;

        // Hide welcome message on first message
        if (state.isFirstMessage && elements.welcomeMessage) {
            elements.welcomeMessage.style.display = 'none';
            state.isFirstMessage = false;
        }

        // Add user message to chat
        addMessage(content, true);

        // Clear input
        elements.messageInput.value = '';

        // Update status and show typing indicator
        updateStatus('thinking');
        const typingIndicatorResult = showTypingIndicator();

        // Process the message using Railway pattern
        typingIndicatorResult.flatMap(async () => {
            try {
                // Fetch response from API
                const responseResult = await fetchResponse(content);

                return responseResult.flatMap(async (data) => {
                    // Save session ID for future requests
                    state.sessionId = data.outputSessionId;

                    // Get the typing indicator message box
                    const typingMessageBox = document.getElementById('assistant-typing-message');
                    if (!typingMessageBox) {
                        return Result.failure(new Error('Typing indicator not found'));
                    }

                    // Simulate streaming of the response
                    const streamingResult = await simulateResponseStreaming(
                        data.outputMessage,
                        typingMessageBox
                    );

                    return streamingResult;
                });
            } catch (error) {
                handleMessageError(error);
                return Result.failure(error);
            }
        }).match({
            onSuccess: () => { }, // Success case already handled in simulateResponseStreaming
            onFailure: handleMessageError
        });
    };

    // Event listeners
    elements.sendButton.addEventListener('click', () => {
        sendMessage(elements.messageInput.value);
    });

    elements.messageInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            sendMessage(elements.messageInput.value);
        }
    });

    // Focus the input field on load
    elements.messageInput.focus();

    // Initialize with ready status
    updateStatus('ready');
});

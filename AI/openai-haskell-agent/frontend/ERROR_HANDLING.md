# Error Handling Implementation

This document describes the comprehensive error handling system implemented for the AI Chatbot Web UI.

## Overview

The error handling system provides:
- **User-friendly error messages** for different error types
- **Automatic retry functionality** for transient errors
- **Manual retry capabilities** for failed operations
- **Proper error classification** and display
- **Comprehensive test coverage** for error scenarios

## Components

### 1. ErrorDisplay Component

A dedicated component for displaying errors with user-friendly messages and retry functionality.

**Features:**
- Different error icons and styling based on error type
- User-friendly error messages (hiding technical details from users)
- Retry button for retryable errors
- Dismiss functionality
- Technical details in development mode
- Accessibility support (ARIA labels, roles)

**Usage:**
```tsx
<ErrorDisplay
  error={apiError}
  onRetry={handleRetry}
  onDismiss={handleDismiss}
  canRetry={true}
/>
```

### 2. Enhanced ApiService

The ApiService has been enhanced with comprehensive error handling and retry logic.

**Features:**
- **Exponential backoff** with jitter for retries
- **Configurable retry policies** per error type
- **Timeout handling** with AbortController
- **Error classification** (Network, Server, Timeout, Validation, Unknown)
- **Manual retry operations** for custom scenarios

**Configuration:**
```typescript
const apiService = new ApiService({
  baseUrl: 'http://localhost:8080',
  timeout: 10000,
  retryConfig: {
    maxRetries: 3,
    baseDelay: 1000,
    maxDelay: 10000,
    backoffMultiplier: 2,
    retryableErrors: [
      ErrorType.NETWORK_ERROR,
      ErrorType.TIMEOUT_ERROR,
      ErrorType.SERVER_ERROR
    ]
  }
});
```

### 3. Enhanced useChatState Hook

The chat state hook now includes retry functionality and better error handling.

**New Features:**
- `retryLastMessage()` action for retrying failed messages
- Enhanced error handling with ApiError objects
- Proper error propagation to components
- Last message tracking for retry functionality

### 4. Updated ChatContainer

The main chat container now integrates the ErrorDisplay component and retry functionality.

**Features:**
- Automatic error display when errors occur
- Retry button for retryable errors
- Error dismissal functionality
- Error classification for appropriate UI feedback

## Error Types

The system classifies errors into the following types:

### 1. Network Errors (`NETWORK_ERROR`)
- **Cause:** No internet connection, CORS issues, DNS failures
- **User Message:** "Unable to connect to the server. Please check your internet connection and try again."
- **Icon:** 🌐
- **Retryable:** Yes
- **Severity:** Warning

### 2. Server Errors (`SERVER_ERROR`)
- **Cause:** HTTP 5xx status codes, backend crashes
- **User Message:** "The server encountered an error. Please try again in a moment."
- **Icon:** 🔧
- **Retryable:** Yes
- **Severity:** Critical

### 3. Timeout Errors (`TIMEOUT_ERROR`)
- **Cause:** Request takes longer than configured timeout
- **User Message:** "The request took too long to complete. Please try again."
- **Icon:** ⏱️
- **Retryable:** Yes
- **Severity:** Warning

### 4. Validation Errors (`VALIDATION_ERROR`)
- **Cause:** HTTP 4xx status codes, invalid input
- **User Message:** "There was an issue with your request. Please check your input and try again."
- **Icon:** ⚠️
- **Retryable:** No
- **Severity:** Info

### 5. Unknown Errors (`UNKNOWN_ERROR`)
- **Cause:** Unexpected errors, unclassified issues
- **User Message:** "An unexpected error occurred. Please try again."
- **Icon:** ❌
- **Retryable:** No
- **Severity:** Critical

## Retry Logic

### Automatic Retries

The ApiService automatically retries failed requests based on the retry configuration:

1. **Exponential Backoff:** Delay increases exponentially with each retry
2. **Jitter:** Random delay added to prevent thundering herd
3. **Max Delay Cap:** Prevents excessively long delays
4. **Error Type Filtering:** Only retries specific error types

**Formula:**
```
delay = min(baseDelay * (backoffMultiplier ^ attempt), maxDelay) + random(0, 1000)
```

### Manual Retries

Users can manually retry failed operations through the UI:

1. **Retry Button:** Appears for retryable errors
2. **Last Message Retry:** Retries the last failed message
3. **Error Recovery:** Removes duplicate messages and updates state

## User Experience

### Error Display
- **Non-blocking:** Errors don't prevent other interactions
- **Contextual:** Different styling based on error severity
- **Actionable:** Clear retry and dismiss options
- **Informative:** User-friendly messages with technical details in dev mode

### Loading States
- **Immediate Feedback:** User messages appear immediately
- **Loading Indicators:** Show when requests are in progress
- **Disabled States:** Prevent duplicate requests during loading

### Accessibility
- **ARIA Labels:** Proper labeling for screen readers
- **Keyboard Navigation:** All interactive elements are keyboard accessible
- **High Contrast:** Support for high contrast mode
- **Reduced Motion:** Respects user's motion preferences

## Testing

### Unit Tests

**ErrorDisplay Component:**
- Error message display for all error types
- Retry and dismiss functionality
- CSS class application
- Accessibility attributes
- Technical details visibility

**ApiService:**
- Network error handling and retries
- Timeout error handling
- Server error classification
- Client error handling (no retries)
- Retry configuration respect
- Manual retry operations

**useChatState Hook:**
- Retry last message functionality
- Error handling improvements
- State management during retries
- Error propagation

**ChatContainer:**
- Error display integration
- Retry functionality
- Error classification
- User interaction handling

### Integration Tests

- Complete error flow from API to UI
- Retry scenarios with state updates
- Error recovery and user feedback
- Cross-component error handling

## Configuration

### Environment Variables

```env
# API Configuration
VITE_API_BASE_URL=http://localhost:8080
VITE_API_TIMEOUT=10000

# Retry Configuration
VITE_MAX_RETRIES=3
VITE_RETRY_BASE_DELAY=1000
VITE_RETRY_MAX_DELAY=10000
```

### Runtime Configuration

The error handling system can be configured at runtime:

```typescript
// Update API service configuration
apiService.updateBaseUrl('https://new-api-url.com');

// Custom retry configuration
const customApiService = new ApiService({
  retryConfig: {
    maxRetries: 5,
    baseDelay: 500,
    maxDelay: 30000,
    backoffMultiplier: 1.5,
    retryableErrors: [ErrorType.NETWORK_ERROR, ErrorType.SERVER_ERROR]
  }
});
```

## Best Practices

### For Developers

1. **Always handle errors:** Use try-catch blocks and proper error propagation
2. **Classify errors correctly:** Use appropriate ErrorType for different scenarios
3. **Provide user-friendly messages:** Hide technical details from end users
4. **Test error scenarios:** Include error cases in unit and integration tests
5. **Log errors appropriately:** Use console.error for debugging, avoid exposing sensitive data

### For Users

1. **Check internet connection:** For network errors
2. **Wait and retry:** For timeout and server errors
3. **Review input:** For validation errors
4. **Contact support:** For persistent unknown errors

## Monitoring and Debugging

### Development Mode

- Technical error details are shown in expandable sections
- Console logging for all error scenarios
- Detailed error objects with stack traces

### Production Mode

- User-friendly error messages only
- Error reporting to monitoring services (if configured)
- Graceful degradation of functionality

### Error Tracking

The system is designed to integrate with error tracking services:

```typescript
// Example integration with error tracking
const errorBoundaryHandler = (error: Error, errorInfo: ErrorInfo) => {
  // Send to error tracking service
  errorTrackingService.captureException(error, {
    extra: errorInfo,
    tags: { component: 'ChatContainer' }
  });
};
```

## Future Enhancements

1. **Offline Support:** Handle offline scenarios with appropriate messaging
2. **Error Analytics:** Track error patterns and frequencies
3. **Smart Retry:** Adaptive retry strategies based on error patterns
4. **User Preferences:** Allow users to configure retry behavior
5. **Error Recovery Suggestions:** Provide specific recovery steps for different errors
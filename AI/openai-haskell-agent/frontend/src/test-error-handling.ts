/**
 * Simple test script to verify error handling functionality
 * This can be run manually to test the error handling components
 */

import { ApiService } from './services/ApiService';
import { ErrorType } from './types/chat';

// Test the ApiService error handling
async function testApiServiceErrorHandling() {
  console.log('Testing ApiService error handling...');
  
  const apiService = new ApiService({
    baseUrl: 'http://invalid-url:9999', // This will cause network errors
    timeout: 1000,
    retryConfig: {
      maxRetries: 2,
      baseDelay: 100,
      maxDelay: 1000,
      backoffMultiplier: 2,
      retryableErrors: [ErrorType.NETWORK_ERROR, ErrorType.TIMEOUT_ERROR]
    }
  });

  try {
    await apiService.sendMessage({
      inputMessage: 'Test message',
      sessionId: null
    });
  } catch (error) {
    console.log('Caught expected error:', error);
    
    if (error && typeof error === 'object' && 'type' in error) {
      console.log('Error type:', (error as any).type);
      console.log('Error message:', (error as any).message);
    }
  }

  // Test retry operation
  console.log('Testing manual retry operation...');
  
  let attemptCount = 0;
  const failingOperation = async () => {
    attemptCount++;
    console.log(`Attempt ${attemptCount}`);
    if (attemptCount < 3) {
      throw new Error(`Operation failed on attempt ${attemptCount}`);
    }
    return 'Success!';
  };

  try {
    const result = await apiService.retryOperation(failingOperation, 3);
    console.log('Retry operation result:', result);
  } catch (error) {
    console.log('Retry operation failed:', error);
  }
}

// Test error classification
function testErrorClassification() {
  console.log('Testing error classification...');
  
  const testErrors = [
    'network connection failed',
    'timeout occurred',
    'server error 500',
    'validation failed',
    'invalid input',
    'unknown error'
  ];

  testErrors.forEach(errorMessage => {
    let errorType = ErrorType.UNKNOWN_ERROR;
    
    if (errorMessage.toLowerCase().includes('network') || errorMessage.toLowerCase().includes('connection')) {
      errorType = ErrorType.NETWORK_ERROR;
    } else if (errorMessage.toLowerCase().includes('timeout')) {
      errorType = ErrorType.TIMEOUT_ERROR;
    } else if (errorMessage.toLowerCase().includes('server')) {
      errorType = ErrorType.SERVER_ERROR;
    } else if (errorMessage.toLowerCase().includes('validation') || errorMessage.toLowerCase().includes('invalid')) {
      errorType = ErrorType.VALIDATION_ERROR;
    }

    console.log(`"${errorMessage}" -> ${errorType}`);
  });
}

// Run tests if this file is executed directly
if (typeof window === 'undefined') {
  testErrorClassification();
  testApiServiceErrorHandling().catch(console.error);
}

export { testApiServiceErrorHandling, testErrorClassification };
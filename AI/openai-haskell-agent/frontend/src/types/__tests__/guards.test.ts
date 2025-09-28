/**
 * Tests for type guards and validation utilities
 */

import {
  isChatInput,
  isChatOutput,
  isHealthInfo,
  isMessage,
  isValidMessageContent,
  isValidSessionId,
  isErrorType,
  isSuccessStatusCode,
  isValidTimestamp
} from '../guards';
import { ErrorType } from '../chat';

describe('Type Guards', () => {
  describe('isChatInput', () => {
    it('should return true for valid ChatInput', () => {
      const validInput = {
        inputMessage: 'Hello',
        sessionId: 'session-123'
      };
      expect(isChatInput(validInput)).toBe(true);
    });

    it('should return true for ChatInput with null sessionId', () => {
      const validInput = {
        inputMessage: 'Hello',
        sessionId: null
      };
      expect(isChatInput(validInput)).toBe(true);
    });

    it('should return false for invalid ChatInput', () => {
      expect(isChatInput({})).toBe(false);
      expect(isChatInput({ inputMessage: 123 })).toBe(false);
      expect(isChatInput({ sessionId: 'test' })).toBe(false);
      expect(isChatInput(null)).toBe(false);
    });
  });

  describe('isChatOutput', () => {
    it('should return true for valid ChatOutput', () => {
      const validOutput = {
        outputMessage: 'Hello response',
        outputSessionId: 'session-123'
      };
      expect(isChatOutput(validOutput)).toBe(true);
    });

    it('should return false for invalid ChatOutput', () => {
      expect(isChatOutput({})).toBe(false);
      expect(isChatOutput({ outputMessage: 'test' })).toBe(false);
      expect(isChatOutput({ outputSessionId: 'test' })).toBe(false);
      expect(isChatOutput(null)).toBe(false);
    });
  });

  describe('isHealthInfo', () => {
    it('should return true for valid HealthInfo', () => {
      const validHealth = {
        status: 'healthy',
        version: '1.0.0',
        uptime: 3600,
        timestamp: '2023-01-01T00:00:00Z'
      };
      expect(isHealthInfo(validHealth)).toBe(true);
    });

    it('should return false for invalid HealthInfo', () => {
      expect(isHealthInfo({})).toBe(false);
      expect(isHealthInfo({ status: 'healthy' })).toBe(false);
      expect(isHealthInfo(null)).toBe(false);
    });
  });

  describe('isMessage', () => {
    it('should return true for valid Message', () => {
      const validMessage = {
        id: 'msg-123',
        content: 'Hello',
        role: 'user' as const,
        timestamp: new Date()
      };
      expect(isMessage(validMessage)).toBe(true);
    });

    it('should return false for invalid Message', () => {
      expect(isMessage({})).toBe(false);
      expect(isMessage({ id: 'test', content: 'test', role: 'invalid' })).toBe(false);
      expect(isMessage(null)).toBe(false);
    });
  });

  describe('isValidMessageContent', () => {
    it('should return true for valid message content', () => {
      expect(isValidMessageContent('Hello')).toBe(true);
      expect(isValidMessageContent('  Hello  ')).toBe(true);
    });

    it('should return false for invalid message content', () => {
      expect(isValidMessageContent('')).toBe(false);
      expect(isValidMessageContent('   ')).toBe(false);
      expect(isValidMessageContent(123 as any)).toBe(false);
    });
  });

  describe('isValidSessionId', () => {
    it('should return true for valid session IDs', () => {
      expect(isValidSessionId(null)).toBe(true);
      expect(isValidSessionId('session-123')).toBe(true);
    });

    it('should return false for invalid session IDs', () => {
      expect(isValidSessionId('')).toBe(false);
      expect(isValidSessionId(123)).toBe(false);
      expect(isValidSessionId({})).toBe(false);
    });
  });

  describe('isErrorType', () => {
    it('should return true for valid ErrorType values', () => {
      expect(isErrorType(ErrorType.NETWORK_ERROR)).toBe(true);
      expect(isErrorType(ErrorType.SERVER_ERROR)).toBe(true);
      expect(isErrorType(ErrorType.VALIDATION_ERROR)).toBe(true);
      expect(isErrorType(ErrorType.TIMEOUT_ERROR)).toBe(true);
      expect(isErrorType(ErrorType.UNKNOWN_ERROR)).toBe(true);
    });

    it('should return false for invalid ErrorType values', () => {
      expect(isErrorType('INVALID_ERROR')).toBe(false);
      expect(isErrorType(123)).toBe(false);
      expect(isErrorType(null)).toBe(false);
    });
  });

  describe('isSuccessStatusCode', () => {
    it('should return true for success status codes', () => {
      expect(isSuccessStatusCode(200)).toBe(true);
      expect(isSuccessStatusCode(201)).toBe(true);
      expect(isSuccessStatusCode(299)).toBe(true);
    });

    it('should return false for non-success status codes', () => {
      expect(isSuccessStatusCode(199)).toBe(false);
      expect(isSuccessStatusCode(300)).toBe(false);
      expect(isSuccessStatusCode(404)).toBe(false);
      expect(isSuccessStatusCode(500)).toBe(false);
    });
  });

  describe('isValidTimestamp', () => {
    it('should return true for valid ISO timestamps', () => {
      expect(isValidTimestamp('2023-01-01T00:00:00.000Z')).toBe(true);
      expect(isValidTimestamp('2023-12-31T23:59:59.999Z')).toBe(true);
    });

    it('should return false for invalid timestamps', () => {
      expect(isValidTimestamp('invalid-date')).toBe(false);
      expect(isValidTimestamp('2023-01-01')).toBe(false);
      expect(isValidTimestamp('')).toBe(false);
    });
  });
});
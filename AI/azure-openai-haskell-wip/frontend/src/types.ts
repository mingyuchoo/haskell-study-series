export interface Message {
  role: 'user' | 'assistant'
  content: string
}

export interface ChatInput {
  inputMessage: string
  sessionId?: string
}

export interface ChatOutput {
  outputMessage: string
  outputSessionId: string
}

export interface HealthInfo {
  status: string
  version: string
  uptime: number
  timestamp: string
}

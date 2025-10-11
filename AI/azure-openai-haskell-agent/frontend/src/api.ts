import type { ChatRequest, ChatResponse, HealthResponse, Message } from './types'

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000'

export class ApiError extends Error {
  status?: number

  constructor(message: string, status?: number) {
    super(message)
    this.name = 'ApiError'
    this.status = status
  }
}

export const chatApi = {
  async sendMessage(messages: Message[]): Promise<ChatResponse> {
    const request: ChatRequest = {
      chatMessages: messages.map(msg => ({
        msgRole: msg.role,
        msgContent: msg.content
      }))
    }

    const response = await fetch(`${API_BASE_URL}/api/chat`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(request),
    })

    if (!response.ok) {
      throw new ApiError(
        `Failed to send message: ${response.statusText}`,
        response.status
      )
    }

    return response.json()
  },

  async checkHealth(): Promise<HealthResponse> {
    const response = await fetch(`${API_BASE_URL}/health`)

    if (!response.ok) {
      throw new ApiError(
        `Health check failed: ${response.statusText}`,
        response.status
      )
    }

    return response.json()
  },
}

import type { ChatInput, ChatOutput, HealthInfo } from './types'

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
  async sendMessage(input: ChatInput): Promise<ChatOutput> {
    const response = await fetch(`${API_BASE_URL}/api/chat`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(input),
    })

    if (!response.ok) {
      throw new ApiError(
        `Failed to send message: ${response.statusText}`,
        response.status
      )
    }

    return response.json()
  },

  async checkHealth(): Promise<HealthInfo> {
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

export interface Message {
  role: 'user' | 'assistant' | 'system'
  content: string
}

export interface ChatMessageDTO {
  msgRole: string
  msgContent: string
}

export interface ChatRequest {
  chatMessages: ChatMessageDTO[]
}

export interface ChatResponse {
  response: string
}

export interface HealthResponse {
  status: string
}

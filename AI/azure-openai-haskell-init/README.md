# azure-openai-azure-openai-haskell-init

A Haskell port of the bun-init project for Azure OpenAI multi-turn conversations.

## Prerequisites

- GHC (Glasgow Haskell Compiler) >= 9.2
- Cabal >= 3.6 or Stack

## Setup

1. Copy `.env.example` to `.env` and fill in your Azure OpenAI credentials:

```bash
cp .env.example .env
```

2. Install dependencies:

```bash
cabal update
cabal build
```

Or with Stack:

```bash
stack build
```

## Running

With Cabal:

```bash
cabal run azure-openai-azure-openai-haskell-init
```

Or with Stack:

```bash
stack run
```

## Project Structure

- `app/Main.hs` - Main application entry point
- `src/AzureOpenAI.hs` - Azure OpenAI client implementation
- `azure-openai-azure-openai-haskell-init.cabal` - Project configuration
- `.env.example` - Environment variables template

## Features

- Multi-turn conversation with Azure OpenAI
- Streaming response support
- Environment variable configuration
- Type-safe API interactions

# URL Shortener with Redis Enhancement

## Overview
This URL shortener service has been enhanced to use Redis for persistent storage and includes additional metadata for each shortened URL.

## New Features

### Enhanced URL Entity
Each URL now includes:
- **ID**: Unique identifier for the URL
- **Original URL**: The full URL that was shortened
- **Short URL**: The generated short URL (format: `http://localhost:8000/{id}`)
- **Created At**: Timestamp when the URL was created

### Redis Integration
- **Persistent Storage**: URLs are now stored in Redis instead of in-memory
- **Data Persistence**: URLs survive application restarts
- **Scalability**: Redis allows for better performance and scalability

### Web Interface Improvements
- **Enhanced Table**: Displays all URL metadata in a formatted table
- **Clickable Links**: Both original and short URLs are clickable
- **Timestamps**: Shows when each URL was created
- **Better Styling**: Improved CSS for better readability

## Running the Application

### Using Docker Compose (Recommended)
```bash
# Start Redis and the application
docker compose -f docker/docker-compose.yaml up --build -d

# Check logs
docker compose -f docker/docker-compose.yaml logs app

# Stop services
docker compose -f docker/docker-compose.yaml down
```

### Services
- **Redis**: Available on port 6379
- **Web Application**: Available on port 8000

## API Endpoints

### GET /
- **Description**: Home page with URL shortening form and list of all URLs
- **Response**: HTML page

### POST /
- **Description**: Create a new shortened URL
- **Parameters**: 
  - `url` (form parameter): The URL to shorten
- **Response**: Redirects to home page with updated list

### GET /{id}
- **Description**: Redirect to the original URL
- **Parameters**: 
  - `id` (path parameter): The URL ID
- **Response**: 302 redirect to original URL or 404 if not found

## Example Usage

1. **Access the web interface**: http://localhost:8000
2. **Shorten a URL**: Enter a URL in the form and submit
3. **Use the short URL**: Click on the generated short URL or use it directly
4. **View all URLs**: The home page shows all shortened URLs with metadata

## Data Storage

URLs are stored in Redis with the following structure:
- **Key**: `url:{id}` 
- **Value**: JSON object containing URL metadata
- **Counter**: `url:counter` tracks the next available ID

## Architecture

The application follows Clean Architecture principles:
- **Domain**: URL entity and repository interface
- **Application**: Use cases for URL operations
- **Infrastructure**: Redis repository implementation
- **Adapters**: Web controllers and views

## Dependencies

New dependencies added for Redis integration:
- `hedis`: Redis client for Haskell
- `aeson`: JSON serialization
- `time`: Timestamp handling
- `bytestring`: Binary data handling
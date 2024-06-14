#!/bin/bash

# Function to call AI model and retrieve the response
call_ai_model_stream() {
    local prompt="$1"
    local response=$(curl -s -X POST -H "Content-Type: application/json" -H "Accept: text/plain" \
        --data "{\"prompt\":\"${prompt}\"}" "http://localhost:5000/predict")
    echo "$response"
}

# Main script execution
read -p "Enter your prompt: " user_prompt
result=$(call_ai_model_stream "$user_prompt")
echo "Response from AI model:"
echo "$result"

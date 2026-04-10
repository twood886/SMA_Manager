library(websocket)
library(later)
library(httr)

# --- 1. CONFIGURATION ---
# The server likely requires this EXACT ID as the login, not just the email
STOMP_USER <- "f:8f21673e-a9e5-4d71-9258-59e301314298:twood@callodine.com"

WS_HOST    <- "dataserver-prod-us01.enfusionsystems.com"
WS_PORT    <- "8443"
WS_PATH    <- "/oms"
ORIGIN_URL <- "https://login-prod-us01.enfusionsystems.com"
BROWSER_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"

# PASTE YOUR VALID BROWSER TOKEN HERE (The long one starting with eyJ...)
BROWSER_ACCESS_TOKEN <- "eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJfSTVCWFk1SEpKUlZ3dklMcVFGUUdJWEVaRkh2OVBURDItNHpqSlR5bG1NIn0.eyJleHAiOjE3Njc4ODY0ODMsImlhdCI6MTc2Nzg4NjQyMywiYXV0aF90aW1lIjoxNzY3ODg2NDIzLCJqdGkiOiIwNDE1MmFmYi00MTVkLTQ0MDQtYWQ5My1mMjkyZGUyMjA5ZGIiLCJpc3MiOiJodHRwczovL2xvZ2luLXByb2QtdXMwMS5lbmZ1c2lvbnN5c3RlbXMuY29tL2F1dGgvcmVhbG1zL2VuZnVzaW9uIiwiYXVkIjoiYWNjb3VudCIsInN1YiI6ImY6OGYyMTY3M2UtYTllNS00ZDcxLTkyNTgtNTllMzAxMzE0Mjk4OnR3b29kQGNhbGxvZGluZS5jb20iLCJ0eXAiOiJCZWFyZXIiLCJhenAiOiJFbmZ1c2lvbiIsIm5vbmNlIjoiYjA5YTYwMDYtZmI4OS00MTZjLWI0ZDMtZjlmODdhY2Q3M2M3Iiwic2Vzc2lvbl9zdGF0ZSI6ImJjYmU5NzQzLWQzZDctNGQ0ZC1hOGM3LWVkNWZmNWVjZWZiYiIsImFjciI6IjEiLCJhbGxvd2VkLW9yaWdpbnMiOlsiLyoiXSwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iXX0sInJlc291cmNlX2FjY2VzcyI6eyJhY2NvdW50Ijp7InJvbGVzIjpbIm1hbmFnZS1hY2NvdW50Iiwidmlldy1wcm9maWxlIl19LCJFbmZ1c2lvbiI6eyJyb2xlcyI6WyJBdXRoZW50aWNhdGVkIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBlbWFpbCIsImVtYWlsX3ZlcmlmaWVkIjpmYWxzZSwibmFtZSI6IlRheWxvciBXb29kIiwiZW5mdXNpb25fd2ViYXBwX3VybCI6Imh0dHBzOi8vd2ViYXBwLmVuZnVzaW9uc3lzdGVtcy5jb20iLCJlbmZ1c2lvbl90b2tlbiI6ImV5SmhiR2NpT2lKU1V6STFOaUo5LmV5SndiMTlwWkNJNk5UQXdNRGt6TENKMWMyVnlYMmxrSWpvek5ETXlOQ3dpY205c1pYTWlPaUpCZFhSb1pXNTBhV05oZEdWa0lpd2lZV1J0YVc1eWIyeGxjeUk2SWlJc0luQnZYMmh2YzNRaU9pSndiMlF4TnkxemRHRnVaR0ZzYjI1bExYTmxjblpsY2k1a1pXWmhkV3gwTG5OMll5NXdjbVF4TG1zNGN5NXNiMk5oYkNJc0luTmxjblpwWTJWZmFHOXpkQ0k2SW5ObGNuWnBZMlV3T1NJc0luTjFZaUk2SW5SM2IyOWtRR05oYkd4dlpHbHVaUzVqYjIwaUxDSnBZWFFpT2pFM05qYzRPRFkwTWpNc0ltVjRjQ0k2TVRjMk9UQTVOakF5TTMwLmV2SmZYWWtWbi1vWVhOX2hyUkc5cDQ4WWhhRC1IYmV6azBrRkt6SWlUUElTREFTQzgxQjl0R0ZLYWN3SWk0QnRzQ0xXalhlcFdJakM0eDBVbFdDOVBXeTJNN0p6TkYwcW1mdjBqRVYySWxYN3JNdFNUV1R6Y255SjBxZHBCSXl1QllqcEdhMTFxbDZjcGRBT2hzb2I4N1dIMG9zS013WVBPYlBiZEdhX0lYZTd6ZEJvU3ZhNktVcUxZR0xQdnBFUk1EQ2F3N1FTTEFYMXdlTjAwV2ZRQXJFTS1Nd1BLeXBRaDJpckhaV21xOFNpU2x0RXhkS09VZjFlcXFJNFdXd2kyWjBGY2dvcEMwdmlYemR2YWJCNzF2T3JRanpEUU1RNURyeEdoS0xOYkRlQkxhM1Iwcl84OS1YVmx1ZG9IVmdWMGRaSWNBWXBJRjVPbUFvNWt5Sk9SZyIsInByZWZlcnJlZF91c2VybmFtZSI6InR3b29kQGNhbGxvZGluZS5jb20iLCJnaXZlbl9uYW1lIjoiVGF5bG9yIiwiZmFtaWx5X25hbWUiOiJXb29kIiwiZW1haWwiOiJ0d29vZEBjYWxsb2RpbmUuY29tIn0.USbmvkCNjdbZgGarSgWjaMu0Jk3cBTqQXNy2JOG5zZ6H_EDzAd34pP5gVp4mzLgtlsEY9LYMfY_08xdJ3Nh4LqtaG36BPJUFqDNOjwKbS1fZi2EcDTqbnQTUvGB2NCbNS-g-J5FB4-hctGwzTmoiQZ-EScUmvwtiQT783FddJWWEdBJsV5yNxiDcUg6bjcJCB6MNW5YSRNyMTmu2QamDZrpu_3JaRAHIo6Jn4NtwsJ1WfGAjqijRdM3VcFVbkCQ9E3wN8t7vTTZRnF3i3NQYfjSoG52OfMM8ygLSh_nWgeYcocUwapIsvUKRtSkxKJOho3Uz8d8Qc5yXJn6qIbpQ4g"

connect_identity_fix <- function() {
  cat("Connecting with Full Identity...\n")
  
  ws <- WebSocket$new(
    paste0("wss://", WS_HOST, ":", WS_PORT, WS_PATH),
    protocols = c("v12.stomp", "v11.stomp"),
    headers = list(
      "Authorization" = paste("Bearer", BROWSER_ACCESS_TOKEN),
      "Origin" = ORIGIN_URL,
      "User-Agent" = BROWSER_UA
    ),
    autoConnect = FALSE
  )
  
  ws$onOpen(function(event) {
    cat("Socket Open! Sending STOMP Handshake...\n")
    
    stomp_text <- paste0(
      "CONNECT\n",
      "accept-version:1.2\n",
      "heart-beat:30000,30000\n",
      "host:", WS_HOST, "\n",
      # FIX: Use the specific ID from the token, not just the email
      "login:", STOMP_USER, "\n",
      "passcode:", BROWSER_ACCESS_TOKEN, "\n",
      "\n",
      intToUtf8(0)
    )
    
    ws$send(stomp_text)
  })

  ws$onMessage(function(event) {
    cat("RX:", event$data, "\n")
    if (grepl("CONNECTED", event$data)) {
        cat("\n>>> VICTORY: CONNECTED! <<<\n")
    }
  })
  
  ws$onClose(function(event) {
    cat("Disconnected: ", event$code, "\n")
  })
  
  ws$connect()
  return(ws)
}

# Run
ws_client <- connect_identity_fix()
while (TRUE) { later::run_now(0.1); Sys.sleep(0.1) }

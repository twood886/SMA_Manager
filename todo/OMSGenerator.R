#' @title OEMSConnection (R6 Object)
#' @description Manage Enfusion OEMS WebSocket and REST connectivity.
#' @import R6
#' @import httr
#' @import jsonlite
#' @import websocket
#' @import later
OEMSConnection <- R6::R6Class( #nolint
  "OEMSConnection",
  private = list(
    base_url_ = NULL,
    ws_url_ = NULL,
    timeout_ = 300, # Increased default to 5 minutes
    api_token_ = NULL,
    ws_ = NULL,
    application_session_token_ = NULL,

    generate_rest_api_token = function(username, password) {
      response <- httr::POST(
        url = paste0(private$base_url_, "/auth/authentication/generateSecureAPIToken"), #nolint
        httr::authenticate(username, password, type = "basic")
      )

      if (httr::status_code(response) != 200) {
        stop(sprintf("REST Auth Failed. Code: %s", httr::status_code(response)))
      }

      raw_token <- httr::content(response, "text", encoding = "UTF-8")
      # Fix 1: Strip quotes from token
      api_token <- gsub('^"|"$', '', raw_token)
      private$api_token_ <- api_token
      invisible(api_token)
    },

    create_websocket = function(username, api_token) {
      ws <- WebSocket$new(private$ws_url_, autoConnect = FALSE)

      ws$onOpen(function(event) {
        cat("[WS] Opened -> sending CONNECT\n")
        connect_msg <- jsonlite::toJSON(list(
          command = "CONNECT",
          login = username,
          passcode = api_token,
          `heart-beat` = "30000,30000",
          `accept-version` = "1.2"
        ), auto_unbox = TRUE)
        ws$send(connect_msg)

        # Heartbeat
        beat <- function() {
          if (!is.null(ws) && ws$readyState() == 1) {
            ws$send("\n")
            later::later(beat, 30)
          }
        }
        later::later(beat, 30)
      })

      ws$onMessage(function(event) {
        # server heartbeats may be just "\n"
        if (identical(event$data, "\n") || identical(trimws(event$data), "")) {
          return(invisible(NULL))
        }

        msg <- tryCatch(jsonlite::fromJSON(event$data), error = function(e) {
          cat("[WS] Non-JSON message:", substr(event$data, 1, 80), "\n")
          return(NULL)
        })

        if (is.null(msg)) return(invisible(NULL))

        if (!is.null(msg$command) && msg$command == "CONNECTED") {
          cat("[WS] CONNECTED -> SUBSCRIBE /oms\n")
          ws$send(jsonlite::toJSON(
            list(command = "SUBSCRIBE", destination = "/oms"),
            auto_unbox = TRUE
          ))
          return(invisible(NULL))
        }

        if (!is.null(msg$command) && msg$command == "ERROR") {
          cat("[WS ERROR]\n")
          print(msg)
          return(invisible(NULL))
        }

        if (!is.null(msg$payload$type) && msg$payload$type == "init-status") {
          cat("[WS Status]:", msg$payload$message, "\n")

          if (isTRUE(msg$payload$completed)) {
            private$application_session_token_ <- msg$payload$applicationSessionToken #nolint
            cat("[WS] Session token acquired!\n")
          }
        }
      })

      ws$onClose(function(event) {
        cat("[WS] Closed. Code:", event$code, "Reason:", event$reason, "\n")
      })

      ws$onError(function(event) {
        cat("[WS] Error:", event$message, "\n")
      })

      ws$connect()

      # Wait Loop
      start_time <- Sys.time()
      while (is.null(private$application_session_token_)) {
        if (difftime(Sys.time(), start_time, units = "secs") > 30) {
          ws$close()
          stop("Timeout waiting for init-status completed=true")
        }
        later::run_now(timeout = 0.1)
        Sys.sleep(0.05)
      }
      private$ws_ <- ws
      invisible(ws)
    }
  ),

  public = list(
    initialize = function(
      base_url = "https://dataserver-prod-us01.enfusionsystems.com",
      ws_url = "wss://dataserver-prod-us01.enfusionsystems.com/oms"
    ) {
      private$base_url_ <- base_url
      private$ws_url_ <- ws_url
    },

    connect = function(username, password) {
      private$generate_rest_api_token(username, password)
      private$create_websocket(username, private$api_token_)
      invisible(self)
    },

    session_token = function() private$application_session_token_,

    send_order = function(order_payload) {
      if (is.null(private$application_session_token_)) stop("Not connected.")
      resp <- httr::POST(
        url = paste0(private$base_url_, "/api/oms/v1/orders/save/transmit"),
        httr::add_headers(`X-Auth-Token` = private$application_session_token_),
        body = jsonlite::toJSON(
          order_payload,
          auto_unbox = TRUE,
          null = "null"
        ),
        encode = "json",
        httr::content_type_json()
      )
      httr::content(resp, "parsed")
    },
    close = function() {
      if (!is.null(private$ws_)) private$ws_$close()
      private$application_session_token_ <- NULL
      private$api_token_ <- NULL
      private$ws_ <- NULL
    }
  )
)


#' @title Connect to OEMS API
#' @param username Enfusion username
#' @param password Enfusion password
connectOEMS <- function(username, password) {
  if (!exists("OEMSConn")) {
    OEMSConn <<- OEMSConnection$new()
  }
  checkmate::assert_r6(OEMSConn, "OEMSConnection")
  OEMSConn$connect(username, password)
  invisible(TRUE)
}


sendTradestoOEMS <- function(
  id = NULL,
  instrument_id = NULL,
  trader_id = 
)

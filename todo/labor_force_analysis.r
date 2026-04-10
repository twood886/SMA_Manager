# Labor Force Analysis

MonthlyData <- R6::R6Class(
  public = list(
    # Employment
    empl = NULL,              # Employed - Latest (in Thousands)
    empl_lag = NULL,          # Employed - Lag 1M (in Thousands) 
    empl_to_empl = NULL,      # Employed to Employed (in Thousands)
    empl_to_unempl = NULL,    # Employed to Unemployed (in Thousands)
    empl_to_nilf = NULL,      # Employed to Not in Labor Force (in Thousands)
    empl_to_un5 = NULL,       # Employed to Unemployed < 5 Weeks (in Thousands)

    # Unemployment
    unempl = NULL,            # Unemployed - Latest (in Thousands)
    unempl_lag = NULL,        # Unemployed - Lag 1M (in Thousands)
    unempl_to_empl = NULL,    # Unemployed to Employed (in Thousands)
    unempl_to_unempl = NULL,  # Unemployed to Unemployed (in Thousands)
    unempl_to_nilf = NULL,    # Unemployed to Not in Labor Force (in Thousands)

    # Not in Labor Force
    nilf = NULL,              # Not in Labor Force - Latest (in Thousands)      
    nilf_lag = NULL,          # Not in Labor Force - Lag 1M (in Thousands)  
    nilf_to_empl = NULL,      # Not in Labor Force to Employed (in Thousands)
    nilf_to_unempl = NULL,    # Not in Labor Force to Unemployed (in Thousands)
    nilf_to_nilf = NULL,      # Not in Labor Force to Not in Labor Force (in Thousands)
    nilf_to_un5 = NULL,       # Not in Labor Force to Unemployed < 5 Weeks (in Thousands)
    nilf_to_un14 = NULL,      # Not in Labor Force to Unemployed 5-14 Weeks (in Thousands)
    nilf_to_un26 = NULL,      # Not in Labor Force to Unemployed 15-26 Weeks (in Thousands)
    nilf_to_un27 = NULL,      # Not in Labor Force to Unemployed 27+ Weeks (in Thousands)

    # Other
    other_to_empl = NULL,     # Other to Employed (in Thousands)
    other_to_unempl = NULL,   # Other to Unemployed (in Thousands)
    other_to_nilf = NULL,     # Other to Not in Labor Force (in Thousands)
    other_to_un5 = NULL,      # Other to Unemployed < 5 Weeks (in Thousands)

    # Unemployed < 5 Weeks
    un5 = NULL,               # Unemployed < 5 Weeks - Latest (in Thousands)
    un5_lag = NULL,           # Unemployed < 5 Weeks - Lag 1M (in Thousands)
    un5_to_empl = NULL,       # Unemployed < 5 Weeks to Employed (in Thousands)
    un5_to_un14 = NULL,       # Unemployed < 5 Weeks to Unemployed 5-14 Weeks (in Thousands)
    un5_to_nilf = NULL,       # Unemployed < 5 Weeks to Not in Labor Force (in Thousands)

    # Unemployed 5-14 Weeks
    un14 = NULL,              # Unemployed 5-14 Weeks - Latest (in Thousands)
    un14_lag = NULL,          # Unemployed 5-14 Weeks - Lag 1M (in Thousands)
    un14_to_empl = NULL,      # Unemployed 5-14 Weeks to Employed (in Thousands)
    un14_to_un14 = NULL,      # Unemployed 5-14 Weeks to Unemployed 5-14 Weeks (in Thousands)
    un14_to_un26 = NULL,      # Unemployed 5-14 Weeks to Unemployed 15-26 Weeks (in Thousands)
    un14_to_nilf = NULL,      # Unemployed 5-14 Weeks to Not in Labor Force (in Thousands)

    # Unemployed 15-26 Weeks
    un26 = NULL,              # Unemployed 15-26 Weeks - Latest (in Thousands)
    un26_lag = NULL,          # Unemployed 15-26 Weeks - Lag 1M (in Thousands
    un26_to_empl = NULL,      # Unemployed 15-26 Weeks to Employed (in Thousands)
    un26_to_un27 = NULL,      # Unemployed 15-26 Weeks to Unemployed 27+ Weeks (in Thousands)
    un26_to_nilf = NULL,      # Unemployed 15-26 Weeks to Not in Labor Force (in Thousands

    # Unemployed 27+ Weeks
    un27 = NULL,              # Unemployed 27+ Weeks - Latest (in Thousands)
    un27_lag = NULL,          # Unemployed 27+ Weeks - Lag 1M (in Thousands
    un27_to_empl = NULL,      # Unemployed 27+ Weeks to Employed (in Thousands)
    un27_to_un27 = NULL,      # Unemployed 27+ Weeks to Unemployed 27+ Weeks (in Thousands)
    un27_to_nilf = NULL,      # Unemployed 27+ Weeks to Not in Labor Force (in Thousands


    # Constructor
    #' @param date Date of the data
    initialize = function(date) {
      checkmate::assert_date(date)
      date <- lubridate::ceiling_date(date, "month") - 1
      prev_date <- (date + 1) %m-% months(1) - 1

      empl_data <- bdh("USEMTOTN Index", "PX_LAST", prev_date, date)
      self$empl <- empl_data$PX_LAST[2]
      self$empl_lag <- empl_data$PX_LAST[1]

      self$empl_to_empl <- bdh("BLSFE2EF Index", "PX_LAST", date, date)$PX_LAST[1]
      self$empl_to_unempl <- bdh("BLSFE2EN Index", "PX_LAST", date, date)$PX_LAST[1]
      self$empl_to_nilf <- bdh("BLSFE2EM Index", "PX_LAST", date, date)$PX_LAST[1]
      self$empl_to_un5 <- self$empl_to_unempl

      unempl_data <- bdh("USUETOTN Index", "PX_LAST", prev_date, date)
      self$unempl <- unempl_data$PX_LAST[2]
      self$unempl_lag <- unempl_data$PX_LAST[1]

      self$unempl_to_empl <- bdh("BLSFEMFN Index", "PX_LAST", date, date)$PX_LAST[1]
      self$unempl_to_unempl <- bdh("BLSFEMMN Index", "PX_LAST", date, date)$PX_LAST[1]
      self$unempl_to_nilf <- bdh("BLSFEMPF Index", "PX_LAST", date, date)$PX_LAST[1]

      nilf_data <- bdh("USNLTOTN Index", "PX_LAST", prev_date, date)
      self$nilf <- nilf_data$PX_LAST[2]
      self$nilf_lag <- nilf_data$PX_LAST[1]
      self$nilf_to_empl <- bdh("BLSFE2MN Index", "PX_LAST", date, date)$PX_LAST[1]
      self$nilf_to_unempl <- bdh("BLSFE2NM Index", "PX_LAST", date, date)$PX_LAST[1]
      self$nilf_to_nilf <- bdh("BLSFE2NF Index", "PX_LAST", date, date)$PX_LAST[1]

      self$other_to_empl <- bdh("BLSFFE2U Index", "PX_LAST", date, date)$PX_LAST[1]
      self$other_to_unempl <- bdh("BLSFE2UM Index", "PX_LAST", date, date)$PX_LAST[1]
      self$other_to_nilf <- bdh("BLSFE2UF Index", "PX_LAST", date, date)$PX_LAST[1]

      self$un5 <- bdh("ULVLNDTE Index", "PX_LAST", prev_date, date)$PX_LAST[2]
      self$un14 <- bdh("ULVLXHXL Index", "PX_LAST", prev_date, date)$PX_LAST[2]
      self$un26 <- bdh("ULVLWPWI Index", "PX_LAST", prev_date, date)$PX_LAST[2]
      self$un27 <- bdh("ULVLXMBG Index", "PX_LAST", prev_date, date)$PX_LAST[2]
    },

    


  )
)

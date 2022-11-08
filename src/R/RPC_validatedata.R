RPC_validatedata <- function(df, config) {
  vtg::log$info("Starting the initial validation")
  result = tryCatch({
    # Using the external_set flag to avoid splitting the data.
    config[["impute"]] <- FALSE
    df <- df <- data.frame(lapply(df , as.factor))

    return(lapply(df, levels))
  }, error = function(e) {
    msg <- "Error while running bayesian data validate"
    vtg::log$info(msg)
    vtg::log$info(e)
    return(list(
      "error_message" = paste(msg, e, sep=" ")
    ))
  })
return(result)
}

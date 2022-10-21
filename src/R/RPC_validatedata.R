RPC_validatedata <- function(df, config) {
  vtg::log$info("Starting the initial validation")
  requireNamespace("mice", quietly=T)
  result = tryCatch({
    # Using the external_set flag to avoid splitting the data.
    df <- factor_dataframe(df, config, external_set=TRUE)

    return(lapply(df, levels))
  }, error = function(e) {
    vtg::log$info("Error while running bayesian data validate")
    vtg::log$info(e)
    return(list(
      "error_message" = paste("Error running bayesian data validate:", e, sep=" ")
    ))
  })
return(result)
}

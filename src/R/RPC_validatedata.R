RPC_validatedata <- function(df, config) {
  vtg::log$info("Starting the initial validation")

  # Using the external_set flag to avoid splitting the data.
  df <- factor_dataframe(df, config, external_set=TRUE)

  return(lapply(df, levels))
}

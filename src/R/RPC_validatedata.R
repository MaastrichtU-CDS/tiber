RPC_validatedata <- function(df, config) {
  vtg::log$info("Starting the initial validation")

  df <- factor_dataframe(df, list())

  return(lapply(df, levels))
}

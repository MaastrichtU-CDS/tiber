RPC_histogram <- function(df, config) {
    vtg::log$info(
        "Creating the histogram for the necessary variables '{names(config)}'")

    histograms <- list()
    for (variable in names(config)) {
        bin_width <- config$variable[["bin_width"]]
        histograms <- c(
            histograms,
            variable=hist(
                df[variable],
                breaks=seq(from=(min(df[variable])%/%bin_width)*bin_width,
                           to=(1 + max(df[variable])%/%bin_width)*bin_width,
                           by=bin_width,
                           plot=FALSE
                )
            )
        )
    }
    vtg::log$info("Successfully obtained the histograms")
    return(histograms)
}

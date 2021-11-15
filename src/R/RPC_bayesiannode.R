RPC_bayesiannode <- function(df, config) {
    df <- data.frame(lapply(df , as.factor))

    vtg::log$info("Got {nrow(df)} rows in this node's data")

    parameters <- list(
        df[,-1],
        algorithm = "hc",
        R = 400,
        algorithm.args = list(score = "bde", restart = 5, perturb = 5)
    )
    if (!is.null(config)){
        for (arg in names(config)) {
            if (arg != "") {
                parameters[arg] = config[arg]
            }
        }
    }

    bootstrapped <- do.call(bnlearn::boot.strength, parameters)

    arcs <- bootstrapped[bootstrapped$from < bootstrapped$to,]

    arcs$sample <- nrow(df)

    vtg::log$info("Got {nrow(arcs)} arcs out of this")

    return(arcs)
}

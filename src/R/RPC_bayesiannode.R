RPC_bayesiannode <- function(df, config) {
    vtg::log$info("Got {nrow(df)} rows in this node's data")
    set_seed_config(config)

    df <- factor_dataframe(df, config)

    parameters <- list(
        df,
        algorithm = "hc",
        R = 400,
        algorithm.args = list(score = "bde", restart = 5, perturb = 5)
    )
    arc_config <- config[["arc_strength_args"]]
    if (!is.null(arc_config)){
        for (arg in names(arc_config)) {
            if (arg != "") {
                parameters[arg] = arc_config[arg]
            }
        }
    }

    arcs <- do.call(bnlearn::boot.strength, parameters)

    arcs$sample <- nrow(df)

    vtg::log$info("Got {nrow(arcs)} arcs out of this")
    return(arcs)
}

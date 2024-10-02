RPC_bayesianstructurelearning <- function(df, factors_by_column, config) {
    vtg::log$info("Starting bayesian structure learning")
    result = tryCatch({
        structural_em = "structural_em" %in% names(config) && config[["structural_em"]]
        if (structural_em) {
            set_seed_config(config)
            config_complete <- config
            config_complete[["impute_m"]] <- 3
            config_complete[["impute"]] <- TRUE
            config_complete[["bn_impute"]] <- FALSE
            out_complete <- factor_dataframe(df, config_complete, factors_by_column=factors_by_column)
        }
        set_seed_config(config)
        out <- factor_dataframe(df, config, factors_by_column=factors_by_column)
        df <- out$data
        vtg::log$info("Got {nrow(df)} rows in this node's data")
        parameters <- list(
            df
            # algorithm = "hc",
            # R = 400,
            # algorithm.args = list(score = "bde", restart = 5, perturb = 5)
        )
        arc_config <- config[["arc_strength_args"]]
        fitted <- NULL
        if (!is.null(arc_config)){
            for (arg in names(arc_config)) {
                if (arg != "") {
                    parameters[arg] = arc_config[arg]
                }
            }
        }
        if (structural_em) {
            vtg::log$info("Structural EM algorithm")
            start = bnlearn::bn.fit(
                bnlearn::empty.graph(names(out_complete$data)),
                out_complete$data,
                method = "bayes"
            )
            parameters[["start"]] <- start
            arcs <- do.call(bnlearn::structural.em, parameters)
            number_arcs <- nrow(arcs$arcs)
        } else {
            vtg::log$info("Boot strength algorithm")
            arcs <- do.call(bnlearn::boot.strength, parameters)
            arcs$sample <- nrow(df)
            number_arcs <- nrow(arcs)
        }
        vtg::log$info("Got {number_arcs} arcs out of this")

        return(list("arcs"=arcs, "pre_summary"=out$summary, "summary"=summary_stats(df)))
    }, error = function(e) {
        msg <- "Error while running bayesian structure learning"
        vtg::log$info(msg)
        vtg::log$info(e)
        return(list(
            "error_message" = paste(msg, e, sep=" ")
        ))
    })
    return(result)
}

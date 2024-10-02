RPC_bayesianparameterlearning <- function(df, nodes, arcs, factors_by_column, config) {
    vtg::log$info("Starting bayesian parameter learning")
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

        g <- bnlearn::empty.graph(nodes)
        bnlearn::arcs(g) <- arcs

        # Train the Bayesian network
        img_ss = 2
        bn_fit_method <- "bayes"
        if ("bn_fit_method" %in% names(config)) {
            vtg::log$info("BN fit method provided")
            bn_fit_method <- config[["bn_fit_method"]]
            if (bn_fit_method != "bayes") {
                img_ss <- 1
            }
        }
        debug <- FALSE
        if ("debug" %in% names(config)) {
            debug <- config[["debug"]]
        }
        keep_fitted <- FALSE
        if ("keep_fitted" %in% names(config)) {
            keep_fitted <- config[["keep_fitted"]]
        }
        max_iter <- 10
        if ("max_iter" %in% names(config)) {
            max_iter <- config[["max_iter"]]
        }
    
        vtg::log$info("Learning the parameters")
        if ("naive" %in% names(config)) {
            bn = bnlearn::naive.bayes(df, config[["pred_col"]])
            fitted = bnlearn::bn.fit(bn, df)
        } else if (structural_em) {
            # start <- config[["bn_fit_start"]]
            flat.prior <- bnlearn::bn.fit(
                g,
                out_complete$data,
                method = "bayes",
                iss=img_ss
            )
            fitted <- bnlearn::bn.fit(
                g,
                df,
                method=bn_fit_method,
                iss=img_ss,
                debug=debug,
                keep.fitted=keep_fitted,
                start=flat.prior,
                max.iter = max_iter
            )
        } else {
            fitted <- bnlearn::bn.fit(
                g,
                df,
                method=bn_fit_method,
                iss=img_ss,
                debug=debug,
                keep.fitted=keep_fitted
            )
        }
        # Count the number of cases for each node
        vtg::log$info("Count the number of cases")
        node_count <- list()
        for (node in names(fitted)) {
            parents <- names(dimnames(fitted[[node]]$prob))[-1]
            parents_grouped <- aggregate(df[[node]], by=df[parents], FUN=length, drop=FALSE)
            sample_length <- rep(
                parents_grouped[,ncol(parents_grouped)],
                each=dim(fitted[[node]]$prob)[[1]]
            )
            node_count[[node]] <- replace(
                sample_length, is.na(sample_length), img_ss)
        }

        vtg::log$info("Sending back the results")
        return(list(n_obs=nrow(df), model=fitted, count=node_count))
    }, error = function(e) {
        msg <- "Error while running bayesian parameter learning"
        vtg::log$info(msg)
        vtg::log$info(e)
        return(list(
            "error_message" = paste(msg, e, sep=" ")
        ))
    })
    return(result)
}

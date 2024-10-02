bayesian_network <- function(client, config, training_orgs, validation_orgs, external_validation, factors_by_column, pred_col) {
    # Set the organizations to train/validate
    # TODO: move setting the training organizations + external validation out
    client$collaboration$organizations <- training_orgs
    AllSitesInfo <- c()
    averaged_network <- list()
    structures <- list()
    if ("arc_structure" %in% names(config)) {
        # Use the structure provided in the arguments
        if ("arcs" %in% names(config[["arc_structure"]]) & "nodes" %in% names(config[["arc_structure"]])) {
            vtg::log$info("Arc structure provided")
            FinalArc <- as.data.frame(config[["arc_structure"]][["arcs"]])
            nodes <- config[["arc_structure"]][["nodes"]]
        } else {
            return(c("Malformed argument: arc_structure"))
        }
    } else {
        # Get the structure of the network
        vtg::log$info("Getting the arc strength from each node")
        responses <- client$call("bayesianstructurelearning", config)

        vtg::log$info("Got {length(responses)} responses")
        error_check = check_responses(responses)
        if (!is.null(error_check)) {
            return(error_check)
        }

        for (i in 1:length(responses)) {
            vtg::log$info("Returned DF {i} has {nrow(responses[[i]])} rows")
        }
        # Summary statistics obtained
        summary <- sapply(responses, `[`, "summary")
        pre_summary <- sapply(responses, `[`, "pre_summary")
        structures <- sapply(responses, `[`, "arcs")

        # Combine the structures
        allArcs <- data.frame()
        for (r in responses) {
            if ("std_strength" %in% names(config) && config[["std_strength"]]) {
                r$arcs$strength <- (r$arcs$strength - mean(r$arcs$strength)) / sd(r$arcs$strength)
            }
            allArcs <- rbind(allArcs, as.data.frame(r$arcs))
        }
        nodes <- unique(c(allArcs$to, allArcs$from))

        # Reinforce the arc strength if requested
        if ("reinforce" %in% names(config)) {
            reinforce_factor <- 0.5
            if ("reinforce_factor" %in% names(config)) {
                reinforce_factor <- config[["reinforce_factor"]]
            }
            reinforce_list <- config[["reinforce"]]
            arc_filter <- paste(allArcs$from, allArcs$to) %in%
                paste(reinforce_list[, "from"], reinforce_list[, "to"])
            allArcs[arc_filter, ]["strength"] <- allArcs[arc_filter, ]["strength"] + reinforce_factor
        }

        # Group the information and filter based on the threshold
        AllSitesInfo <-
            allArcs %>%
            dplyr::group_by(from, to) %>%
            dplyr::summarise(strength = weighted.mean(strength, sample),
                             direction = weighted.mean(direction, sample), .groups = 'drop')
        class(AllSitesInfo) <- c("bn.strength", "data.frame")
        attributes(AllSitesInfo)[["method"]] <- "bootstrap"
        attributes(AllSitesInfo)[["threshold"]] <- config[["weighted_strength"]] %>% if (is.null(.)) 0.2 else .
        attributes(AllSitesInfo)[["nodes"]] <- nodes
        # Get the
        averaged_network <- bnlearn::pdag2dag(
            bnlearn::averaged.network(AllSitesInfo),
            ordering=config[["ordering"]] %>% if (is.null(.)) nodes else .
        )
        FinalArc <- averaged_network$arcs
    }

    # If requested, send the arcs and skip training and validation
    if ("train" %in% names(config) && !(config[["train"]])) {
        return(list(
            "info"=AllSitesInfo,
            "averaged_network"=averaged_network,
            "summary"=summary,
            "pre_summary"=pre_summary
        ))
    }

    vtg::log$info("Ended up with {nrow(FinalArc)} arcs")
    if (nrow(FinalArc) == 0) {
        return(c("No arcs found."))
    }

    FinalStructure <- bnlearn::empty.graph(nodes)
    bnlearn::arcs(FinalStructure) <- FinalArc

    # Training the network
    vtg::log$info("Training the network")
    responses <- client$call(
        "bayesianparameterlearning",
        bnlearn::nodes(FinalStructure),
        as.data.frame(bnlearn::arcs(FinalStructure)),
        factors_by_column,
        config
    )
    error_check = check_responses(responses)
    if (!is.null(error_check)) {
        return(error_check)
    }

    # Weighted average to determine the parameters for the conditional probability tables
    vtg::log$info("Aggregate the conditional probability tables")
    prob_dist <- list()
    model_structure <- responses[[1]][["model"]]
    for (node in names(model_structure)) {
        n_samples <- Reduce('+', sapply(sapply(responses, "[", "count"), "[", node))
        weighted_prob <- lapply(
            1:length(responses),
            function(i) c(responses[[i]]$model[[node]][["prob"]]) * responses[[i]][["count"]][[node]] / n_samples
        )
        prob_dist[[node]] <- c(Reduce('+', weighted_prob))
        dim(prob_dist[[node]]) <- attributes(model_structure[[node]]$prob)$dim
        dimnames(prob_dist[[node]]) <- attributes(model_structure[[node]]$prob)$dimnames
    }
    aggregated_model <- bnlearn::custom.fit(FinalStructure, dist=prob_dist)

    # Result from the structure and parameter learning
    results <- list(
        "structure"=FinalStructure$arcs,
        "model"=aggregated_model,
        "info"=AllSitesInfo,
        "averaged_network"=averaged_network,
        "config"=config,
        "cohort_networks"=structures,
        "cohort_results"=responses
    )

    # Validate the training set
    vtg::log$info("Validating the network")
    responses <- client$call(
        "bayesianvalidate",
        aggregated_model,
        pred_col,
        factors_by_column,
        config
    )
    error_check = check_responses(responses)
    if (!is.null(error_check)) {
        return(error_check)
    }
    results[["training_results"]] <- evaluation(responses)

    if (external_validation) {
        vtg::log$info("Validating the network - external validation")
        client$collaboration$organizations <- validation_orgs
        responses <- client$call(
            "bayesianvalidate",
            aggregated_model,
            pred_col,
            factors_by_column,
            config,
            external_set=TRUE
        )
        error_check = check_responses(responses)
        if (!is.null(error_check)) {
            return(list("result"=results, "error"=error_check))
        }
        results <- c(results, "test_results"=evaluation(responses), "ext_val_org"=config[["val_org_id"]])
    }
    return(results)
}

bayesian <- function(client, pred_col, config=list()) {
    vtg::log$info("Running bayesian main algorithm")
    bayesian_result = tryCatch({
        pkg.name <- getPackageName()
        # sd <- names(Sys.getenv())
        # vtg::log$info(paste(sd, collapse=" - "))
        image.name <- Sys.getenv("IMAGE_NAME")
        # To run a specific docker image, you must specify it here due
        # to a problem with the R version of vtg
        image.name <- 'pmateus/tiber:2.0.0'
        config[["pred_col"]] <- pred_col
        print(config[["pred_col"]])
        client$set.task.image(
            image.name,
            task.name="bayesian"
        )

        # Send off a new container if necessary
        if (client$use.master.container) {
            vtg::log$info(
                "Running `bayesian` in master container using image '{image.name}'")
            result <- client$call("bayesian")
            return(result)
        }
        vtg::log$info("Running `bayesian` locally")

        # Initialize the seed in case it isn't provided
        seed <- sample(1:10000, 1)
        if ("seed" %in% names(config)) {
            seed <- config[["seed"]]
        } else {
            vtg::log$info("Using a random seed '{seed}'")
            config[["seed"]] <- seed
        }
        set.seed(seed)

        # Initialize the data split percentage for training and validation
        split <- 1
        if ("data_split" %in% names(config)) {
            if (config[["data_split"]] > 0 & config[["data_split"]] <= 1) {
                split <- config[["data_split"]]
            } else {
                vtg::log$info("Invalid data split provided")
            }
        }

        # Perform an initial validation
        factors_by_column <- list()
        num_org = length(client$collaboration$organizations)
        if (num_org > 1) {
            vtg::log$info("Perform initial validation")
            responses <- client$call("validatedata", config)
            vtg::log$info("Got {length(responses)} responses")
            error_check = check_responses(responses)
            if (!is.null(error_check)) {
                return(error_check)
            }

            # Evaluate the columns available at each site and
            # the factors for each column
            columns <- unique(unlist(sapply(responses, names)))
            column_warning <- c()
            column_factors_warning <- c()
            column_factors_by_node <- list()
            column_warning_by_node <- list()
            for (column in columns) {
                # TODO: responses 1 may not include the column
                column_factors <- c(
                    responses[[1]][names(responses[[1]]) %in% c(column)][[column]]
                )
                column_factors_by_node[[column]] <- list()
                i <- 1
                for (response in responses) {
                    if (column %in% names(response)) {
                        factors <- response[names(response) %in% c(column)][[column]]
                        column_factors_by_node[[column]][[i]] <- factors
                        if (!all(factors %in% column_factors) | length(factors) != length(column_factors)) {
                            column_factors_warning <- c(column_factors_warning, column)
                        }
                        column_factors <- unique(c(column_factors, factors))
                    } else {
                        column_warning <- c(column_warning, column)
                        if (length(column_warning_by_node) < i) {
                            column_warning_by_node[[i]] <- c(column)
                        } else {
                            column_warning_by_node[[i]] <- c(column_warning_by_node[[i]], column)
                        }
                    }
                    i <- i + 1
                }
                factors_by_column[[column]] <- column_factors
            }
            # Check the warnings
            if (!("ignore_warnings" %in% names(config))) {
                response <- list()
                if (length(column_warning) > 0) {
                    response <- c(
                        response,
                        "column_warning"= list(
                            warning="Different columns found in the nodes.",
                            columns=unique(column_warning),
                            columns_by_node=column_warning_by_node
                        )
                    )
                }
                if (length(column_factors_warning) > 0) {
                    response <- c(
                        response,
                        "factor_warning"=c(
                            warning="Different factors for the columns.",
                            factors_by_node=column_factors_by_node[column_factors_warning]
                        )
                    )
                }
                if (length(response) > 0) {
                    vtg::log$error("Warnings while validating the data.")
                    return(response)
                }
            }
        }

        # Define the nodes used for training and validation
        # By default, it will select a random node for validation
        collaboration_org_ids = client$collaboration$organizations
        external_validation <- TRUE
        if (!("val_org_id" %in% names(config)) & length(collaboration_org_ids) > 1) {
            org_id <- collaboration_org_ids[[sample(1:length(collaboration_org_ids), 1)]]$id
            vtg::log$info("Organization '{org_id}' selected for external validation")
            config[["val_org_id"]] <- org_id
        } else if (length(config[["val_org_id"]]) == 0) {
            vtg::log$info("External validation won't be performed")
            external_validation <- FALSE
        }

        # Update the client organizations according to the ones that will be used
        # for training
        training_orgs <- list()
        validation_orgs <- list()
        if (external_validation) {
            vtg::log$info("Setting up the client for training")
            client$collaboration$organizations <- list()
            for (collaboration in collaboration_org_ids) {
                if (collaboration$id %in% config[["val_org_id"]]) {
                    validation_orgs <- append(validation_orgs, list(collaboration))
                } else {
                    training_orgs <- append(training_orgs, list(collaboration))
                    #client$collaboration$organizations <- append(
                    #    client$collaboration$organizations, list(collaboration))
                }
            }
        } else {
            training_orgs <- collaboration_org_ids
        }

        results <- list()
        if ("k_fold" %in% names(config)) {
            folds <- c(1:config[['k_fold']])
            vtg::log$info("Running the k-fold for '{config[['k_fold']]}' folds")
            if ("f_fold_l" %in% names(config)) {
                folds <- config[["f_fold_l"]]
            }
            for (k in folds) {
                vtg::log$info("Fold '{k}'")
                config[["k"]] <- k
                results[[k]] <- bayesian_network(
                    client, config, training_orgs, validation_orgs, external_validation, factors_by_column, pred_col
                )
            }
        } else {
            vtg::log$info("Running bayesian network with data split")
            results <- bayesian_network(client, config, training_orgs, validation_orgs, external_validation, factors_by_column, pred_col)
        }
        return(results)
    }, error = function(e) {
        vtg::log$info("Error while running the master")
        vtg::log$info(e)
        return(list(
            "error_message" = paste("Error running the master:", e, sep=" ")
        ))
    })
    return(bayesian_result)
}

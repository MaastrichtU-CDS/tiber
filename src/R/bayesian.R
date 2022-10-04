bayesian <- function(client, pred_col, config=list()) {
    vtg::log$info("Running bayesian main algorithm")
    pkg.name <- getPackageName()
    # sd <- names(Sys.getenv())
    # vtg::log$info(paste(sd, collapse=" - "))
    image.name <- Sys.getenv("IMAGE_NAME")

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
        if (length(responses) == 0) {
            return(c("No responses obtained from the node, please check the logs"))
        }

        # Evaluate the columns available at each site and
        # the factors for each column
        columns <- unique(c(sapply(responses, names)))
        column_warning <- c()
        column_factors_warning <- c()
        for (column in columns) {
            column_factors <- c(
                responses[[1]][names(responses[[1]]) %in% c(column)][[column]]
            )
            for (response in responses) {
                if (column %in% names(response)) {
                    factors <- response[names(response) %in% c(column)][[column]]
                    if (!all(factors %in% column_factors) && length(factors) != length(column_factors)) {
                        column_factors_warning <- c(column_factors_warning, column)
                        column_factors <- unique(c(column_factors, factors))
                    }
                } else {
                    column_warning <- c(column_warning, column)
                }
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
                        columns=unique(column_warning)
                    )
                )
            }
            if (length(column_factors_warning) > 0) {
                response <- c(
                    response,
                    "factor_warning"=c(
                        warning="Different factors for the columns.",
                        columns=factors_by_column[column_factors_warning]
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
    validation_orgs <- list()
    if (external_validation) {
        vtg::log$info("Setting up the client for training")
        client$collaboration$organizations <- list()
        for (collaboration in collaboration_org_ids) {
            if (collaboration$id %in% config[["val_org_id"]]) {
                validation_orgs <- append(validation_orgs, list(collaboration))
            } else {
                client$collaboration$organizations <- append(
                    client$collaboration$organizations, list(collaboration))
            }
        }
    }

    AllSitesInfo <- c()
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
        responses <- client$call("bayesiannode", config)

        vtg::log$info("Got {length(responses)} responses")
        if (length(responses) == 0) {
            return(c("No responses obtained from the node, please check the logs"))
        }

        for (i in 1:length(responses)) {
            vtg::log$info("Returned DF {i} has {nrow(responses[[i]])} rows")
        }

        # Combine the structures
        allArcs <- data.frame()
        for (r in responses) {
            allArcs <- rbind(allArcs, as.data.frame(r))
        }
        nodes <- unique(c(allArcs$to, allArcs$from))

        # Reinforce the arc strength if requested
        if ("reinforce" %in% names(config)) {
            reinforce_factor <- 0.5
            if ("reinforce_factor" %in% names(config)) {
                reinforce_factor <- config[["reinforce_factor"]]
            }
            reinforce_list <- config[["reinforce"]]
            arc_filter <-paste(allArcs$from, allArcs$to) %in%
                paste(reinforce_list[, "from"], reinforce_list[, "to"])
            allArcs[arc_filter, ]["strength"] <- allArcs[arc_filter, ]["strength"] + reinforce_factor
        }

        # Group the information and filter based on the threshold
        AllSitesInfo <-
            allArcs %>%
            dplyr::group_by(from, to) %>%
            dplyr::summarise(weighted_strength = weighted.mean(strength, sample),
                             weighted_direction = weighted.mean(direction, sample), .groups = 'drop')

        FinalArc <- as.data.frame(subset(
            AllSitesInfo,
            weighted_strength >= (
                config[["weighted_strength"]] %>% if (is.null(.)) 0.2 else .),
            select = c(from, to)
        ))
    }

    # If requested, send the arcs and skip training and validation
    if ("train" %in% names(config)) {
        if (!(config[["train"]])) {
            return(list("arcs"=FinalArc, "info"=AllSitesInfo, "nodes"=nodes))
        }
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
        "bayesiantrain",
        bnlearn::nodes(FinalStructure),
        as.data.frame(bnlearn::arcs(FinalStructure)),
        factors_by_column,
        config
    )

    # Weighted average to determine the parameters for the conditional probability tables
    vtg::log$info("Aggregate the conditional probability tables")
    #total_samples <- Reduce('+', sapply(responses, "[", "n_obs"))
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

    # Validate the training set
    vtg::log$info("Validating the network - training")
    responses <- client$call(
        "bayesianvalidate",
        aggregated_model,
        pred_col,
        factors_by_column,
        config,
        train_set=TRUE
    )
    results <- list(
        "structure"=FinalStructure$arcs,
        "model"=aggregated_model,
        "training_results"=evaluation(responses),
        "config"=config
    )

    # Validate the testing set
    if (split < 1) {
        vtg::log$info("Validating the network - validation")
        responses <- client$call(
            "bayesianvalidate",
            aggregated_model,
            pred_col,
            factors_by_column,
            config
        )

        results <- c(results, "val_results"=evaluation(responses))
    }
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
        results <- c(results, "test_results"=evaluation(responses), "ext_val_org"=config[["val_org_id"]])
    }

    return(results)
}

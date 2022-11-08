get_data_split <- function(config, size) {
    split <- 1
    if ("data_split" %in% names(config)) {
        split <- config[["data_split"]]
    }
    dt = sort(sample(size, size * split))
    return(dt)
}

factor_dataframe <- function(df, config, train=TRUE, external_set=FALSE, factors_by_column=list(), validating=FALSE) {
    pred_col <- config[["pred_col"]]
    nan_limit <- 1
    if ("nan_limit" %in% names(config)) {
        nan_limit <- config[["nan_limit"]]
    }
    # Exclude columns that won't be used
    if ("exclude" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude"]])]
    }
    # Split the data according to the options
    if (!external_set) {
        # Perform the split for the rows that have an Outcome
        # All columns without an outcome will be used for training
        df_pred <- df[!is.na(df[pred_col]),]
        split_pred <- get_data_split(config, nrow(df_pred))
        if (train) {
            if ("impute" %in% names(config) && config[["impute"]]) {
                df <- rbind(df_pred[split_pred,], df[is.na(df[pred_col]),])
            } else {
                df <- df_pred[split_pred,]
            }
        } else {
            df <- df_pred[-split_pred,]
        }
    } else {
        # Only the rows with the outcome available
        df <- df[!is.na(df[pred_col]),]
    }
    df <- data.frame(lapply(df , as.factor))
    # Imputation
    if (validating) {
        cnt_na <- apply(df, 1, function(z) sum(is.na(z)))
        df <- df[cnt_na < nan_limit, ]
    }
    if (("impute" %in% names(config) && config[["impute"]]) || (validating && nan_limit > 1)) {
        m <- 5
        if ("impute_m" %in% names(config)) {
            m <- config[["impute_m"]]
        }
        df <- mice::complete(mice::mice(df, m=m), "stacked")
    } else {
        df <- na.omit(df)
    }
    # Factors can only be included after imputation
    if (length(factors_by_column) > 0) {
        for (column in names(factors_by_column)) {
            if (column %in% names(df)) {
                levels(df[[column]]) <- factor(factors_by_column[[column]])
            }
        }
    }
    return(df)
}

parse_roc <- function(partial_rocs) {
    # Preparing the input
    fpr <- sapply(partial_rocs, `[`, "fpr")
    tpr <- sapply(partial_rocs, `[`, "tpr")
    thresholds <- sapply(partial_rocs, `[`, "thresholds")
    negative_count <- unname(unlist(sapply(partial_rocs, `[`, "negative_count")))
    total_count <- unname(unlist(sapply(partial_rocs, `[`, "total_count")))
    partial_auc <- unname(unlist(sapply(partial_rocs, `[`, "auc")))

    # Compute the global ROC curve for the model
    roc_aggregated <- ROCaggregator::roc_curve(fpr, tpr, thresholds, negative_count, total_count)

    # Calculate the AUC
    roc_auc <- pracma::trapz(roc_aggregated$fpr, roc_aggregated$tpr)

    sprintf("ROC AUC aggregated from each node's results: %f", roc_auc)

    # Calculate the precision-recall
    precision_recall_aggregated <- ROCaggregator::precision_recall_curve(
        fpr, tpr, thresholds, negative_count, total_count)

    # Calculate the precision-recall AUC
    precision_recall_auc <- -(pracma::trapz(
        precision_recall_aggregated$recall, precision_recall_aggregated$pre))

    return(list(
        roc=list(roc=roc_aggregated, auc=roc_auc, partial_auc=partial_auc),
        pr=list(pr=precision_recall_aggregated, auc=precision_recall_auc)
    ))
}

evaluation <- function(responses) {
    eval_by_set = list()
    for (evaluation_set in names(responses[[1]])) {
        evaluation_metrics <- sapply(responses, `[`, evaluation_set)
        eval <- list("num_samples" = sapply(evaluation_metrics, `[`, "n_obs"))
        if ("roc" %in% names(evaluation_metrics[[1]])) {
            eval <- c(eval, "metrics"=parse_roc(sapply(evaluation_metrics, `[`, "roc")))
        }
        eval <- c(eval, "cm"=list(Reduce('+', sapply(evaluation_metrics, `[`, "cm"))))
        eval_by_set[[evaluation_set]] <- eval
    }
    return(eval_by_set)
}

set_seed_config <- function(config) {
    if ("seed" %in% names(config)) {
        set.seed(config[["seed"]])
    }
}

check_responses <- function(responses) {
    if (length(responses) == 0) {
        return(c("No responses obtained from the node, please check the logs"))
    }
    error_message = list()
    for (i in 1:length(responses)) {
        if ("error_message" %in% names(responses[[i]])) {
            error_message <- c(error_message, responses[[i]][["error_message"]])
        }
    }
    if (length(error_message) > 0) {
        return(error_message)
    }
}

get_data_split <- function(config, size) {
    split <- 1
    if ("data_split" %in% names(config)) {
        split <- config[["data_split"]]
    }
    dt = sort(sample(size, size * split))
    return(dt)
}

factor_dataframe <- function(df, config, train=TRUE, external_set=FALSE, factors_by_column=list()) {
    df <- data.frame(lapply(df , as.factor))
    if (length(factors_by_column) > 0) {
        for (column in names(factors_by_column)) {
            if (column %in% names(df)) {
                levels(df[[column]]) <- factor(factors_by_column[[column]])
            }
        }
    }
    if (!external_set) {
        dt <- get_data_split(config, nrow(df))
        if (train) {
            df <- df[dt,]
        } else {
            df <- df[-dt,]
        }
    }
    if ("exclude" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude"]])]
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
    eval <- list("num_samples" = sapply(responses, `[`, "n_obs"))
    if ("roc" %in% names(responses[[1]])) {
        eval <- c(eval, "metrics"=parse_roc(sapply(responses, `[`, "roc")))
    }
    eval <- c(eval, "cm"=list(Reduce('+', sapply(responses, `[`, "cm"))))
    return(eval)
}

set_seed_config <- function(config) {
    if ("seed" %in% names(config)) {
        set.seed(config[["seed"]])
    }
}

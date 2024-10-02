summary_stats <- function(df) {
    metrics = list(
        "n" = nrow(df)
    )
    df <- data.frame(lapply(df , as.factor))
    for (variable in colnames(df)) {
        if (is.factor(df[[variable]]) && length(levels(as.factor(df[[variable]]))) < 5) {
            distribution = table(df[[variable]])
            distribution[distribution < 5] = 5
            metrics[[variable]] <- list(
                "null_values" = sum(is.na(df[[variable]])),
                "stats" = distribution
            )
        }
    }
    return(metrics)
}

factor_outcome <- function(pred_col) {
    return (droplevels(factor(
        pred_col,
        levels=c("no", "0", 0, "yes", "1", 1),
        labels=c(0, 0, 0, 1, 1, 1)
    )))
}

get_indexes <- function(size, folds, k, sampler=c()) {
    interval = round(size/folds)
    start = if (k == 1) 1 else 1 + interval * (k - 1)
    end = if (k == folds) size else interval * k
    if (length(sampler) > 0) {
        indexes = sample(sampler, size)
    } else {
        indexes = sample(size, size)
    }
    dt = indexes[!indexes %in% indexes[start:end]]
    print(dt)
    return(dt)
}

get_data_split <- function(config, size, pos_ind, neg_ind) {
    dt <- c()
    print(config[["k_fold_p"]])
    if ("k_fold_p" %in% names(config) && config[["k_fold_p"]] == TRUE ) {
        print("K fold - partitioned")
        folds <- config[["k_fold"]]
        k <- config[["k"]]
        print(k)
        print(pos_ind)
        print(neg_ind)
        dt <- c(
            get_indexes(length(pos_ind), folds, k, pos_ind),
            get_indexes(length(neg_ind), folds, k, neg_ind)
        )
    } else if ("k_fold" %in% names(config) && "k" %in% names(config)) {
        print("K fold")
        folds <- config[["k_fold"]]
        k <- config[["k"]]
        dt <- get_indexes(size, folds, k)
        #interval = round(size/folds)
        #start = if (k == 1) 1 else 1 + interval * (k - 1)
        #end = if (k == folds) size else interval * k
        #indexes = sample(size, size)
        #dt = indexes[!indexes %in% indexes[start:end]]
    } else {
        split <- 1
        if ("data_split" %in% names(config)) {
            split <- config[["data_split"]]
        }
        dt = sort(sample(size, size * split))
    }
    return(dt)
}

factor_dataframe <- function(df, config, train=TRUE, external_set=FALSE, factors_by_column=list(), validating=FALSE, bn_impute=FALSE, imputation_model=NULL) {
    age_tertiles_rome <- c(20.00,	60.3,	71.0,	100)
    age_quartiles_rome <- c(20.00,	57.0,	66.0,	72.0,	100)
    age_tertiles_maastro <- c(20.00, 65.00,	75.00,	100)
    age_quartiles_maastro <- c(20.00,	62.00,	70.00,	77.00,	100)
    age_tertiles_global <- (age_tertiles_maastro * 432 + age_tertiles_rome * 470) / (432 + 470)
    age_quartiles_global <- (age_quartiles_maastro * 432 + age_quartiles_rome * 470) / (432 + 470)
    df$age_discretised <- cut(as.numeric(df$Age), age_tertiles_global, include.lowest = TRUE)
    df$age_discretised_q <- cut(as.numeric(df$Age), age_quartiles_global, include.lowest = TRUE)

    gtv_tertiles_rome <- c(2000,	21246.634,	47868.789,	800000)
    gtv_quartiles_rome <- c(2000,	17971.273,	32054.622,	52764.269,	800000)
    gtv_tertiles_maastro <- c(2000,	39085.259,	66849.359,	800000)
    gtv_quartiles_maastro <- c(2000,	31991.594,	50794.598,	79404.664,	800000)

    gtv_tertiles_global <- (gtv_tertiles_maastro * 426 + gtv_tertiles_rome * 86) / (426 + 86)
    gtv_quartiles_global <- (gtv_quartiles_maastro * 426 + gtv_quartiles_rome * 86) / (426 + 86)
    df$gtv_discretised <- cut(as.numeric(df$GTV), gtv_tertiles_global, c("small", "medium", "large"), include.lowest = TRUE)
    df$gtv_discretised_q <- cut(as.numeric(df$GTV), gtv_quartiles_global, c("micro", "small", "medium", "large"), include.lowest = TRUE)

    gtv_tertiles <- quantile(as.numeric(df$GTV), probs = seq(from = 0, to = 1, by = 0.33), na.rm = TRUE)
    gtv_quartiles <- quantile(as.numeric(df$GTV), probs = seq(from = 0, to = 1, by = 0.25), na.rm = TRUE)
    gtv_tertiles <- c(3000, gtv_tertiles[[2]], gtv_tertiles[[3]], 800000)
    gtv_quartiles <- c(3000, gtv_quartiles[[2]], gtv_quartiles[[3]], gtv_quartiles[[4]], 800000)
    df$gtv_discretised_local_q <- cut(as.numeric(df$GTV), gtv_quartiles, c("micro", "small", "medium", "large"), include.lowest = TRUE)
    df$gtv_discretised_local <- cut(as.numeric(df$GTV), gtv_tertiles, c("small", "medium", "large"), include.lowest = TRUE)

    distance_tertiles_rome <- c(30.00,	60.0,	85.0,	230.00)
    distance_quartiles_rome <- c(30.00,	50.0,	70.0,	90.0,	230.00)
    distance_tertiles_maastro <- c(30.00,	55.4,	90.0,	230.00)
    distance_quartiles_maastro <- c(30.00,	45.0,	70.0,	100.0,	230.00)
    distance_tertiles_global <- (distance_tertiles_maastro * 375 + distance_tertiles_rome * 447) / (375 + 447)
    distance_quartiles_global <- (distance_quartiles_maastro * 375 + distance_quartiles_rome * 447) / (375 + 447)
    df$distance_discretised <- cut(as.numeric(df$distance), distance_tertiles_global, c("small", "medium", "large"), include.lowest = TRUE)
    df$distance_discretised_q <- cut(as.numeric(df$distance), distance_quartiles_global, c("micro", "small", "medium", "large"), include.lowest = TRUE)

    # Set the outcome column
    pred_col <- config[["pred_col"]]

    # Differences between the nodes' values
    df[[pred_col]] <- factor_outcome(df[[pred_col]])

    # Convert to factors
    df <- data.frame(lapply(df , as.factor))

    # WAS included?
    include_was <- "include_was" %in% names(config) && config[["include_was"]]
    if (include_was) {
        df[["CR_or_WaS"]] <- factor_outcome(df[["CR_or_WaS"]])
        df[["Complete_response"]] <- factor_outcome(df[["Complete_response"]])
        outcome_or_was <- (df[["CR_or_WaS"]] == 1 & is.na(df[["Complete_response"]])) | !is.na(df[[pred_col]])
        was <- df[["CR_or_WaS"]] == 1 & is.na(df[["Complete_response"]])
        was <- ifelse(
            is.na(was),
            FALSE,
            was
        )
        was_samples <- sum(was == TRUE, na.rm = TRUE)
        if (was_samples > 0) {
            print("Impute WAS")
            print(was_samples)
            if ("imputation_model" %in% names(config)) {
                # TODO: model can't be applied without having the same factors/columns
                df_pre <- df
                df_pre[[pred_col]] <- factor_outcome(df_pre[[pred_col]])
                df_pre <- df_pre[,!(names(df_pre) %in% config[["exclude"]])]
                if (length(factors_by_column) > 0) {
                    print("Including the factors")
                    for (column in names(factors_by_column)) {
                        if (column %in% names(df_pre) && column != pred_col) {
                            df_pre[[column]] <- factor(df_pre[[column]], levels = factors_by_column[[column]])
                        }
                    }
                }
                print(nrow(df))
                print(sum(!is.na(df[[pred_col]])))
                print(sum(df[[pred_col]] == 1, na.rm = TRUE))
                print("Imputing WAS")
                imputed_df <- bnlearn::impute(
                    config[["imputation_model"]],
                    data = df_pre,
                    method="exact",
                    debug = TRUE,
                    strict=FALSE
                )
                print(sum(df[[pred_col]] == 1, na.rm = TRUE))
                print(imputed_df[was, ][[pred_col]])
                # print("Prediction")
                # preds <- predict(
                #     config[["imputation_model"]],
                #     node=pred_col,
                #     data=imputed_df,
                #     method="exact",
                #     prob=TRUE
                # )
                # print(preds[was])
                df[[pred_col]] <- ifelse(
                    was,
                    as.numeric(imputed_df[[pred_col]]) - 1,
                    df[[pred_col]]
                )
                df[[pred_col]] <- factor_outcome(df[[pred_col]])
            } else {
                df <- df[outcome_or_was, ]
            }
            print("Including WAS, number of samples:")
            print(sum(!is.na(outcome_or_was)))
        }
    }

    # Limit for the number of missing fields for each patient
    nan_limit <- 1
    if ("nan_limit" %in% names(config)) {
        nan_limit <- config[["nan_limit"]]
    }
    # Exclude columns that won't be used
    if ("exclude" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude"]])]
    }
    summary <- summary_stats(df)

    # Only include rows with a number of missing fields within the limit
    #if (validating) {
    cnt_na <- apply(df, 1, function(z) sum(is.na(z)))
    df <- df[cnt_na <= nan_limit, ]
    #}

    # Split the data according to the options
    if (!external_set) {
        # Perform the split for the rows that have an Outcome
        # All columns without an outcome will be used for training
        df_pred <- df[!is.na(df[pred_col]),]
        # TODO: remove the magic numbers
        split_pred <- get_data_split(config, nrow(df_pred), which(df_pred[pred_col] == 1), which(df_pred[pred_col] == 0))
        print(split_pred)
        # split_pred <- sample(split_pred, length(split_pred))
        if (train) {
            # TODO: modify variable name for impute_n (represents the % of rows without outcome to include)
            if (("impute" %in% names(config) && config[["impute"]]) || ("impute_n" %in% names(config) && config[["impute_n"]])) {
                print("Prepare dataset for imputation or merging with rows without outcome.")
                df_no_pred <- df[is.na(df[pred_col]),]
                fraction_without_pred <- 1
                if ("impute_n" %in% names(config)) {
                    fraction_without_pred <- config[["impute_n"]]
                }
                df <- rbind(df_pred[split_pred,], df_no_pred[sample(nrow(df_no_pred), round(nrow(df_no_pred) * fraction_without_pred)), ])
            } else {
                df <- df_pred[split_pred,]
            }
            #df <- df[sample(nrow(df)),]
        } else {
            df <- df_pred[-split_pred,]
        }
    } else {
        # Only the rows with the outcome available
        df <- df[!is.na(df[pred_col]),]
    }

    # Variables to be excluded after the pre-processing
    if ("exclude_pos" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude_pos"]])]
    }

    if (nrow(df) > 0 && (("impute" %in% names(config) && config[["impute"]]) || (validating && nan_limit > 0 && !bn_impute))) {
        print("Imputation performed.")
        m <- 5
        if ("impute_m" %in% names(config)) {
            m <- config[["impute_m"]]
        }
        if (is.null(imputation_model)) {
            print("New imputation model")
            imputation_model <- mice::mice(df, m=m, maxit = 30)
            df <- mice::complete(imputation_model, action="stacked")
        } else {
            # Validation set can create the following error:
            # Error in mice.impute.polyreg(y = structure(c(3L, 2L, 2L, 2L, 5L, 2L, 2L, : dims [product 4] do not match the length of object [5]\n
            # Not enough data to represent ...
            imputation_model.mids <- mice::mice.mids(imputation_model, newdata = df, maxit = 1)
            df <- mice::complete(imputation_model.mids, action="stacked")
            #df_full <- mice::complete(imputation_model.mids, action="stacked")
            #df <- mice::mice.reuse(imputation_model, newdata = df, maxit = 0)
            # df_indexes <- c()
            # for (i in 1:m) {
            #     start <- 1 + i * nrow(imputation_model$data) + (i - 1) * nrow(df)
            #     end <- start + nrow(df) - 1
            #     df_indexes <- c(df_indexes, c(start:end))
            # }
            # df <- df_full[df_indexes, ]
        }
        # imputation_model.mids <- mice::mice.mids(imputation_model, newdata = df, maxit = 0)
    }

    # Imputed afterwards with bnlearn or keep the missing values (for methods that
    # can handle)
    if (!bn_impute && (!("keep_nan" %in% names(config)) || !config[["keep_nan"]])) {
        print("Ommiting the rows with missing fields")
        df <- na.omit(df)
    }
    # Factors can only be included after imputation
    if (length(factors_by_column) > 0) {
        print("Including the factors")
        for (column in names(factors_by_column)) {
            if (column %in% names(df) && column != pred_col) {
                # Guaranteeing the same order to avoid switching the categories
                df[[column]] <- factor(df[[column]], levels = factors_by_column[[column]])
                #levels(df[[column]]) <- factor(
                #    unique(c(levels(df[[column]]), factors_by_column[[column]]))
                #)
            }
        }
    }
    print(paste("Number of rows:", nrow(df)))
    print(paste("Number of rows with missing data:", sum(is.na(df))))
    print(paste("Number of rows with missing outcome:", sum(is.na(df[[pred_col]]))))
    return(list("data"=df, "imputation_model"=imputation_model, "summary"=summary))
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
        eval <- c(eval, list("na_pred" = sapply(evaluation_metrics, `[`, "na_pred")))
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

factor_dataframe <- function(df, config) {
    df <- data.frame(lapply(df , as.factor))
    if ("exclude" %in% names(config)) {
        df <- df[,!(names(df) %in% config[["exclude"]])]
    }
    return(df)
}

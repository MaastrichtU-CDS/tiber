setup.client <- function() {
  # Define parameters
  username <- ""
  password <- ""
  host <- 'http://111.111.111.111:111'
  api_path <- '/api'

  # Create the client
  client <- vtg::Client$new(host, api_path=api_path)
  client$authenticate(username, password)

  return(client)
}

# Create a client
client <- setup.client()

# Get a list of available collaborations
print( client$getCollaborations() )

# Select a collaboration
client$setCollaborationId(1)

client$set.task.image(
  'pmateus/tiber:2.0.0',
  task.name="bayesian"
)

pred_col <- "Near_complete_response"
age_var <- "age_discretised"
distance_var <- "distance_discretised"
gtv_var <-  "gtv_discretised_local"

config <- list(
  arc_strength_args = list(
    # algorithm = "hc",
    R = 100,
    algorithm.args = list(
      score = "bde",
      restart = 5,
      perturb = 5,
      blacklist = matrix(
        c(
          pred_col, age_var,
          pred_col, distance_var,
          pred_col, "Gender",
          pred_col, "M",
          pred_col, "N",
          pred_col, "T",
          pred_col, "MRF_involved",
          pred_col, "treatment_type",
          pred_col, gtv_var,
          pred_col, "WHO",
          gtv_var, "Gender",
          "treatment_type", "Gender",
          distance_var, "Gender",
          "WHO", "Gender",
          "MRF_involved", "Gender",
          "N", "Gender",
          "T", "Gender",
          "M", "Gender",
          age_var, "Gender",
          "Gender", age_var,
          gtv_var, age_var,
          "treatment_type", age_var,
          "WHO", age_var,
          "MRF_involved", age_var,
          "N", age_var,
          "T", age_var,
          "M", age_var,
          "N", age_var,
          distance_var, age_var,
          "treatment_type", age_var,
          "treatment_type", distance_var,
          "treatment_type", "Gender",
          "treatment_type", "M",
          "treatment_type", "T",
          "treatment_type", "N",
          "treatment_type", "MRF_involved",
          "treatment_type", gtv_var
        ),
        ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))
      )
    ),
    debug = TRUE
  ),
  # threshold for the arc strength
  weighted_strength = 0.5,
  # normalize the arc strength by cohort
  std_strength = FALSE,
  # List of cohorts to be used as holdout-set/external validation
  # An empty list -> all cohorts are included in the training
  val_org_id = c(),
  # Exclude columns from the dataframe
  exclude = c("gtv_discretised_local_q", "gtv_discretised", "gtv_discretised_q",
              "age_discretised_q",
              "distance_discretised_q",
              "distance", "GTV", "Age", "tumour_length",
              "CR_or_WaS", "NCR_or_CR_or_WaS", "Complete_response", "NCR_or_CR", "NCR_CR"),
  # Training and validation split done at cohort level
  data_split = 0.7,
  # To perform k-fold
  # Number of folds
  # k_fold = 5,
  # Stratified k-fold
  # k_fold_p = TRUE,
  # Run the algorithm for one or more specific folds (without this
  # argument, it will run the 5 folds)
  # f_fold_l = c(1, 2, 3, 4, 5),
  # seed = 82,
  train = TRUE,
  impute = TRUE,
  # Portion of data without outcome that should be included
  impute_n = 0,
  # Number of multiple imputations
  impute_m = 3,
  # Maximum number of missing fields (a patient with more missing fields than
  # this limit will be excluded)
  nan_limit = 3,
  # Use a bayesian model to impute the missing data fields
  # Only applicable when assessing the performance (RCP_bayesianvalidate)
  bn_impute = FALSE,
  # Keep rows with missing data after the pre-processing?
  # Useful when performing bnlearn impute after pre-processing the dataset.
  keep_nan=FALSE,
  # Reinforce certain arcs by providing a structure and the additional strength
  # reinforce=bn_arcs,
  # reinforce_factor=0.3,
  ignore_warnings = TRUE
  # arc_structure=bn_arcs
  # discretize = list(
  #   "variable" = list(
  #    method = "quantile",
  #    bin_width = 2
  #  )
  #)
)

# Run the bayesian network algorithm
client$use.master.container <- TRUE
client$data_format <- "RDS"
result <- client$call('bayesian', pred_col, config)

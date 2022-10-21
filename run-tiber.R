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
  'pmateus/tiber',
  task.name="bayesian"
)

# library(bnlearn)
# bn_struct <- bnlearn::read.net("network.net")
# bn_arcs <- list()
# for(node in bn_struct)
# {
#   for (child in node$children)
#   {
#     bn_arcs <- bn_arcs %>% rbind(data.frame(from=node$node, to=child))
#   }
# }

config <- list(
  arc_strength_args = list(
    # algorithm = "hc",
    R = 50,
    algorithm.args = list(
      score = "bde",
      restart = 5,
      perturb = 5,
      blacklist = matrix(
        c(
          "Outcome", "age_discretised",
          "Outcome", "distance_discretised",
          "Outcome", "Gender",
          "Outcome", "M",
          "Outcome", "T",
          "Outcome", "N",
          "Outcome", "MRF_involved",
          "Outcome", "treatment_type",
          "Outcome", "gtv_discretised_local",
          "Outcome", "WHO"
        ),
        ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))
      )
    ),
    debug = TRUE
  ),
  weighted_strength = 0.5,
  val_org_id = c(),
  exclude = c("distance", "GTV", "Age", "gtv_discretised_global"),
  data_split = 0.7,
  # seed = 82,
  train = TRUE,
  impute = TRUE,
  # impute_m = 5,
  # reinforce=bn_arcs,
  # reinforce=matrix(c("Node1", "Node2"), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to"))),
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
result <- client$call('bayesian', 'Outcome', config)

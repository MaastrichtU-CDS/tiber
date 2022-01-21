setup.client <- function() {
  # Define parameters
  username <- "node1-user"
  password <- "node1-password"
  host <- 'http://localhost:5000'
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

# Possibility to directly configure options, such as
# the number of bootstrap replicates.
config <- list(
  arc_strength_args = list(
    # algorithm = "hc",
    R = 300
    # algorithm.args = list(score = "bde", restart = 5, perturb = 5)
  )
  #weighted_strength = 0.3
  #val_org_id = list()
  #exclude = c('id')
  #predictions = FALSE
)

# Run the bayesian network algorithm
client$use.master.container <- TRUE
client$data_format <- "RDS"
result <- client$call('bayesian', 'PN', config)

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

# Should output something like this:
#   id     name
# 1  1 ZEPPELIN
# 2  2 PIPELINE

# Select a collaboration
client$setCollaborationId(1)

client$set.task.image(
  'jaspersnel/tiber',
  task.name="bayesian"
)

# Run the bayesian network algorithm
client$setUseMasterContainer(T)
result <- client$call('bayesian', 'PN')

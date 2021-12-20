library(pins)

# Connect to local board
board <- pins::board_local()

# Pin a dataset to the local board 
pins::pin_write(
  board = board, 
  x = iris[1:30, ], 
  name = "iris_test_1", 
  type = "rds"
)

# Pin a dataset to the local board 
pins::pin_write(
  board = board, 
  x = iris[31:40, ], 
  name = "iris_test_2", 
  type = "rds"
)

# Read the current 'iris_test' pins information into a single data frame
my_pins <- pins::pin_search(
  board = board, 
  search = "iris_test"
)

my_pins

# Read all of the pins into a single data frame
df <- my_pins %>% 
  split(.$name) %>% 
  purrr::map_dfr(
    function(x) pins::pin_read(board = board, name = x$name), 
    .id = "source"
  )

df

# Remove the pinned datasets from the board
pins::pin_delete(
  board = board, 
  names = my_pins$name
)

# List the current pins
pins::pin_list(board = board)




library(pins)

# Connect to local board (this will *create* a local board if you don't already
# have one)
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

# List the pins with names that match "iris_test" 
my_pins <- pins::pin_search(
  board = board, 
  search = "iris_test"
)

my_pins

# Read both of the pins into a single, concatenated data frame
df <- my_pins %>% 
  split(.$name) %>% 
  purrr::map_dfr(
    function(x) pins::pin_read(board = board, name = x$name), 
    .id = "source"
  ) %>% 
  tibble::as_tibble()

df

# Remove the pinned datasets from the board
pins::pin_delete(
  board = board, 
  names = my_pins$name
)

# List the current pins
pins::pin_list(board = board)

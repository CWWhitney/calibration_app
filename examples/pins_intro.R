# This is an overview for working with pins in the rsconnect system

library(pins)

# Connect to a board (ours is on rsconnect)
# try other auth options (auth = "envvar") to get this running in rsconnect
board <- pins::board_rsconnect(auth = "rsconnect") 

# Connect to local board (this will *create* a local board if you don't already
# have one)
# board <- pins::board_local() 

# # Pin a dataset to the local board 
# pins::pin_write(
#   board = board, 
#   x = iris[1:30, ], 
#   name = "iris_test_1", 
#   type = "rds"
# )

# # Pin a dataset to the local board 
# pins::pin_write(
#   board = board, 
#   x = iris[31:40, ], 
#   name = "iris_test_2", 
#   type = "rds"
# )

# List the pins with names that match "iris_test" 
my_pins <- pins::pin_search(
  board = board, 
  search = "range_" #for example get all the data where people did the range questions
  #change this search argument according to the part of the 
  #workshop to be assessed _range 
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

df #downlod this as a .csv or other file format 
# this can be used for further analysis

# Remove the pinned datasets from the board
# run this code each time you want to clear the board that was named 
pins::pin_delete(
  board = board, 
  names = my_pins$name
)

# List the current pins
pins::pin_list(board = board)

library(pins)

# Connect to local board
board <- pins::board_local()

# Pin a dataset to the local board 
pins::pin_write(
  board = board, 
  x = iris, 
  name = "iris_test", 
  type = "rds"
)

# View the current pins on the board
pins::pin_list(board)

# Remove the pinned dataset from the board
pins::pin_delete(
  board = board, 
  names = "iris_test"
)
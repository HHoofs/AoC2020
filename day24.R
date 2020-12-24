inp = readLines('inp/day24_inp.txt')

OPTIONS = c('e', 'se', 'sw', 'w', 'nw', 'ne')

commands = vector(mode='list', length = length(inp))

for (line in seq(length(inp))){
  seperated = strsplit(inp[line], '')[[1]]
  i = 1
  while(i <= length(seperated)){
    if(seperated[i] %in% OPTIONS){
      commands[[line]] = c(commands[[line]], seperated[i])
      i = i + 1
    } else {
      commands[[line]] = c(commands[[line]], paste(seperated[i], seperated[i+1], sep=''))
      i = i + 2
    }
  }
}

grid = matrix(NA, ncol = 800, nrow = 800)

for(row in seq(1, 800)){
  if(row%%2 == 0){
    start = 2
  } else {
    start = 1
  }
  for(col in seq(start,800,2)){
    grid[row, col] = 0
  }
}

SP = c(400,400)

move <- function(cur_coords, direction){
  c('e', 'se', 'sw', 'w', 'nw', 'ne')
  if(direction == 'e'){
    return(cur_coords + c(0,2))
  } else 
    if(direction == 'w'){
      return(cur_coords + c(0,-2))
    } else
      if(direction == 'se'){
        return(cur_coords + c(1,1))
      } else
        if(direction == 'sw'){
          return(cur_coords + c(1,-1))
        } else
          if(direction == 'nw'){
            return(cur_coords + c(-1,-1))
          } else
            if(direction == 'ne'){
              return(cur_coords + c(-1,1))
            }
}

for (command in commands) {
  coord = SP
  for(step in command){
    coord = move(coord, step)
  }
  grid[coord[1], coord[2]] = grid[coord[1], coord[2]] + 1
}

sum(grid %% 2 == 1, na.rm = TRUE)


grid = grid %% 2

az = which(grid==1, arr.ind=TRUE)


for ( i in seq(100)){
  print(i)
  copy_grid = grid
  for(row in seq(3, 798)){
    if(row%%2 == 0){
      start = 4
    } else {
      start = 3
    }
    for(col in seq(start,798,2)){
      neighbhour_blacks = 0
      for(option in OPTIONS){
        check_coord = move(c(row, col), option)
        if(grid[check_coord[1], check_coord[2]] == 1){
          neighbhour_blacks =neighbhour_blacks+1
        }
      }
      if(grid[row, col] == 0 & neighbhour_blacks == 2){
        copy_grid[row, col] = 1
      }
      if(grid[row, col] == 1 & neighbhour_blacks > 2){
        copy_grid[row, col] = 0
      }
      if(grid[row, col] == 1 & neighbhour_blacks == 0){
        copy_grid[row, col] = 0
      }
    }
  }
  grid = copy_grid
  print(sum(grid == 1, na.rm = TRUE))
}


inp  = readLines("inp/day11_inp.txt", )

grid = as.matrix(inp)
grid = do.call(rbind, strsplit(grid, ""))
grid = cbind(".", rbind(".", grid, "."), ".")

# Init new grid
grid_new = grid

while(TRUE){
  for(i in 2:(nrow(grid) - 1)) {
    for(j in 2:(ncol(grid)-1)) {
      if(grid[i,j] == "#" & sum(grid[(i-1):(i+1),(j-1):(j+1)] == "#") > 4){
        grid_new[i,j] = "L" 
      }
      if(grid[i,j] == "L" & sum(grid[(i-1):(i+1),(j-1):(j+1)] == "#") == 0){
        grid_new[i,j] = "#" 
      }      
    }
  }
  if(sum(grid=="#") == sum(grid_new=="#")){
    cat(paste('Solution to part 1 is:', 
              sum(grid_new == "#"), 
              sep='\n'))
    cat('\n====\n')
    break()
  }
  grid = grid_new
}


### Star 2 ---------
# Kijklijnen
walk_ul <- function(grid, start_i, start_j){
  i = start_i - 1
  j = start_j - 1
  while(i > 0 & j > 0){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i - 1
    j = j - 1
  }
  return('.')
}

walk_l <- function(grid, start_i, start_j){
  i = start_i 
  j = start_j - 1
  while(j > 0){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    j = j - 1
  }
  return('.')
}

walk_dl <- function(grid, start_i, start_j){
  i = start_i + 1
  j = start_j - 1
  while(i < nrow(grid) & j > 0){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i + 1
    j = j - 1
  }
  return('.')
}

walk_d <- function(grid, start_i, start_j){
  i = start_i + 1
  j = start_j
  while(i < nrow(grid)){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i + 1
  }
  return('.')
}

walk_dr <- function(grid, start_i, start_j){
  i = start_i + 1
  j = start_j + 1
  while(i < nrow(grid) & j < ncol(grid)){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i + 1
    j = j + 1
  }
  return('.')
}

walk_r <- function(grid, start_i, start_j){
  i = start_i
  j = start_j + 1
  while(j < ncol(grid)){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    j = j + 1
  }
  return('.')
}

walk_ur <- function(grid, start_i, start_j){
  i = start_i - 1
  j = start_j + 1
  while(i > 0 & j < ncol(grid)){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i - 1
    j = j + 1
  }
  return('.')
}

walk_u <- function(grid, start_i, start_j){
  i = start_i - 1
  j = start_j
  while(i > 0){
    if(grid[i,j] != '.'){
      return(grid[i,j])
    }
    i = i - 1
  }
  return('.')
}

# Kijk alle kanten op
walk_all <- function(grid, start_i, start_j){
  all_dir = matrix('.', ncol=3, nrow=3)
  
  all_dir[1,1] = walk_ul(grid = grid, start_i = i, start_j = j)
  all_dir[2,1] = walk_l(grid = grid, start_i = i, start_j = j)
  all_dir[3,1] = walk_dl(grid = grid, start_i = i, start_j = j)
  all_dir[3,2] = walk_d(grid = grid, start_i = i, start_j = j)
  all_dir[3,3] = walk_dr(grid = grid, start_i = i, start_j = j)
  all_dir[2,3] = walk_r(grid = grid, start_i = i, start_j = j)
  all_dir[1,3] = walk_ur(grid = grid, start_i = i, start_j = j)
  all_dir[1,2] = walk_u(grid = grid, start_i = i, start_j = j)
  all_dir[2,2] = "O"
  
  return(all_dir)
}

# Re-read input
grid = as.matrix(inp)
grid = do.call(rbind, strsplit(grid, ""))
grid = cbind(".", rbind(".", grid, "."), ".")

grid_new = grid

# Apply new rules
while(TRUE){
  for(i in 2:(nrow(grid) - 1)) {
    for(j in 2:(ncol(grid)-1)) {
      vis_seats = walk_all(grid, i, j)
      if(grid[i,j] == "#" & sum(vis_seats == '#') > 4){
        grid_new[i,j] = "L" 
      }
      if(grid[i,j] == "L" & sum(vis_seats == '#') == 0){
        grid_new[i,j] = "#" 
      }      
    }
  }
  if(sum(grid=="#") == sum(grid_new=="#")){
    cat(paste('Solution to part 2 is:', 
              sum(grid_new == "#"), 
              sep='\n'))
    break()
  }
  grid = grid_new
}


DIRECTIONS = c('N', 'E', 'S', 'W')

west <- function(coords, steps){
  return(c(coords[1], coords[2]-steps))
}

south <- function(coords, steps){
  return(c(coords[1]+steps, coords[2]))
}

east <- function(coords, steps){
  return(c(coords[1], coords[2]+steps))
}

north <- function(coords, steps){
  return(c(coords[1]-steps, coords[2]))
}

right <- function(current, degrees){
  a = which(DIRECTIONS == current) + ((degrees/90) %% 4)
  if(a > 4){
    a = a - 4
  }
  return(DIRECTIONS[a])
}

left <- function(current, degrees){
  a = which(DIRECTIONS == current) - ((degrees/90) %% 4)
  if(a < 1){
    a = 4 - abs(a)
  }
  return(DIRECTIONS[a])
}

forward <- function(coords, steps, current){
  return(switch(current, 'N'=north, 'E'=east, 'S'=south, 'W'=west)(coords, steps))
}


inp  = readLines("inp/day12_inp.txt", )
inp = lapply(strsplit(inp, ''), function(x) c(x[1], paste(x[2:length(x)], collapse = '')))

ship_coords = c(0,0)
direc = 'E'

for(action in inp){
  command = action[1]
  steps = as.numeric(action[2])
  if(command %in% DIRECTIONS){
    ship_coords = switch(command, 'N'=north, 'E'=east, 'S'=south, 'W'=west)(ship_coords, steps)
  } else if (command == 'F') {
    ship_coords = forward(ship_coords, steps, direc)
  } else if (command == 'R') {
    direc = right(direc,  steps)
  } else if (command == 'L') {
    direc = left(direc,  steps)
  } 
}

cat(paste('Solution to part 1 is:', 
          sum(abs(ship_coords)), 
          sep='\n'))
cat('\n====\n')


### Star 2 ---------
ship_coords = c(0,0)
waypoint = c(-1,10)

for(action in inp) {
  command = action[1]
  steps = as.numeric(action[2])
  if(command=='F'){
    ship_coords = ship_coords + waypoint * steps
  } else if(command %in% DIRECTIONS) {
    waypoint = switch(command, 'N'=north, 'E'=east, 'S'=south, 'W'=west)(waypoint, steps)
  } else if (command == 'R') {
    for(turns in 1: (steps/90)) {
      waypoint = c(waypoint[2], -waypoint[1])
    }
  } else if (command == 'L'){
    for(turns in 1: (steps/90)) {
      waypoint = c(-waypoint[2], waypoint[1])
    }
  }
}

cat(paste('Solution to part 2 is:', 
          sum(abs(ship_coords)), 
          sep='\n'))
    
### Input ---------
inp = readLines('inp/day17_inp.txt',)

cycles = 6
dim_size = length(inp) + cycles + 8

### Star 1 ----------
store = array(0, rep(dim_size, 3))

# Fill initial state
for(inp_line in seq(length(inp))) {
  # split line
  cubes = strsplit(inp[inp_line], '')[[1]]
  for(cube in seq(length(cubes))) {
    if(cubes[cube] == '#'){
      # place in the middle
      store[inp_line + cycles, 
            cube + cycles,
            11] = 1
    }
  }
}

# Get dims and init new state
store_dims = dim(store)
new_store = store


# Iterate over each cell for a given number of cycles
for(cycle_i in seq(cycles)) {
  for(x in seq(2,dim_size-1)) {
    for(y in seq(2,dim_size-1)) {
      for(z in seq(2,dim_size-1)) {
        # Get active cube 
        cube = store[x,y,z]
        # Get surrounding cubes
        surrounding = store[(x-1):(x+1), 
                            (y-1):(y+1), 
                            (z-1):(z+1)]
        # If inactive make active if 3 active surrounding cubes
        if(cube == 0){
          if(sum(surrounding) == 3) new_store[x,y,z] = 1
        # If active keep active only if 2 or 3 surrounding cubes are
        } else {
          # +1 for correction of current (active) cube
          if(sum(surrounding) == 3 | sum(surrounding) == 4) {
            new_store[x,y,z] = 1 } else {
              new_store[x,y,z] = 0
            }
        }
      }
    }
  }
  # copy new store as active state
  store = new_store
}

cat(paste('Solution to part 1 is:', 
          sum(store), 
          sep='\n'))
cat('\n====\n')


### Star 2 ----------------

store = array(0, rep(dim_size,4))

for(inp_line in seq(length(inp))) {
  # split line
  cubes = strsplit(inp[inp_line], '')[[1]]
  for(cube in seq(length(cubes))) {
    if(cubes[cube] == '#'){
      # place in the middle
      store[inp_line + cycles, 
            cube + cycles,
            11, 
            11] = 1
    }
  }
}

new_store = store

for(cycle_i in seq(cycles)) {
  for(x in seq(2,dim_size-1)) {
    for(y in seq(2,dim_size-1)) {
      for(z in seq(2,dim_size-1)) {
        # just add 1 dimension
        for(w in seq(2, dim_size-1)) {
          cube = store[x,y,z,w]
          surrounding = store[(x-1):(x+1), 
                              (y-1):(y+1), 
                              (z-1):(z+1), 
                              (w-1):(w+1)]
          if(cube == 0){
            if(sum(surrounding) == 3) new_store[x,y,z,w] = 1
          } else {
            if(sum(surrounding) == 3 | sum(surrounding) == 4) {
              new_store[x,y,z,w] = 1 } else {
                new_store[x,y,z,w] = 0
              }
          }
        }
      }
    }
  }
  store = new_store
}

cat(paste('Solution to part 2 is:', 
          sum(store), 
          sep='\n'))

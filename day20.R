library(readr)


inp = readLines('inp/day20_inp.txt')
inp = c('',inp)

all_tiles = split(inp,cumsum(inp==''))
all_tiles = lapply(all_tiles, function(x) x[2:length(x)])

tiles = list()

for(tile in all_tiles){
  num_tile = parse_number(tile[1])
  tile_par = tile[2:length(tile)]
  tile_par = do.call(rbind,strsplit(tile_par,''))
  tile_par[tile_par=='.'] = 0
  tile_par[tile_par=='#'] = 1
  tile_fin = as.matrix(as.data.frame(lapply(as.data.frame(tile_par), as.numeric)), rownames.force = TRUE)
  colnames(tile_fin) <- NULL
  row.names(tile_fin) <- NULL
  
  tiles[[length(tiles)+1]] = tile_fin
  names(tiles)[length(tiles)] = as.character(num_tile)
}


retrieve_side = function(x_mat, side){
  if(side == 'N'){
    return(x_mat[1,])
  } else if(side == 'O'){
    return(x_mat[,10])
  } else if(side == 'Z'){
    return(x_mat[10,])
  } else if(side == 'W'){
    return(x_mat[,1])
  }
}

rotate <- function(x) t(apply(x, 2, rev))


matzes = list()


for(target in seq(length(tiles))){
  active_target = tiles[target]
  target_no = names(active_target)
  print(target_no)
  target_tile = active_target[[1]]
  matzes[[target_no]] = c(NA)
  for(flip_target in seq(2)){
    target_tile = t(target_tile)
    for (rotate_target in seq(4)){
      target_tile = rotate(target_tile)
      target_side = retrieve_side(target_tile, 'Z')
      for(potential in seq(length(tiles))){
        if(potential != target){
          active_potential = tiles[potential]
          active_no = names(active_potential)
          active_tile = active_potential[[1]]
          for(flip_potential in seq(2)){
            active_tile = t(active_tile)
            for (rotate_potential in seq(4)){
              active_tile = rotate(active_tile)
              potential_side = retrieve_side(active_tile, 'N')
              if(all.equal(potential_side, target_side) == TRUE){
                matzes[[target_no]] = c(matzes[[target_no]], active_no)
              }
            }
          } 
        }
      }
    }
  }
}


format(prod(as.numeric(names(which(lapply(matzes, function(x) length(unique(x))) == 3)))), scientific = FALSE)




"107399567124539"

all_matches = matzes
all_matches = lapply(all_matches, function(x) unique(x[!is.na(x)]))


# Sides
sides = all_matches[unlist(lapply(all_matches, function(x) length(x) == 3))]

# Corners
corners = all_matches[unlist(lapply(all_matches, function(x) length(x) == 2))]

# Centers
centers = all_matches[unlist(lapply(all_matches, function(x) length(x) == 4))]


full_pattern = matrix(NA, nrow=12*10, ncol=12*10)
fill_pattern = matrix(0, nrow=12,12)

full_pattern[1:10, 111:120] = rotate(rotate(rotate(tiles[['3061']])))
fill_pattern[1,12] = 3061

full_pattern[1:10, 101:110] = rotate(rotate(t(tiles[['2897']])))
fill_pattern[1,11] = 2897

full_pattern[11:20, 111:120] = rotate(tiles[['1783']])
fill_pattern[2,12] = 1783

fill_pattern[1,10] = -1
fill_pattern[2,11] = -2
fill_pattern[3,12] = -1

corners = corners[names(corners) != '3061']
sides = lapply(sides, function(x) x[x!='3061'])


while(length(tiles) > 0){
  # Get highest neighbours
  coords = matrix(ncol=2)
  colnames(coords) = c('row', 'col')
  for(i in -4:-1){
    active_coords = which(fill_pattern == i, arr.ind=TRUE)
    coords = rbind(coords, active_coords)
  }
  coords = coords[2:nrow(coords),]
  
  for(coord in 1:nrow(coords)){
    active_coord = coords[coord, ]
    side_points = 0
    if (active_coord['row'] == 1 | active_coord['row'] == 12){
      side_points = side_points + 1
    }
    if (active_coord['col'] == 1 | active_coord['col'] == 12){
      side_points = side_points + 1
    }
  }
  
  # Check if center/corner/side
  
  # check if any unique solution exists
  
  # rotate untill you drop and fit solution
  
  # update puzzle and neighbours
}


 
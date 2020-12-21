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
sides = sides[names(sides) != '1783']
sides = sides[names(sides) != '2897']

check_valid_coord <- function(x, max_s){
  if(x < 1 | x > max_s){
    return(FALSE)
  }
  return(TRUE)
}

check_valid_coords <- function(coords, max_s){
  for(coord in coords){
    if(!check_valid_coord(coord, max_s)){
      return(FALSE)
    }
  }
  return(TRUE)
}

while(length(corners) > 0){
  # Get highest neighbours
  coords = matrix(ncol=2)
  colnames(coords) = c('row', 'col')
  for(i in -4:-1){
    active_coords = which(fill_pattern == i, arr.ind=TRUE)
    coords = rbind(coords, active_coords)
  }
  coords = coords[2:nrow(coords),,drop=FALSE]
  
  BREAK = FALSE
  
  for(coord in 1:nrow(coords)){
    if(BREAK){
      break()
    }
    active_coord = coords[coord, ]
    side_points = 0
    if (active_coord['row'] == 1 | active_coord['row'] == 12){
      side_points = side_points + 1
    }
    if (active_coord['col'] == 1 | active_coord['col'] == 12){
      side_points = side_points + 1
    }
    
    matching_pieces = c()
    for(side in list(c(0,-1), c(0,1), c(1,0), c(-1,0))){
      neigh_coord = c(active_coord[1] + side[1], active_coord[2] + side[2])
      if(check_valid_coords(neigh_coord,12)){
        matching_pieces = c(matching_pieces, fill_pattern[neigh_coord[1], neigh_coord[2]])
      }
      matching_pieces = matching_pieces[matching_pieces>0]
    }
    
    if(side_points == 0){
      matches = names(which(unlist(lapply(centers, function(x) sum(x %in% as.character(matching_pieces)))) == length(matching_pieces)))
    } else if (side_points == 1){
      matches = names(which(unlist(lapply(sides, function(x) sum(x %in% as.character(matching_pieces)))) == length(matching_pieces)))
    } else if (side_points == 2){
      matches = names(which(unlist(lapply(corners, function(x) sum(x %in% as.character(matching_pieces)))) == length(matching_pieces)))
    }
    
    if(length(matches) == 1){
      store_pass = list()
      match = matches[1]
      to_fit = tiles[[match]]
      x_row = active_coord[1] - (active_coord[1] - 1) + ((active_coord[1] - 1) * 10)
      y_col =  active_coord[2] - (active_coord[2] - 1) + ((active_coord[2] - 1) * 10)
      
      check_north = NA
      check_south = NA
      check_west = NA
      check_east = NA
      
      if(check_valid_coords(c(x_row-1,y_col), 120)){
        check_north = full_pattern[x_row-1,seq(from=y_col,length.out = 10)]
      }
      if(check_valid_coords(c(x_row+10,y_col), 120)){
        check_south = full_pattern[x_row+10,seq(from=y_col,length.out = 10)]
      }
      if(check_valid_coords(c(x_row,y_col-1), 120)){
        check_west = full_pattern[seq(from=x_row,length.out = 10),y_col-1]
      }
      if(check_valid_coords(c(x_row,y_col+10), 120)){
        check_east = full_pattern[seq(from=x_row,length.out = 10),y_col+10]
      }
      
      active_tile = tiles[[match]]
      for(flip_potential in seq(2)){
        active_tile = t(active_tile)
        for (rotate_potential in seq(4)){
          active_tile = rotate(active_tile)
          passing = FALSE
          if(!is.na(check_north[1])){
            if(all.equal(retrieve_side(active_tile, 'N'), check_north) == TRUE){
              passing = TRUE
            } else {
              passing = FALSE
            }
          }
          
          if(!is.na(check_south[1])){
            if(all.equal(retrieve_side(active_tile, 'S'), check_south) == TRUE){
              passing = TRUE
            } else {
              passing = FALSE
            }
          }
          
          if(!is.na(check_east[1])){
            if(all.equal(retrieve_side(active_tile, 'O'), check_east) == TRUE){
              passing = TRUE
            } else {
              passing = FALSE
            }
          }
          
          if(!is.na(check_west[1])){
            if(all.equal(retrieve_side(active_tile, 'W'), check_west) == TRUE){
              passing = TRUE
            } else {
              passing = FALSE
            }
          }
          
          if(passing){
            store_pass[[length(store_pass) + 1]] = active_tile
          }
          
        }
      }
      
      if(length(store_pass) == 1){
        full_pattern[seq(from=x_row,length.out = 10), seq(from=y_col,length.out = 10)] = store_pass[[1]]
        fill_pattern[active_coord[1], active_coord[2]] = as.numeric(match)
        for(side in list(c(0,-1), c(0,1), c(1,0), c(-1,0))){
          neigh_coord = c(active_coord[1] + side[1], active_coord[2] + side[2])
          if(check_valid_coords(neigh_coord,12)){
            if(fill_pattern[neigh_coord[1], neigh_coord[2]] <= 0) {
              fill_pattern[neigh_coord[1], neigh_coord[2]] = fill_pattern[neigh_coord[1], neigh_coord[2]] - 1
            }
          }
        }
        
        if(side_points == 0){
          centers = centers[names(centers) != match]
        } else if (side_points == 1){
          sides = sides[names(sides) != match]
        } else if (side_points == 2){
          corners = corners[names(corners) != match]
        }
        BREAKK = TRUE
        
      } else {
        print(length(store_pass))
      }
      
      
    }
    
  }
  
  # Check if center/corner/side
  
  # check if any unique solution exists
  
  # rotate untill you drop and fit solution
  
  # update puzzle and neighbours
}

NESIE = data.frame(row=c(1,2,2,2,2,2,2,2,2,3,3,3,3,3,3),
                   col=c(19,1,6,7,12,13,18,19,20,2,5,8,11,14,17))

nessie_finder <- function(grid){
  if(all(grid[cbind(NESIE$row, NESIE$col)] == 0)){
    grid[cbind(NESIE$row, NESIE$col)] = 2
  }
}

ss_full = full_pattern

all_idx = 1:120 
neg_idx = sort(c(seq(from=10,by=10,length.out=11),seq(from=11,by=10,length.out=11)))
neg_idx = c(1,neg_idx,120)


ss_cut = ss_full[all_idx[!(all_idx %in% neg_idx)], all_idx[!(all_idx %in% neg_idx)]]


acctive_ss = ss_cut

for(flip_potential in seq(2)){
  acctive_ss = t(acctive_ss)
  for (rotate_potential in seq(4)){
    acctive_ss = rotate(acctive_ss)
    for(i_r in 0:(96-3)){
      for(j_c in 0:(96-20)){
        NESIE_tmp = NESIE
        NESIE_tmp$row = NESIE_tmp$row + i_r
        NESIE_tmp$col = NESIE_tmp$col + j_c
        if(all(acctive_ss[cbind(NESIE_tmp$row, NESIE_tmp$col)] == 1)){
          acctive_ss[cbind(NESIE_tmp$row, NESIE_tmp$col)] = 2
          print(flip_potential)
          print('-')
          print(rotate_potential)
        }
      }
    }
    
  }
}





inp  = readLines("inp/day3_inp.txt", )

grid = as.matrix(inp)
grid = do.call(rbind, strsplit(grid, ""))

counter = 0

NCOL = ncol(grid)


SLOPES = list(c(1,1), c(1,3), c(1,5), c(1,7), c(2,1))

# SLOPES = list(c(1,3))


for(slope in SLOPES){
  counter = 0 
  down = seq(from=1, to=nrow(grid), by=slope[1])
  right = seq(from=1, by=slope[2], length.out = length(down))
  for(i in 1:length(down)){
    r = right[i] %% NCOL
    if(r == 0) {
      r = NCOL
    }
    if(grid[down[i], r] == "#"){
      counter = counter + 1
    }
  }
  print(counter)
}


# for(i in 1:nrow(grid)){
#   if(i %% 2 == 1){
#     y = ((i - 1) * 1 + 1) %% NCOL
#     if(y == 0){
#       y = NCOL
#     }
#     if(grid[i, y] == "#"){
#       counter = counter + 1
#     }
#   }
# }



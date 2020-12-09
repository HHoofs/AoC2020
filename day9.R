inp  = as.numeric(readLines("inp/day9_inp.txt", ))

### Star 1 ----------
look_back = 25
i = look_back

while(i < length(inp)){
  active_seq = (i-look_back+1): i
  potentials = combn(inp[active_seq], 2, sum)
  if( !(inp[i+1] %in% potentials)) {
    store = inp[i+1]
    return()
  }
  i = i + 1
}

cat(paste('Solution to part 1 is:', store, sep='\n'))
cat('\n====\n')

### Star 2 ----------
look_back = 1

while (TRUE) {
  look_back = look_back + 1
  i = look_back
  while(i <= length(inp)){
    active_seq = (i-look_back+1): i
    if(sum(inp[active_seq]) == store){
      return()
    }
    i = i + 1
  }
}

cat(paste('Solution to part 2 is:', 
          sum(range(inp[active_seq])), sep='\n'))

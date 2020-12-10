### Parse Input ----------
inp  = as.numeric(readLines("inp/day10_inp.txt", ))

inp_sorted = sort(inp)

### Star 2 ----------
inp_sorted_prepend = c(0, inp_sorted)
inp_sorted_apppend = c(inp_sorted, NA)
# Differences between offset
diff = inp_sorted_apppend - inp_sorted_prepend
# Add difference of 3 to last plug
diff[length(diff)] = 3

# Compute difference of 1 and 3 (and multiply)
cat(paste('Solution to part 1 is:', 
          length(diff[diff == 1]) * (length(diff[diff == 3])), 
          sep='\n'))
cat('\n====\n')

### Star 2 ----------
chunks = c(0)

for (i in diff){
  if(i == 3 ){
    chunks = c(chunks, 0)
  } else {
    chunks[length(chunks)] = chunks[length(chunks)] + 1
  }
}

chunks = chunks[chunks!=0]

cat(paste('Solution to part 2 is:', 
          format(prod(chunks*(chunks-1)/2+1), scientific = FALSE), 
          sep='\n'))

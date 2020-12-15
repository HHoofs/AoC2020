require(Matrix)

inp = c(1,20,11,6,12,0)

a = sparseVector(1:length(inp), inp + 1, 30000000 - 1)

cur = 0

for(i in (length(inp)+1):(30000000-1)){
  if(i%%30000 == 0){
    print(i/30000000 * 100)
  }
  last_seen = as.numeric(a[cur+1])
  a[cur+1] = i
  if(last_seen ){
    cur = i - last_seen
  } else {
    cur = 0
  }
}

print(cur)




lint = rev(lint)

for(i in 1:length(lint)){
  if(lint[i] == 0 ){
    # cat('\n')
    cat(lint[i-1])
    cat('\n')
  } else {
    # cat(lint[i])
    # cat(' ')
  }
}

inp = readLines("inp/day13_inp.txt", )
start_time = as.numeric(inp[1])
inp = c('', "19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,383,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,457,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17")
busses = as.numeric(strsplit(inp[2], ',')[[1]])

require(numbers)

valid_times = data.frame(time=1:200000)
index = c()
i = 0

for (bus in busses){
  if(!is.na(bus)){
    valid_times[as.character(bus)] = 0
    valid_times[seq(from=0, by=bus, to=200000), as.character(bus)] = 1
    index = c(index, i)
  }
  i = i + 1
}

valid_times = valid_times[,2:ncol(valid_times)]
valid_busses = as.numeric(names(valid_times))


departure = min(unlist(lapply(valid_times, function(x) x$times[x$times >= start_time][1]))) 
first_bus = as.numeric(names(which(unlist(lapply(valid_times, function(x) x$times[x$times >= start_time][1])) == departure)))

print((departure-start_time) * first_bus)

df = data.frame(bus=valid_busses[1:length(valid_busses)], index=index,inter=NA, slop=NA)

for(bus in 2:length(valid_busses)){
  dd = c()
  offset = index[bus]
  i = offset + 1
  print(bus)
  while(TRUE){
    if(valid_times[i - offset, 1] + valid_times[i,bus] == 2) {
      print(i )
      dd = c(dd,i -offset )
    }
    i = i + 1
    if(length(dd) == 2){
      df[bus,c('inter', 'slop')] = c(dd[1], dd[2]-dd[1])
      break()
    }
  }
}



format(chinese(c(0,df$inter[2:nrow(df)]), df$bus), scientific = FALSE)

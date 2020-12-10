inp  = readLines("inp/day6_inp.txt", )

require(stringr)

inp

# init passports list
quest = list("")

# iterate over all input lines
for (inp_line in inp) {
  # If empty line start new (empty) passport entry
  if (inp_line == "") {
    quest[[length(quest) + 1]] = ""
    # If not empty concat to current (active) passport
  } else {
    quest[[length(quest)]] = paste(quest[[length(quest)]], inp_line, sep=" ")
  }
}

quest = lapply(quest, function(x) str_trim(x))


sum(unlist(lapply(quest, function(x) length(unique(strsplit(x, " ")[[1]])))))

a = lapply(quest, function(x) strsplit(x, " ")[[1]])


d =c()
for(aa in a){
  d = c(d, length(Reduce(intersect, strsplit(aa, ""))))
}    

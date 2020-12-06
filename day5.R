inp  = readLines("inp/day5_inp.txt", )

binary_txt <- function(inp, I){
  inp_splitted = strsplit(inp, "")[[1]]
  inp_splitted_rev = rev(inp_splitted)
  val = 0
  for(base in 1:length(inp_splitted_rev)) {
    if(inp_splitted_rev[base] == I) {
      val = val + (2 ^(base - 1))
    }
  }
  return(val)
}

boarding_passes = inp

all = c()

for(boarding_pass in boarding_passes){
  chars = strsplit(boarding_pass,"")[[1]]
  row_num = binary_txt(paste(chars[1:7], collapse = ""), I="B")
  chair_num = binary_txt(paste(chars[8:10], collapse = ""), I="R")
  id = row_num * 8 + chair_num
  all = c(all, id)
}

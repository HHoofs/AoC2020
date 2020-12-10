require(stringr)

inp  = readLines("inp/day2_inp.txt")

counter = 0

for (inp_line in inp) {
 inp_line = strsplit(inp_line, "-|:| ")[[1]]
 count = 0
 for(let in strsplit(inp_line[5], "")[[1]]){
   if(let == inp_line[3]){
     count = count + 1
   }
 }
 if(count >= as.numeric(inp_line[1]) & count <= as.numeric(inp_line[2])){
   counter = counter + 1
 } 
}

#### First star -----------------
print(counter)


for (inp_line in inp) {
  inp_line = strsplit(inp_line, "-|:| ")[[1]]
  count = 0
  password= strsplit(inp_line[5], "")[[1]]
  corr = (password[as.numeric(inp_line[1])] == inp_line[3]) + (password[as.numeric(inp_line[2])] == inp_line[3])
  if(corr == 1){
    counter = counter + 1
  }
}

#### Second star ----------------
print(counter)
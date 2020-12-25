1 * 7 

7 %% 20201227

7 * 7  %% 20201227

(7 * 7 * 7 * 7 * 7 * 7 * 7 * 7)  %% 20201227


subject_transform <- function(loops, sn=7, remainder=20201227){
  value = 1
  for(loop in seq(loops)){
    value = value * sn
    value = value %% remainder
  }
  return(value)
}

subject_transform(11)


retrieved = NULL

loops = 1
value = 1
remainder=20201227

while(length(retrieved) < 2){
  value = value * 7
  value = value %% remainder
  if(value == 9232416){
    retrieved = c(retrieved, loops)
  }
  if(value == 14144084){
    retrieved = c(retrieved, loops)
  }
  loops = loops + 1
  print(value)
}

subject_transform(8) == 9232416

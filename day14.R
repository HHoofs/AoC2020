numeric2binary <- function(numeric_value){
  if(numeric_value == 0){
    return(0)
  }
  res <- c()
  quotient = numeric_value
  while (quotient > 0) {
    res <- c(quotient %% 2, res)
    quotient = quotient %/% 2
  }
  return(res)
}

binary2numeric <- function(binary_value){
  res = 0
  pow = 0
  for(i in length(binary_value):1){
    if(binary_value[i] == 1){
      res = res + 2^pow
    }
    pow = pow + 1
  }
  return(res)
}

mask2binary <- function(mask){
  mask_split = suppressWarnings(as.numeric(strsplit(mask, "")[[1]]))
  return(mask_split) 
}

mask_binary <- function(mask_bin, numeric_bin){
  size = max(length(mask_bin), length(numeric_bin))
  result_matrix = matrix(0, ncol = size, nrow = 2)
  result_matrix[1, (size-length(mask_bin) + 1):size] = mask_bin
  result_matrix[2, (size-length(numeric_bin) + 1):size] = numeric_bin
  
  result = result_matrix[1,]
  result[is.na(mask_bin)] = result_matrix[2, is.na(result_matrix[1,])]
  return(result)
}

mask_binary_v2 <- function(mask_bin, numeric_bin){
  size = max(length(mask_bin), length(numeric_bin))
  result_matrix = matrix(0, ncol = size, nrow = 2)
  result_matrix[1, (size-length(mask_bin) + 1):size] = mask_bin
  result_matrix[2, (size-length(numeric_bin) + 1):size] = numeric_bin
  
  result = result_matrix[2,]
  result[result_matrix[1,] == 1 & !is.na(result_matrix[1,])] = 1
  result[is.na(result_matrix[1,])] = NA
  
  indices = which(is.na(result))
  if(length(indices) == 0){
    return(binary2numeric(result))
  }
  
  perm = list()
  for(i in indices){
    perm[[length(perm) + 1]] = 0:1
  }
  combinations = expand.grid(perm)
  
  num_results = c()
  
  for(combination in 1:nrow(combinations)){
    new_bin = result
    new_bin[indices] = combinations[combination,]
    num_results = c(num_results, binary2numeric(new_bin))
  }
  
  return(num_results)
}




inp  = readLines("inp/day14_inp.txt", )

### Star 1 ----
memory = list()

for(instruction in inp){
  parsed_instruction = strsplit(instruction, " = ")[[1]]
  if(parsed_instruction[1] == 'mask'){
    mask = parsed_instruction[2]
  } else {
    slot = as.numeric(regmatches(parsed_instruction[[1]], gregexpr("[[:digit:]]+", parsed_instruction[1]))[[1]])
    numeric_value = as.numeric(parsed_instruction[2])
    final_numeric_value = binary2numeric(mask_binary(mask2binary(mask), numeric2binary(numeric_value)))
    memory[as.character(slot)] = final_numeric_value
  }
}

cat(paste('Solution to part 1 is:', 
          sum(unlist(memory)), 
          sep='\n'))
cat('\n====\n')


### Star 2 ----------------------------------------------------------
pb = txtProgressBar(min = 0, max = length(inp), initial = 0, style=3) 
i = 1

memory = list()

for(instruction in inp){
  parsed_instruction = strsplit(instruction, " = ")[[1]]
  if(parsed_instruction[1] == 'mask'){
    # If mask retrieve it and set it as current
    mask = parsed_instruction[2]
  } else {
    # Retrieve slot (numeric)
    slot = as.numeric(regmatches(parsed_instruction[[1]], gregexpr("[[:digit:]]+", parsed_instruction[1]))[[1]])
    # Convert slot (numeric) to binary
    slot_bin = numeric2binary(slot)
    # Get all slots using the mask
    slots = mask_binary_v2(mask_bin = mask2binary(mask), numeric_bin = slot_bin)
    # Assign value to all slots
    for (slot in slots){
      memory[as.character(slot)] = as.numeric(parsed_instruction[2])
    }
  }
  
  # Progress bar
  setTxtProgressBar(pb,i)
  i = i + 1
}

cat(paste('Solution to part 2 is:', 
          sum(unlist(memory)), 
          sep='\n'))


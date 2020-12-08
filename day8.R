# sum_list <- function(list1, list2){
#   Map("+", list1[intersect(names(list1),names(list2))], list1[intersect(names(x),names(y))])
# }


switch_opp <- function(operations, index, scheme = list(jmp='nop', nop='jmp', acc='acc')){
  operations[[index]]$opp = scheme[[operations[[index]]$opp]]
  return(operations)
}


#' acc (accumulator)
#'
#' @description 
#' acc increases or decreases a single global value called the accumulator by the value given in the argument. 
#' For example, acc +7 would increase the accumulator by 7. 
#' The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next.
#' 
#' @param arg[integer]. Argument of current instruction
#' @param state [list]. Current state (see return).
#' @return The new active state  containing:
#'  - ind [integer]. active index
#'  - val [integer]. active value
#'  
#' @examples 
#' state = list(ind=1, val=0)
#' acc(2, state)
#'  
#' @details the argument is added to the current value, index is increased by one
acc <- function(arg, state){
  return(list(ind=state$ind+1, val=state$val+arg))
}


#' jmp (jumps)
#'
#' @description 
#' jmp jumps to a new instruction relative to itself. The next instruction to execute is found 
#' using the argument as an offset from the jmp instruction; 
#' for example, jmp +2 would skip the next instruction, 
#' jmp +1 would continue to the instruction immediately below it, 
#' and jmp -20 would cause the instruction 20 lines above to be executed next.
#' 
#' @param arg[integer]. Argument of current instruction
#' @param state [list]. Current state (see return).
#' @return The new active state  containing:
#'  - ind [integer]. active index
#'  - val [integer]. active value
#'  
#' @examples 
#' state = list(ind=1, val=0)
#' jmp(2, state)
#'  
#' @details the argument is added to the current index, the value remains unchanged
jmp <- function(arg, state){
  return(list(ind=state$ind+arg, val=state$val))
}


#' nop (No OPeration)
#'
#' @description 
#' nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.
#' 
#' @param arg[integer]. Argument of current instruction
#' @param state [list]. Current state (see return).
#' @return The new active state  containing:
#'  - ind [integer]. active index
#'  - val [integer]. active value
#'  
#' @examples 
#' state = list(ind=1, val=0)
#' nop(2, state)
#'  
#' @details the index is increased by one, the value remains unchanged
#' 
nop <- function(arg, state){
  return(list(ind=state$ind+1, val=state$val))
}


#' computer
#'
#' @description 
#' Loops trough the computer-instruction untill termination.
#' Termination is achieved if:
#'  - repetation of and index (unsuccesfull)
#'  - index outside instrucion-set (succesfull)
#' 
#' @param operations_l [list]. List containing:
#'  - opp (operation) [character]. either: `acc`, `jmp`, or `nop`.
#'  - arg (arguments) [integer]
#' @param start_index [integer]. Integer that is used as starting index
#' @param start_value [integer]. Integer that is used as starting value
#' 
#' @return List with:
#'  - last active state [List(integer, integer)]
#'  - all indices used [vector(integer)]
#'  - termination status [boolean]
#'  
computer <- function(operations_l, start_index=1, start_value=0){
  indices = start_index
  state = list(ind=indices[1], val=start_value)
  while(TRUE){
    # Select active operation and provide active argument and the current state
    state = switch(operations_l[[state$ind]]$opp, 
                   'acc'=acc, 
                   'jmp'=jmp, 
                   'nop'=nop)(operations_l[[state$ind]]$arg, state)
    # Check if current index is already used (Termination unsuccesfull)
    if (state$ind %in% indices) {
      return(list(state=state, indices=indices, ter=FALSE))
    }
    # Check if current index is outside code (Termination succesfull)
    if (state$ind > length(operations_l)){
      return(list(state=state, indices=indices, ter=TRUE))
    }
    indices = c(indices, state$ind)
  }
}


#### Read and parsed input -----------
inp  = readLines("inp/day8_inp.txt", )
operations = lapply(strsplit(inp, " "), function(x) list(opp=x[1], arg=as.numeric(x[2])))


#### Part 1 -------------------------
print(computer(operations)$state$val)


#### Part 2 --------------------
for(i in 1:length(operations)) {
  # Switch operation
  operations_tmp = switch_opp(operations, i)

  ### Parse computer
  result = computer(operations_tmp)
  # If succesfully terminated print value
  if(result$ter){
    print(result$state$val)
  }
}

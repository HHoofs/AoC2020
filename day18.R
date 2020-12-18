require(stringr)

inp = readLines('inp/day18_inp.txt')


#### Part 1 ----------
order_rule <- function(exc){
  exc = str_remove(exc, '\\(')
  exc = str_remove(exc, '\\)')
  exc = str_trim(exc)
  i = 1
  val = NA
  vals = strsplit(exc, ' ')[[1]]
  for (x in seq(from=1,to=length(vals)-2, by =2)){
    if (is.na(val)){
      val = eval(parse(text=paste0(vals[1:3], collapse = '')))
    } else {
      val = eval(parse(text=paste0(val, vals[(x+1)], vals[(x+2)], collapse = '')))
    }
  }
  return(paste(' ', as.character(val), ' '))
}

all_answers = c()

for(exc in inp){
  while(str_detect(exc, '\\(')){
    exc = str_replace_all(exc, "\\([0123456789 \\+\\*]*\\)", function(reg_match) order_rule(reg_match))
    # Remove space before brackets and remove doubles spaces
    exc = str_replace_all(exc, '\\( +', '\\(')
    exc = str_replace_all(exc, ' +', ' ')
  }
  all_answers = c(all_answers, as.numeric(order_rule(exc)))
}

cat('\n')
cat(paste('Solution to part 1 is:', 
          format(sum(all_answers), scientific = FALSE), 
          sep='\n'))
cat('\n====\n')


### Part 2 ------------
order_rule2 <- function(exc){
  exc = str_remove(exc, '\\(')
  exc = str_remove(exc, '\\)')
  exc = str_trim(exc)
  if(!is.na(as.numeric(exc))){
    return(exc)
  } else {
    # split on multiplier and take product of sums between multipliers
    exc = str_split(exc, ' \\* ')[[1]]
    return(as.character(prod(sapply(exc, function(x) eval(parse(text=x))))))
  }
}

all_answers = c()

for(exc in inp){
  while(str_detect(exc, '\\(')){
    exc = str_replace_all(exc, "\\([0123456789 \\+\\*]*\\)", function(reg_match) order_rule2(reg_match))
    exc = str_replace_all(exc, '\\( +', '\\(')
    exc = str_replace_all(exc, ' +', ' ')
  }
  all_answers = c(all_answers, as.numeric(order_rule2(exc)))
}


cat(paste('Solution to part 2 is:', 
          format(sum(all_answers), scientific = FALSE), 
          sep='\n'))
### Input --------------------------
inp = readLines('inp/day16_inp.txt')

# Determine where the new entries start
cuts = which(inp == '')

# Retrieve entries
rules = inp[1:(cuts[1]-1)]
ticket = inp[(cuts[1]+2):(cuts[2]-1)]
ticket = as.numeric(strsplit(ticket, ",")[[1]])
nearby_tickets = inp[(cuts[2]+2):(length(inp))]
nearby_tickets = lapply(nearby_tickets, function(x) as.numeric(strsplit(x, ",")[[1]]))

# Parse rules
all_rules = list()
for (rule in rules) {
  parsed_rule = strsplit(rule, ": ")[[1]]
  rule_range = c()
  for(min_max in strsplit(parsed_rule[[2]], " or ")[[1]]) {
    min_max_parsed = as.numeric(strsplit(min_max,'-')[[1]])
    rule_range = c(rule_range, (min_max_parsed[1]:min_max_parsed[2]))
  }
  all_rules[[parsed_rule[1]]] = rule_range
}

### Star 1 ------------
cat(paste('Solution to part 1 is:', 
          sum(unlist(lapply(nearby_tickets, function(x) x[!x %in% unlist(all_rules)]))), 
          sep='\n'))
cat('\n====\n')

### Start 2 -----------
# Filter out invalid tickets
nearby_tickets = nearby_tickets[which(unlist(lapply(nearby_tickets, function(x) length(x[!x %in% unlist(all_rules)]))) == 0)]


# Retrieve all options for each information point in a ticket
ticket_option = list()
for(nearby_ticket in nearby_tickets){
  ticket_option[[length(ticket_option) + 1]] = 
    matrix(unlist(lapply(nearby_ticket, function(i) lapply(all_rules, function(x) any(x == i)))), ncol=length(all_rules), byrow = TRUE)
}
ticket_option = lapply(ticket_option, function(x) apply(x, 1, function(y) which(y)))

valid_options = list()
for(i in 1:length(all_rules)){
  valid_options[[length(valid_options) + 1]] = Reduce(intersect,lapply(ticket_option, function(x) x[[i]]))
}

valid_option = valid_options

while(TRUE){
  singles = unlist(lapply(valid_option, function(x) length(x) == 1))
  single_values = unlist(valid_option[singles])
  valid_option = lapply(valid_option, function(x) {
    if(length(x) == 1){
      x
    } else {
      x[!(x %in% single_values)]
    }
    
  })
  if(all(unlist(lapply(valid_option, function(x) length(x) == 1)))){
    break()
  }
}

cat(paste('Solution to part 2 is:', 
          prod(ticket[which(unlist(valid_option) %in% c(1,2,3,4,5,6))]), 
          sep='\n'))




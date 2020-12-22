### Read input ---------------------
require(digest)
inp = readLines('inp/day22_inp.txt')
inp = c('', inp)

players = split(inp,cumsum(inp==''))

player_1 = as.numeric(players[[1]])
player_1 = player_1[!is.na(player_1)]

player_2 = as.numeric(players[[2]])
player_2 = player_2[!is.na(player_2)]

### Game of crabs ----------------
crabs = function(p1_cards, p2_cards, SUBGAME=FALSE){
  digests = list(c('', ''))
  
  # RECURSION CHECK
  while(length(p1_cards) > 0 & length(p2_cards) > 0){
    
    new_p1_digest = digest(paste(p1_cards,collapse='-'))
    new_p2_digest = digest(paste(p2_cards,collapse='-'))
    
    if(any(unlist(lapply(digests, function (x) new_p1_digest == x[1] | new_p2_digest == x[2])))) {
      return(list(win=1, cards=p1_cards,rec=TRUE))
    } else {
      digests[[length(digests) + 1]] = c(new_p1_digest, new_p2_digest)
    }
    
    p1 = p1_cards[1]
    p1_cards = p1_cards[-1]
    p2 = p2_cards[1]
    p2_cards = p2_cards[-1]
    
    # SUBGAME?
    if(length(p1_cards) == 0 | length(p2_cards) == 0){
      which_wins = which.max(c(p1,p2))
    } else  if(length(p1_cards) >= p1 & length(p2_cards) >= p2 & SUBGAME){
      which_wins = crabs(p1_cards[1:p1], p2_cards[1:p2], SUBGAME)$win
    } else {
      which_wins = which.max(c(p1,p2))
    }
    
    if(which_wins == 1){
      p1_cards = c(p1_cards, p1, p2)
    } else {
      p2_cards = c(p2_cards, p2, p1)
    }
  }
  if(length(p1_cards) > 0){
    return(list(win=1,cards=p1_cards))
  } else {
    return(list(win=2,cards=p2_cards))
  }
}


### Star 1 --------------
winner = crabs(player_1, player_2)

cat(paste('Solution to part 1 is:',
          sum(winner$cards * seq(from=length(winner$cards), to=1)),
          sep='\n'))

cat('\n====\n')

### Star 2----------------
winner = crabs(player_1, player_2, TRUE)

cat(paste('Solution to part 2 is:', 
          sum(winner$cards * seq(from=length(winner$cards), to=1)), 
          sep='\n'))

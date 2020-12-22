### Read input ---------------------
inp = readLines('inp/day22_inp.txt')
inp = c('', inp)

players = split(inp,cumsum(inp==''))

player_1 = as.numeric(players[[1]])
player_1 = player_1[!is.na(player_1)]

player_2 = as.numeric(players[[2]])
player_2 = player_2[!is.na(player_2)]

### Star 1 --------------
part1_player_1 = player_1
part1_player_2 = player_2

while(length(part1_player_1) > 0 & length(part1_player_2) > 0){
  p1 = part1_player_1[1]
  part1_player_1 = part1_player_1[-1]
  p2 = part1_player_2[1]
  part1_player_2 = part1_player_2[-1]
  which_wins = which.max(c(p1,p2))
  if(which_wins == 1){
    part1_player_1 = c(part1_player_1, p1, p2)
  } else {
    part1_player_2 = c(part1_player_2, p2, p1)
  }
  part1_player_1
  part1_player_2
}

cat(paste('Solution to part 1 is:', 
          sum(c(part1_player_1, part1_player_2) * seq(from=(length(c(part1_player_1, part1_player_2))), to=1)), 
          sep='\n'))
cat('\n====\n')

### Star 2----------------
crabs = function(p1_cards, p2_cards){
  rounds_p1_digest = NULL
  rounds_p2_digest = NULL
  
  # RECURSION CHECK
  while(length(p1_cards) > 0 & length(p2_cards) > 0){

    new_p1_digest = digest(paste(p1_cards,collapse='-'))
    if(new_p1_digest %in% rounds_p1_digest){
      return(list(win=1, cards=p1_cards,rec=TRUE))
    } else {
      rounds_p1_digest = c(rounds_p1_digest, new_p1_digest)
    }
    
    new_p2_digest = digest(paste(p2_cards,collapse='-'))
    if(new_p2_digest %in% rounds_p2_digest){
      return(list(win=1, cards=p1_cards,rec=TRUE))
    } else {
      rounds_p2_digest = c(rounds_p2_digest, new_p2_digest)
    }
    
    p1 = p1_cards[1]
    p1_cards = p1_cards[-1]
    p2 = p2_cards[1]
    p2_cards = p2_cards[-1]
    # SUBGAME?
    if(length(p1_cards) == 0 | length(p2_cards) == 0){
      which_wins = which.max(c(p1,p2))
    } else  if(length(p1_cards) >= p1 & length(p2_cards) >= p2){
      sub_game = crabs(p1_cards[1:p1], p2_cards[1:p2])
      if(sub_game$rec){
        which_wins = sub_game$win
      } else {
        which_wins = sub_game$win
      }
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
    return(list(win=1,cards=p1_cards, rec=FALSE))
  } else {
    return(list(win=2,cards=p2_cards, rec=FALSE))
  }
}

winner = crabs(player_1, player_2)

cat(paste('Solution to part 2 is:', 
          sum(winner$cards * seq(from=length(winner$cards), to=1)), 
          sep='\n'))

ALL_CARDS = 1:9

cards = c(3, 8, 9, 1, 2, 5, 4, 6, 7)
cards = c(6,5,3,4,2,7,9,1,8)
cards = c(cards, 10:20)
cards

cups <- function(cards, rounds=10){
  # uniqq = list()
  for(round in seq(rounds)){
    print(round)
    current = cards[1]
    next_three = cards[2:4]
    new_set = cards[5:length(cards)]
    i = current - 1
    while (TRUE) {
      if(i %in% new_set){
        placing = which(i == new_set)
        break()
      } else {
        if(i==0){
          i = 9
        } else {
          i = i -1
        }
      }
    }
    cards = c(append(new_set, next_three, placing), current) 
    
    # new_d = cards[c(which(cards == 1)+1, which(cards == 1)+2)]
    # uniqq[[length(uniqq) + 1]] = new_d
  }
  return(list(a=cards, b=uniqq))
}


res = cups(cards, 10000000)

final = res$a

while(final[1] != 1){
  final = c(final[2:length(final)], final[1])
}

print(final[1])
print(final[2])
print(final[3])

print(res$b)

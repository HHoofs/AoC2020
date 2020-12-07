#### Read Input ----------------------
inp  = readLines("inp/day7_inp.txt", )
require(stringr)


rules = list()

for (inp_line in inp) {
  # Split key and values
  bag_key = str_split(inp_line,' contain ')[[1]]
  # Reformat key
  active_key = str_replace(str_trim(str_remove(bag_key[1], 'bags')), " ", "_")
  # init values
  rules[[active_key]] = list()
  # Check if bags are present
  if(bag_key[2] != 'no other bags.'){
    # Split values
    passive_key = str_split(bag_key[2], ', ')[[1]]
    # Assign each value and the number of bags
    for (passive_bag in passive_key) {
      amount_bags = str_split(passive_bag, pattern = " ", n=2)[[1]]
      colours_bag = str_replace(str_trim(str_remove(amount_bags[2], 'bag.*')), " ", "_")
      rules[[active_key]][[colours_bag]] = as.numeric(amount_bags[[1]])
    }
  }
}

#### Star 1 --------------------------------
df = data.frame(color='shiny_gold', check=0)

while(any(df$check == 0)){
  # Get uncheck row/color
  active_row = which(df$check==0)[1]
  active_color = df[active_row, 'color']
  # check (all) parents colors
  new_colors = names(which(unlist(lapply(rules, function(x) any(names(x) == active_color)))))
  # assign parent colors (unchecked)
  if (length(new_colors) > 0) {
    df = rbind(df,data.frame(color=new_colors, check=0))
    
  }
  df[active_row, 'check'] = 1
}

# unique parent bag colors
print(length(unique(df$color)) - 1)

#### Star 2 --------------------------------
df = data.frame(color='shiny_gold', check=0, bags=1)

while(any(df$check == 0)){
  active_row = which(df$check==0)[1]
  active_color = df[active_row, 'color']
  # bags present in current row
  active_bags = df[active_row, 'bags']
  # check children bags
  new_colors =   rules[[active_color]]
  if (length(new_colors) > 0) {
    # if any children bags assign (including number of bags)
    df = rbind(df,data.frame(color=names(new_colors), check=0, bags=unlist(new_colors) * active_bags))
    
  }
  df[active_row, 'check'] = 1
}

print(sum(df$bags) - 1)

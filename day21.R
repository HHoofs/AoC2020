inp = readLines('inp/day21_inp.txt')
require(stringr)
require(igraph)

inp_split = strsplit(inp, ' \\(contains')



df = data.frame(ing=NA, aler=NA, meal=NA)

i = 0

for(meal in inp_split){
  i = i + 1
  ingredients = strsplit(meal[1], ' ')[[1]]
  alergies = strsplit(str_sub(meal[2], 2, -2), ', ')[[1]]
  df_tmp = expand.grid(ingredients, alergies, i)
  names(df_tmp) = c('ing', 'aler', 'meal')
  df =  rbind(df, df_tmp)
}

df = df[2:nrow(df),]
df = unique(df)

g <- graph.empty(directed = T)
node.out <- unique(df$aler) #stringsAsFactor = F in data frame
node.in <- unique(df$ing) #stringsAsFactor = F in data frame
g <- add.vertices(g,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
g <- add.vertices(g,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
edge.list.vec <- as.vector(t(as.matrix(data.frame(unique(df[,1:2])))))
g <- add.edges(g,edge.list.vec)
g

maximum.bipartite.matching(g)

plot(g, edge.color="gray30",edge.width=E(g)$weight, layout=layout_as_bipartite)

MMHK(as_incidence_matrix(g))


allergens = vector(mode = "list", length = length(unique(df$aler)))
names(allergens) = unique(df$aler)


for (i in unique(df$meal)){
  df_row = df[df$meal == i,]
  for(all in unique(df_row$aler)){
    all_df = df_row[df_row$aler == all, ]
    if(is.null(allergens[[all]])){
      allergens[[all]] = all_df$ing
    } else {
      allergens[[all]] = intersect(allergens[[all]], all_df$ing)
    }
  }
}

good = unique(df$ing)[!unique(df$ing) %in% unlist(allergens)]

# ANSWER 1
nrow(unique(df[df$ing %in% good,c(1,3)]))

filt = c()

while(any(unlist(lapply(allergens, function(x) length(x) > 1)))){
  for(allies in seq(length(allergens))){
    allie = allergens[allies]
    cur_all = names(allie)
    if(length(allie[[1]]) == 1){
      filt = c(filt, allie[[1]])
    } else {
      allergens[[allies]] = allergens[[allies]][!(allergens[[allies]] %in% filt)] 
    }
  }
}

# ASNWER 2
paste(unlist(allergens[order(names(allergens))]), collapse=',')


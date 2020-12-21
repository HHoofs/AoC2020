inp = readLines('inp/day19_inp.txt')

require(plyr)

break_idx = which(inp == '')

potentials = inp[(break_idx+1):length(inp)]

inp = inp[1:c(break_idx-1)]


require(stringr)

rules = list()
length(rules) = 5

chas = c()
dic = c()


a = NA
b = NA

for (i in seq(length(inp))){
  parsed_rule = strsplit(inp[i], ': ')[[1]]
  rule_num = as.numeric(parsed_rule[1]) + 1
  rule = strsplit(parsed_rule[2], "\\| ")[[1]]
  if(rule == '\"a\"'){
    rules[[rule_num]] = data.frame(V1=i)
    chas = c(chas, rule_num)
    a = rule_num
  } else if (rule == '\"b\"'){
    rules[[rule_num]] =  data.frame(V1=i)
    chas = c(chas, rule_num)
    b = rule_num
  } else {
    df = as.data.frame(matrix(as.numeric(as.character(do.call(rbind, strsplit(rule, ' ')))) + 1, byrow = FALSE, nrow=length(rule)))
    names(df) = seq(ncol(df))
      rules[[rule_num]] = df
  }
  
  
}

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


cln_rules = rules

chas_com = chas


while(length(unique(chas_com)) < length(cln_rules)){
  for(i in seq(length(cln_rules))){
    print(i)
    if(!(i %in% chas)){
      if(all(unique(unlist(cln_rules[[i]])) %in% chas)) {
        chas_com = unique(c(chas_com, i))
      } else if (all(as.numeric(unique(unlist(cln_rules[[i]]))) %in% chas_com)) {
        df = cln_rules[[i]]
        new_df = data.frame()
        print(nrow(df))
        for(r in seq(nrow(df))){
          print(r)
          df_tmp = df[r,]
          df_tmp = df_tmp[,colSums(is.na(df_tmp)) < nrow(df_tmp)]
          new_df_tmp = cln_rules[[as.numeric(df_tmp[1])]]
          for(col_n in seq(2,ncol(df_tmp))){
            new_df_tmp = expand.grid.df(new_df_tmp, cln_rules[[as.numeric(df_tmp[col_n])]])
            names(new_df_tmp) = seq(ncol(new_df_tmp))
          }
          new_df = rbind.fill(new_df, new_df_tmp)
        }
        cln_rules[[i]] = new_df
        chas_com = unique(c(chas_com, i))
      }
    }
  }
}

ff = 0

for(potential in potentials){
  parsed_pot = strsplit(potential, '')[[1]]
  parsed_pot[parsed_pot == 'a'] = as.character(a)
  parsed_pot[parsed_pot == 'b'] = as.character(b)
  parsed_pot = as.numeric(as.character(parsed_pot))
  for(rul in cln_rules){
    df = rul
    for(nr in seq(nrow(df))){
      if(identical(as.numeric(df[nr,]), parsed_pot)){
        ff = ff + 1
      }
    }
  }
}


print(ff)

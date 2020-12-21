library("rjson")
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)

json_data <- fromJSON(paste(readLines('inp/scores.json'), collapse=""))

str(json_data)

df = data.frame(members=unlist(lapply(json_data$members, function(x) x$name)))
for(i in 1:22){
  df[,paste(i, 1, sep="-")] = as.POSIXct(as.numeric(1), origin="1970-01-01")
  df[,paste(i, 2, sep="-")] = as.POSIXct(as.numeric(1), origin="1970-01-01")
}

dayss = names(df)[4:length(names(df))]

for(dat in json_data$members){
  member = dat$name
  for(days in names(df)[2:length(names(df))]){
    d = strsplit(days,'-')[[1]]
    res = dat$completion_day_level[[d[1]]][[d[2]]]
    if(length(res) > 0){
      df[df$members == member, days] =  as.POSIXct(as.numeric(res), origin="1970-01-01")
    } else {
      df[df$members == member, days] = NA
    }
  }
}

i = 1


score <- function(x, part){
  return(part - x + 1)
}

filter_day <- function(x, day){
  xx = x
  xx[xx>day] = NA
  return(xx)
}
  

days_scores = list()
dayz = 1

total_dayz = 21

time_window = seq(from = as.POSIXct('2020-12-2 00:00:00',tz='CET'), length.out = total_dayz, by = "days")

for(time_slice in 1:length(time_window)){
  df_tmp = df
  s =
  df_tmp %>% 
    mutate_at(.vars = dayss, filter_day, day=time_window[time_slice]) %>% 
    mutate_at(.vars = dayss, dense_rank) %>% 
    mutate_at(.vars = dayss, score, part = nrow(df)) 
  df_sum = data.frame(members=df_tmp$members, score=rowSums(s[,dayss], na.rm = TRUE), day=time_window[time_slice] -  days(1))
  df_sum$rank = row_number(-df_sum$score)
  days_scores[[dayz]] = df_sum
  dayz = dayz+1
}

final_df = do.call(rbind, days_scores)

static_plot<-ggplot(final_df,aes(rank,group=members,fill=as.factor(members),color=as.factor(members))) +
  geom_tile(aes(y = score/2,
                height = score,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(members, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=score,label = paste(" ",score)), hjust=0)+
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse(labels=NULL, "") +
  scale_y_continuous("") +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() + 
  theme(plot.margin = margin(1,1,1,4, "cm"),
        plot.title=element_text(size=25, hjust=0.5, face='bold', colour='grey', vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face='italic', color='grey'),
        plot.caption =element_text(size=8, hjust=0.5, face='italic', color='grey'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

static_plot


plt<-static_plot + transition_states(states = day, transition_length = 4, state_length = 1) + 
  ease_aes('cubic-in-out') +
  labs(title = "{closest_state}", 
       subtitle = "Advent of Code ranglijst",
       caption = "DBS-NFI Editie",
       x="",y='score')


# animate(plt,100,fps = 20,duration = 40, width = 950, height = 750, renderer = ffmpeg_renderer())

final_animation<-animate(plt,(total_dayz-1) * 20 ,fps = 20,duration =  (total_dayz-1) * 4, width = 750, height = 750, 
                         renderer = ffmpeg_renderer())

anim_save('Progress.mp4',animation=final_animation)

# animate(plt, (total_dayz-1) * 20 ,fps = 20,duration =  (total_dayz-1) * 4, width = 750, height = 750, renderer = ffmpeg_renderer())
